module lexer;

import std.bigint : BigInt;
import std.algorithm : countUntil, map, join;
import std.string : format;
import std.conv : to;
import std.stdio;


struct Lexer {
	struct Token {
		struct Unknown {
			string errorMessage;
			ubyte triedToParseAsType = UNKNOWN;
		}

		struct Comment {
			enum Type {
				BLOCK,
				LINE,
				NESTING_BLOCK,
			}
			dstring value;
			Type type;
		}

		struct StringLiteral {
			enum Type {
				WYSIWYG,
				ALTERNATE_WYSIWYG,
				DOUBLE_QUOTED,
				HEX,
				DELIMITED,
				TOKEN,
			}
			dstring value;
			Type type;
		}

		struct CharacterLiteral {
			dchar value;
		}

		struct IntegerLiteral {
			BigInt value;
			bool hasLongSuffix;
			bool hasUnsignedSuffix;
		}

		struct FloatLiteral {
			enum TypeSuffix {
				NONE,
				FLOAT,
				REAL,
			}
			BigInt mantissa;
			long exponent;
			TypeSuffix typeSuffix;
			bool hasImaginarySuffix;
		}

		dstring asString;
		ulong position;

		ubyte type;
		union {
			Unknown unknown;
			Comment comment;
			StringLiteral stringLiteral;
			CharacterLiteral characterLiteral;
			IntegerLiteral integerLiteral;
			FloatLiteral floatLiteral;
		}

		enum {
			UNKNOWN = 0,
			COMMENT = 1,
			IDENTIFIER = 2,
			STRING_LITERAL = 3,
			CHARACTER_LITERAL = 4,
			INTEGER_LITERAL = 5,
			FLOAT_LITERAL = 6,
			END_OF_FILE = 7,
			__LAST_DYNAMIC_TOKEN = 7
		}
		template STATIC(string s) {
			static assert(staticTokens.countUntil(s) != -1);
			enum STATIC = __LAST_DYNAMIC_TOKEN + 1 + staticTokens.countUntil(s);
		}
	}

	dstring code;
	Token currentToken;

	this(dstring code) {
		this.code = code ~ 0 ~ 0;
		popFront();
	}

	struct LineSpecialToken {
		uint lineStartPosition;
		uint lineNumberDelta;  // difference between physical and virtual line numbers
		dstring fileName;
	}

	private ulong position;

	// to find line number for position find nearest preceding lineBeginPosition
	// and add lineNumberDelta of nearest preceding lineSpecialToken
	private ulong[] lineBeginPositions = [ 0 ];    // physical lines (#line has no effect here)
	private LineSpecialToken[] lineSpecialTokens = [{0, 0, ""}];  // sorted by lineStartPosition

	Token front() { return currentToken; }
	Token moveFront() { return currentToken; }
	bool empty() { return currentToken.type == Token.END_OF_FILE; }

	void popFront() {
		skipWhitespaceLineBreaksAndComments();
		currentToken.position = position;
		mixin(generateLexerCode);
		currentToken.asString = code[currentToken.position .. position];
	}

	int opApply(int delegate(Token) f) {
		return opApply((index, token) => f(token));
	}

	int opApply(int delegate(size_t, Token) f) {
		int i = 0;
		int result = 0;
		while (!empty()) {
			result = f(i, front());
			if (result) break;
			popFront();
			i++;
		}
		return result;
	}


	private class CodeGenerator {
		private string code = "";
		private CodeGenerator[string] cases;
		private bool previousCharWasFirstIdentifierChar = false;

		private CodeGenerator getCodeGeneratorForCase(string caseString) {
			if (caseString !in cases) cases[caseString] = new CodeGenerator;
			return cases[caseString];
		}

		private CodeGenerator getCodeGeneratorForChar(char c) {
			auto result = getCodeGeneratorForCase(format(`case'\x%02X':`, c));
			result.previousCharWasFirstIdentifierChar = isIdentifierFirstChar(c);
			return result;
		}


		CodeGenerator on(string charSequence, string code) {
			if (charSequence.length == 0) {
				this.code = code;
			} else {
				getCodeGeneratorForChar(charSequence[0]).on(charSequence[1..$], code);
			}
			return this;
		}

		CodeGenerator on(string charSequence)(string code) {
			return on(charSequence, code);
		}

		CodeGenerator onWhitespace(string code) {
			getCodeGeneratorForCase(
				`case'\u0020':` ~
				`case'\u0009':` ~
				`case'\u000B':` ~
				`case'\u000C':`
			).code = code;
			return this;
		}

		CodeGenerator onEndOfLine(string code) {
			getCodeGeneratorForCase(
				`case'\u000D':` ~
					`if (code[position] == '\u000A') position++;` ~
				`case'\u000A':` ~
				`case'\u2028':` ~
				`case'\u2029':`
			).code = code;
			return this;
		}

		CodeGenerator onEndOfFile(string code) {
			getCodeGeneratorForCase(
				`case'\u0000':` ~
				`case'\u001A':`
			).code = code;
			on!"__EOF__"(code);
			return this;
		}

		CodeGenerator onDigit(string code) {
			getCodeGeneratorForCase(`case'0':..case'9':`).code = code;
			return this;
		}

		CodeGenerator onNonZeroDigit(string code) {
			getCodeGeneratorForCase(`case'1':..case'9':`).code = code;
			return this;
		}


		CodeGenerator withKeywords(alias keywords)() {
			foreach (keyword; keywords) {
				on(keyword, "lexKeywordOrIdentifier(Token.STATIC!`" ~ keyword ~ "`);");
			}
			return this;
		}

		CodeGenerator withOperators(alias operators)() {
			foreach (operator; operators) {
				on(operator, "lexStaticToken(Token.STATIC!`" ~ operator ~ "`);");
			}
			return this;
		}


		private CodeGenerator getCasesWithIsIdentifierChar(bool boolValue) {
			auto result = new CodeGenerator;
			result.code = code;
			foreach (key, value; cases) {
				if (value.previousCharWasFirstIdentifierChar == boolValue) result.cases[key] = value;
			}
			return result;
		}

		CodeGenerator getCasesThatStartWithFirstIdentifierChar() {
			return getCasesWithIsIdentifierChar(true);
		}

		CodeGenerator getCasesThatDoNotStartWithFirstIdentifierChar() {
			return getCasesWithIsIdentifierChar(false);
		}


		string generateCode(string onNoMatch) const {
			auto result = "";
			if (cases.length == 0) {
				result = code.length > 0 ? code : onNoMatch;
			} else {
				result = "switch(code[position++]){";
				foreach (key, value; cases) {
					result ~= key ~ value.generateCode(onNoMatch) ~ "break;";
				}
				result ~= "default:position--;" ~ (code.length > 0 ? code : onNoMatch);
				result ~= "}";
			}
			return result;
		}
	}


	private static string generateLexerCode() {
		return
			new CodeGenerator()
				.withKeywords!keywords
				.withOperators!operators

				.on!"/*"("lexBlockComment;")
				.on!"//"("lexLineComment;")
				.on!"/+"("lexNestingBlockComment;")

				.on!"#line"("lexLineSpecialTokenSequence;")

				.on!`r"`("lexWysiwygStringLiteral;")
				.on!"`"("lexAlternateWysiwygStringLiteral;")
				.on!`"`("lexDoubleQuotedStringLiteral;")
				.on!`x"`("lexHexStringLiteral;")
				.on!`q"`("lexDelimitedStringLiteral;")
				.on!"q{"("lexTokenStringLiteral;")

				.on!"'"("lexSingleQuotedCharacterLiteral;")

				.on!"0b"("lexBinaryNumberLiteral;")
				.on!"0B"("lexBinaryNumberLiteral;")
				.on!"0x"("lexHexNumberLiteral;")
				.on!"0X"("lexHexNumberLiteral;")
				.on!"0"("lexDecimalNumberLiteral;")
				.onNonZeroDigit("lexDecimalNumberLiteral;")

				.onEndOfFile("lexEofToken;")

				.generateCode("lexIdentifier;");
	}

	private void lexWhitespaceChar() {
		position++;
	}

	private void lexLineBreakOfLength(uint lengthInChars)() {
		position += lengthInChars;
		lineBeginPositions ~= position;
	}

	private void lexBlockComment() {
		currentToken.type = Token.COMMENT;
		position+=2;
		while ((code[position] != '*') || (code[position+1] != '/')) {
			position++;
		}
		currentToken.comment.value = code[currentToken.position+2 .. position];
		position+=2;
	}

	private void lexLineComment() {
		currentToken.type = Token.COMMENT;
		position+=2;
		do {
			switch (code[position]) {   // TODO: move this code to LexerCodeGenerator.onLineBreak
				case '\u000D':
					if (code[position+1] == '\u000A') position++;
					// fallthrough is intentional
				case '\u000A':
				case '\u2028':
				case '\u2029':
					currentToken.comment.value = code[currentToken.position+2 .. position];
					position++;
					break;
				default:
					continue;
			}
		} while (0);
	}

	private void lexNestingBlockComment() {
		currentToken.type = Token.COMMENT;
		assert(0);
	}

	private void lexLineSpecialTokenSequence() {
		assert(0);
	}

	private void lexKeywordOrIdentifier(ubyte type) {
		if (isIdentifierChar(code[position])) {
			lexIdentifier;
		} else {
			lexStaticToken(type);
		}
	}

	private void lexStaticToken(ubyte type) {
		currentToken.type = type;
	}

	private void lexWysiwygStringLiteral() {
		currentToken.type = Token.STRING_LITERAL;
		assert(0);
	}

	private void lexAlternateWysiwygStringLiteral() {
		currentToken.type = Token.STRING_LITERAL;
		assert(0);
	}

	private void lexDoubleQuotedStringLiteral() {
		currentToken.type = Token.STRING_LITERAL;
		assert(0);
	}

	private void lexHexStringLiteral() {
		currentToken.type = Token.STRING_LITERAL;
		assert(0);
	}

	private void lexDelimitedStringLiteral() {
		currentToken.type = Token.STRING_LITERAL;
		assert(0);
	}

	private void lexTokenStringLiteral() {
		currentToken.type = Token.STRING_LITERAL;
		assert(0);
	}

	private void lexSingleQuotedCharacterLiteral() {
		currentToken.type = Token.CHARACTER_LITERAL;
		assert(0);
	}

	private void lexBinaryNumberLiteral() {
		currentToken.type = Token.INTEGER_LITERAL;
		assert(0);
	}

	private void lexHexNumberLiteral() {
		currentToken.type = Token.INTEGER_LITERAL;
		assert(0);
	}

	private void lexDecimalNumberLiteral() {
		currentToken.type = Token.INTEGER_LITERAL;
		assert(0);
	}

	private void lexEofToken() {
		currentToken.type = Token.END_OF_FILE;
	}

	private void lexIdentifier() {
		currentToken.type = Token.IDENTIFIER;
		while (isIdentifierChar(code[position])) {
			position++;
		}
	}

	private void lexUnknown() {
		currentToken.type = Token.UNKNOWN;
		//position++;
	}

	private void skipWhitespaceLineBreaksAndComments() {
		while (true) {
			switch (code[position]) {
				case '\u000D':
				case '\u000A':
				case '\u2028':
				case '\u2029':
				case '\u0020':
				case '\u0009':
				case '\u000B':
				case '\u000C':
					break;
				//case '/':
				//	switch (code[position+1]) {
				//		case '/': skipLineComment();
				//		case '*': skipBlockComment();
				//		case '+': skipNestingBlockComment();
				//		default: return;
				//	}
				default: return;
			}
			position++;
		}
	}

	private static bool isIdentifierChar(dchar c) {
		return isIdentifierFirstChar(c) || ((c >= '0') && (c <= '9'));
	}

	private static bool isIdentifierFirstChar(dchar c) {
		return ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')) || (c == '_');
	}

	private static bool isNotIdentifierFirstChar(dchar c) {
		return !isIdentifierFirstChar(c);
	}


	private enum keywords = [
		"abstract", "alias", "align", "asm", "assert", "auto", "body", "bool",
		"break", "byte", "case", "cast", "catch", "cdouble", "cent", "cfloat",
		"dchar", "class", "const", "continue", "creal", "dchar", "debug", "default",
		"delegate", "delete", "deprecated", "do", "double", "else", "enum",
		"export", "extern", "false", "final", "finally", "float", "for", "foreach",
		"foreach_reverse", "function", "goto", "idouble", "if", "ifloat",
		"immutable", "import", "in", "inout", "int", "interface", "invariant",
		"ireal", "is", "lazy", "long", "macro", "mixin", "module", "new", "nothrow",
		"null", "out", "override", "package", "pragma", "private", "protected",
		"public", "pure", "real", "ref", "return", "scope", "shared", "short",
		"static", "struct", "super", "switch", "synchronized", "template", "this",
		"throw", "true", "try", "typedef", "typeid", "typeof", "ubyte", "ucent",
		"uint", "ulong", "union", "unittest", "ushort", "version", "virtual", "void",
		"volatile", "wchar", "while", "with", "__DATE__"/*, "__EOF__"*/, "__FILE__",
		"__FUNCTION__", "__gshared", "__LINE__", "__MODULE__", "__parameters",
		"__PRETTY_FUNCTION__", "__TIME__", "__TIMESTAMP__", "__traits", "__vector",
		"__VENDOR__", "__VERSION__",
	];

	private enum operators = [
		",", ".", "..", "...", "/", "/=", "!", "!<", "!<=", "!<>", "!<>=", "!=",
		"!>", "!>=", "$", "%", "%=", "&", "&&", "&=", "(", ")", "*", "*=", "+", "++",
		"+=", "-", "--", "-=", ":", ";", "<", "<<", "<<=", "<=", "<>", "<>=", "=",
		"==", "=>", ">", ">=", ">>", ">>=", ">>>", ">>>=", "?", "@", "[", "]", "^",
		"^=", "^^", "^^=", "{", "|", "|=", "||", "}", "~", "~=",
	];

	private enum staticTokens = keywords ~ operators;



	unittest {
		writeln(Lexer.Token.__LAST_DYNAMIC_TOKEN);
		writeln(Lexer.Token.STATIC!(staticTokens[0]));
		writeln(Lexer.Token.STATIC!(staticTokens[$-1]));
	}
}



unittest {

	import std.range : lockstep;

	enum MAX_TOKENS_COUNT = 20;

	uint testsCount = 0;
	uint failedTestsCount = 0;

	static string red(string s) { return "\033[31m" ~ s ~ "\033[0m"; }
	static string green(string s) { return "\033[32m" ~ s ~ "\033[0m"; }

	void test(dstring input, Lexer.Token[] expectedTokens) {

		static string succesfulComparison(string expected, string actual) {
			return green(expected) ~ " " ~ green(actual);
		}
		static string failedComparison(string expected, string actual) {
			return green(expected) ~ " " ~ red(actual);
		}

		struct TestResult {
			string name;
			bool failed = false;
			string[] report;

			string toString() {
				return failed ? red("FAIL") ~ "\nq{\n" ~ to!string(input) ~ "\n}\n  " ~ report.join("\n  ") ~ "\n\n" : "";
			}
		}

		bool tokensAreEqual(Lexer.Token a, Lexer.Token b) {
			if ((a.type != b.type) || (a.asString != b.asString) || (a.position != b.position)) return false;
			if ((a.type == Lexer.Token.STRING_LITERAL) && (a.stringLiteral != b.stringLiteral)) return false;
			if ((a.type == Lexer.Token.CHARACTER_LITERAL) && (a.characterLiteral != b.characterLiteral)) return false;
			if ((a.type == Lexer.Token.INTEGER_LITERAL) && (a.integerLiteral != b.integerLiteral)) return false;
			if ((a.type == Lexer.Token.FLOAT_LITERAL) && (a.floatLiteral != b.floatLiteral)) return false;
			return true;
		}

		string tokenToString(Lexer.Token token) {
			string type = to!string(token.type);
			string typeSpecific = "";
			if (token.type == Lexer.Token.UNKNOWN) type = "unknown";
			if (token.type == Lexer.Token.END_OF_FILE) type = "endOfFile";
			if (token.type == Lexer.Token.IDENTIFIER) type = "identifier";
			if (token.type == Lexer.Token.STRING_LITERAL) {
				type = "stringLiteral";
				typeSpecific = ` "` ~ to!string(token.stringLiteral.value) ~ '"';
			}
			if (token.type == Lexer.Token.CHARACTER_LITERAL) {
				type = "characterLiteral";
				typeSpecific = to!string(" '"d ~ token.characterLiteral.value ~ "'");
			}
			if (token.type == Lexer.Token.INTEGER_LITERAL) {
				type = "integerLiteral";
				typeSpecific = " " ~ to!string(token.integerLiteral.value);
			}
			if (token.type == Lexer.Token.FLOAT_LITERAL) {
				typeSpecific = " (" ~ to!string(token.floatLiteral.mantissa) ~ ", " ~
					to!string(token.floatLiteral.exponent) ~ ")";
			}
			return format("%s(%d|%d:%d - %d|%d:%d \"%s\"%s)",
				type,
				token.position, 0, 0,
				token.position + token.asString.length, 0, 0,
				token.asString,
				typeSpecific
			);
		}

		TestResult check(Lexer.Token[] actualTokens) {
			TestResult result;
			foreach (expected, actual; lockstep(expectedTokens, actualTokens)) {
				if (tokensAreEqual(expected, actual)) {
					result.report ~= succesfulComparison(tokenToString(expected), tokenToString(actual));
				} else {
					result.failed = true;
					result.report ~= failedComparison(tokenToString(expected), tokenToString(actual));
				}
			}
			if (expectedTokens.length > actualTokens.length) {
				result.failed = true;
				foreach (expected; expectedTokens[actualTokens.length..$]) {
					result.report ~= failedComparison(tokenToString(expected), "<end of list>");
				}
			}
			if (expectedTokens.length < actualTokens.length) {
				result.failed = true;
				foreach (actual; actualTokens[expectedTokens.length..$]) {
					result.report ~= failedComparison("<end of list>", tokenToString(actual));
				}
			}
			testsCount++;
			if (result.failed) failedTestsCount++;
			return result;
		}

		//Lexer.Token[] getTokens_foreach() {
		//	Lexer.Token[] result;
		//	auto lexer = new Lexer(input);
		//	foreach (token; lexer) {
		//		result ~= token;
		//	}
		//	return result;
		//}

		Lexer.Token[] getTokens_manualFront() {
			Lexer.Token[] result;
			auto lexer = new Lexer(input);
			uint i = 0;
			while (!lexer.empty && i < MAX_TOKENS_COUNT) {
				result ~= lexer.front;
				lexer.popFront;
				i++;
			}
			return result;
		}


		//write(check(getTokens_foreach));
		//write(check(getTokens_foreachWithCounter));
		write(check(getTokens_manualFront));
		//write(check(getTokens_manualMoveFront));
	}

	void testMap(Lexer.Token[][dstring] map) {
		foreach (key, value; map) {
			test(key, value);
		}
	}


	Lexer.Token[][dstring] _getAllCombinations(Lexer.Token[][dstring][] elements, uint positionIncrement) {
		if (elements.length == 0) return Lexer.Token[][dstring].init;
		if (elements.length == 1) return elements[0];
		Lexer.Token[][dstring] merged;
		foreach (input1, expected1; elements[0]) {
			foreach (input2, expected2; elements[1]) {
				if (expected2.length > 0) {
					expected2[0].position = positionIncrement + input1.length;
					for (uint i = 1; i < expected2.length; i++) {
						expected2[i].position = positionIncrement + expected2[i-1].position + expected2[i-1].asString.length;
					}
				}
				merged[input1 ~ input2] = expected1 ~ expected2;
			}
		}
		return _getAllCombinations(merged ~ elements[2..$], positionIncrement);
	}

	Lexer.Token[][dstring] getAllCombinations(Lexer.Token[][dstring][] elements...) {
		return _getAllCombinations(elements, 0);
	}


	Lexer.Token[][dstring] merge(Lexer.Token[][dstring][] elements...) {
		Lexer.Token[][dstring] merged;
		foreach (map; elements) {
			foreach (key, value; map) {
				merged[key] = value;
			}
		}
		return merged;
	}


	auto keyword(string s)() {
		return Lexer.Token(s, 0, Lexer.Token.STATIC!s);
	}
	auto identifier(string s)() {
		return Lexer.Token(s, 0, Lexer.Token.IDENTIFIER);
	}

	auto tokens = [
		"abstract"d: [keyword!"abstract"],

		"abstract1"d: [identifier!"abstract1"],
		"abstractalias"d: [identifier!"abstractalias"],
		"_abstract"d: [identifier!"_abstract"],
		"_1"d: [identifier!"_1"],
		"_"d: [identifier!"_"],
		"_01234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"d: [identifier!"_01234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"],
	];

	Lexer.Token[][dstring] whitespaceAndLineBreaks = [
		" "d: [],

		"\n"d: [],

		//""d: [],
	];

	testMap(tokens);
	testMap(whitespaceAndLineBreaks);
	testMap(getAllCombinations(whitespaceAndLineBreaks, tokens, whitespaceAndLineBreaks, tokens, whitespaceAndLineBreaks));
	if (failedTestsCount == 0) {
		writeln(green("All " ~ to!string(testsCount) ~ " tests passed"));
	} else {
		writeln(red(to!string(failedTestsCount) ~ " tests failed"));
	}
}
