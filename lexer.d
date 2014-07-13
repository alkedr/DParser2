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
			enum STATIC = typeForStaticToken(s);
		}
		static ubyte typeForStaticToken(string s) {
			assert(staticTokens.countUntil(s) != -1);
			auto result = __LAST_DYNAMIC_TOKEN + 1 + staticTokens.countUntil(s);
			assert(result < 256);
			return cast(ubyte)result;
		}
	}

	dstring code;     // TODO: InputRange
	Token currentToken;

	this(dstring code) {
		this.code = code ~ 0;
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
		mixin(
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

				.on!"0"("lexNumberLiteralThatStartsWithZero;")
				.onNonZeroDigit("lexDecimalNumberLiteral;")

				.onEndOfFile("lexEofToken;")

				.generateCode("lexIdentifier;")
		);
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


	class CodeGenerator {
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

	private void lexBlockComment() {
		currentToken.type = Token.COMMENT;
		while ((code[position] != '*') || (code[position+1] != '/')) {
			position++;
		}
		currentToken.comment.value = code[currentToken.position+2 .. position];
		position += 2;
	}

	private void lexLineComment() {
		currentToken.type = Token.COMMENT;
		position += 2;
		//while (true) {
		//	new CodeGenerator()
		//		.onLineBreak("if(code[position]=='/')")
		//}


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
		assert(0); // TODO: move to skipWhitespaceAndLineBreaks?
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

	private void lexNumberLiteralThatStartsWithZero() {
		currentToken.type = Token.INTEGER_LITERAL;
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
		"char", "class", "const", "continue", "creal", "dchar", "debug", "default",
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
		"volatile", "wchar", "while", "with", "__DATE__", "__EOF__", "__FILE__",
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

