module lexer2;

import std.algorithm : countUntil;
import std.string : format;
import std.conv : to;
import std.ascii;
import std.stdio;
import std.range : count;
import std.array : empty, join;


struct Lexer {
	dstring code;
	size_t position;

	this(dstring code) {
		this.code = code ~ 0;
	}

	immutable struct Coordinates {
		string fileName;
		immutable size_t line;
		immutable size_t column;
	}

	struct Comment {
		private const(Lexer*) lexer;
		immutable size_t startPosition;
		immutable size_t endPosition;

		dstring code() immutable { return lexer.code[startPosition..endPosition]; }
		Coordinates startCoordinates() immutable { return lexer.coordinates(startPosition); }
		Coordinates endCoordinates() immutable { return lexer.coordinates(endPosition); }
	}

	struct Token {
		private const(Lexer*) lexer;
		immutable size_t startPosition;
		immutable size_t endPosition;
		private immutable uint commentBlockId;
		immutable uint id;

		dstring code() immutable { return lexer.code[startPosition..endPosition]; }
		const(Comment[]) comments() immutable { return lexer.commentBlocks[commentBlockId]; }
		Coordinates startCoordinates() immutable { return lexer.coordinates(startPosition); }
		Coordinates endCoordinates() immutable { return lexer.coordinates(endPosition); }

		enum Type : ubyte {
			END_OF_FILE,
			DYNAMIC_TOKEN,
		}

		static uint idFor(string staticToken) {
			assert(staticTokens.countUntil(staticToken) != -1);
			auto result = staticTokens.countUntil(staticToken) + Type.max + 1;
			return cast(uint)result;
		}

		static uint idFor(string staticToken)() {
			return idFor(staticToken);
		}
	}


	Token nextToken() {
		auto commentBlockId = skipCommentsAndSkipWhitespaceAndLineBreaks();
		auto startPosition = position;
		auto id = lexToken;
		return Token(&this, startPosition, position, commentBlockId, id);
	}

	Coordinates coordinates(size_t position) const {
		return Coordinates("", 0, 0); // TODO
	}


	// For '#line NNN'
	private struct LineNumerationChange {
		size_t startFromLine;
		size_t newLineNumber;
		string fileName;
	}

	private Comment[][] commentBlocks = [[]];
	private size_t[] lineStarts = [0];
	private LineNumerationChange[] lineNumerationChanges = [{0, 0}];



	private uint lexToken() {

		static class CodeGenerator {
			private string code = "";
			private CodeGenerator[string] cases;

			CodeGenerator on(string charSequence, string code) {
				if (charSequence.length == 0) {
					this.code = code;
				} else {
					string caseString = format(`case'\x%02X':`, charSequence[0]);
					if (caseString !in cases) cases[caseString] = new CodeGenerator;
					cases[caseString].on(charSequence[1..$], code);
				}
				return this;
			}

			CodeGenerator on(string charSequence)(string code) {
				return on(charSequence, code);
			}

			CodeGenerator onStaticTokens(alias tokens)(string codeFmt) {
				foreach (token; tokens) on(token, format(codeFmt, token));
				return this;
			}

			string generateCode(bool topLevel = true) const {
				auto result = "";
				if (cases.length == 0) {
					result = code;
				} else {
					if (!topLevel) result = "switch(code[position++]){";
					foreach (key, value; cases) {
						result ~= key ~ value.generateCode(false) ~ "break;";
					}
					if (!topLevel) result ~= "default:position--;" ~ code ~ "break;}";
				}
				return result;
			}
		}

		switch (code[position++]) {
			mixin(new CodeGenerator()
				.onStaticTokens!keywords(q{return lexKeywordOrIdentifier(Token.idFor!(`%s`));})
				.onStaticTokens!operators(q{return Token.idFor!(`%s`);})
				//.on!`__EOF__`(q{ return Token.Type.END_OF_FILE; })
				.on!`r"`(q{ skipNext!'"'; return Token.Type.DYNAMIC_TOKEN; })
				.on!`x"`(q{ skipNext!'"'; return Token.Type.DYNAMIC_TOKEN; })
				.on!`q"`(q{ skipDelimitedStringLiteral; return Token.Type.DYNAMIC_TOKEN; })
				.on!"q{"(q{ skipTokenStringLiteral; return Token.Type.DYNAMIC_TOKEN; })
				.on!"."(q{ return lexDecimalFloatLiteralThatStartsWithDotOrOperatorDot; })
				.generateCode
			);

			case '`':
				skipNext!'`';
				return Token.Type.DYNAMIC_TOKEN;

			case '"':
				skipNextWithEscapeSequences!'"';
				return Token.Type.DYNAMIC_TOKEN;

			case '\'':
				skipNextWithEscapeSequences!'\'';
				return Token.Type.DYNAMIC_TOKEN;

			case '0': .. case '9':
				skipNumberLiteral;
				return Token.Type.DYNAMIC_TOKEN;

			case '\x00':
			case '\x1A':
				return Token.Type.END_OF_FILE;

			default: break;
		}

		if ((position == 0) || isIdentifierChar(code[position-1])) {
			skipCharsWhile!isIdentifierChar;
			return Token.Type.DYNAMIC_TOKEN;
		} else {
			writeln(code);
			assert(0);
		}
	}

	private uint lexKeywordOrIdentifier(uint keywordId) {
		if (isIdentifierChar(code[position])) {
			skipCharsWhile!isIdentifierChar;
			return Token.Type.DYNAMIC_TOKEN;
		} else {
			return keywordId;
		}
	}

	private void skipDelimitedStringLiteral() {
		// TODO
		skipNext!'"';
	}

	private void skipTokenStringLiteral() {
		uint depth = 1;
		while (depth > 0) {
			auto token = nextToken;
			if (token.id == Token.idFor("{")) depth++;
			if (token.id == Token.idFor("}")) depth--;
		}
	}

	private void skipNumberLiteral() {
		if (code[position-1] == '0') {
			if ((code[position] == 'b') || (code[position] == 'B')) {
				position++;
				skipCharsWhile!isBinaryLiteralDigit;
				skipCharsWhile!isAlpha;
				return;
			} else if ((code[position] == 'x') || (code[position] == 'X')) {
				position++;
				skipCharsWhile!isHexLiteralDigit;
				if ((code[position] == '.') && (code[position+1] != '.')) {
					position++;
					skipCharsWhile!isHexLiteralDigit;
				}
				skipCharsWhile!isAlpha;
				return;
			}
		}
		skipCharsWhile!isDecimalLiteralDigit;
		if (code[position] == '.') {
			position++;
			skipCharsWhile!isDecimalLiteralDigit;
		}
		skipCharsWhile!isAlpha;
	}

	private uint lexDecimalFloatLiteralThatStartsWithDotOrOperatorDot() {
		if (isDigit(code[position])) {
			skipCharsWhile!isDigit;
			skipCharsWhile!isAlpha;
			return Token.Type.DYNAMIC_TOKEN;
		} else {
			return Token.idFor(`.`);
		}
	}

	// returns commentBlockId
	private uint skipCommentsAndSkipWhitespaceAndLineBreaks() {
		//switch (code[position++]) {
		//	case '/': lexComment;

		//}
		return 0;  // TODO
	}


	private void skipCharsWhile(alias contition, string skipCode = "")() {
		while ((position < code.length) && (contition(code[position]))) {   // TODO: newlines
			mixin(skipCode);
			position++;
		}
	}

	private void skipNext(char terminator, string skipCode = "")() {
		static bool notChar(dchar expected)(dchar actual) { return actual != expected; }
		skipCharsWhile!(notChar!terminator, skipCode);
		position++;
	}

	private void skipNextWithEscapeSequences(char terminator)() {
		skipNext!(terminator, `if (code[position] == '\\') position++;`);
	}

	private static bool isIdentifierChar(dchar c) { return isAlphaNum(c) || (c == '_'); }
	private static bool isIdentifierFirstChar(dchar c) { return isAlpha(c) || (c == '_'); }
	private static bool isBinaryLiteralDigit(dchar c) { return (c == '0') || (c == '1') || (c == '_'); }
	private static bool isDecimalLiteralDigit(dchar c) { return isDigit(c) || (c == '_'); }
	private static bool isHexLiteralDigit(dchar c) { return isHexDigit(c) || (c == '_'); }


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

}











unittest {
	writeln("Lexer.Comment.sizeof: ", Lexer.Comment.sizeof);
	writeln("Lexer.Token.sizeof: ", Lexer.Token.sizeof);
}


// single tokens
unittest {

	static string escapeSequence(uint number, string s) {
		return format("\033[%dm%s\033[0m", number, s);
	}
	static string red(string s) { return escapeSequence(33, s); }
	static string green(string s) { return escapeSequence(32, s); }

	// returns empty string if test passed
	static string testCase(dstring code, int id) {
		string report = to!string(code) ~ "\n";
		bool failed = false;

		static string structTokenToString(Lexer.Token token) {
			return format("%d(%d..%d, %d)", token.id, token.startPosition, token.endPosition, token.commentBlockId);
		}

		void assertEquals(Lexer.Token actual, Lexer.Token expected) {
			auto expectedString = structTokenToString(expected);
			auto actualString = structTokenToString(actual);
			if (actualString == expectedString) {
				report ~= green(actualString);
			} else {
				failed = true;
				report ~= green(expectedString) ~ " " ~ red(actualString);
			}
			report ~= "\n";
		}

		auto lexer = Lexer(code);
		assertEquals(lexer.nextToken, Lexer.Token(null, 0, code.length, 0, id));
		assertEquals(lexer.nextToken, Lexer.Token(null, code.length, code.length+1, 0, Lexer.Token.Type.END_OF_FILE));

		return failed ? report ~ "\n" : "";
	}

	static void test(string name, int[dstring] cases) {
		string[] reports;
		foreach (code, id; cases) {
			reports ~= testCase(code, id);
		}
		auto passedCount = reports.count!empty;
		auto titleString = format("%s %3d / %3d", name, passedCount, cases.length);
		if (passedCount == cases.length) {
			writeln(green(titleString));
		} else {
			writeln(red(titleString));
			writeln(reports.join("\n"));
		}
	}



	static int[dstring] casesOf(uint id)(string[] codes) {
		int[dstring] result;
		foreach (code; codes) {
			result[to!dstring(code)] = id;
		}
		return result;
	}

	static int[dstring] staticTokens() {
		int[dstring] result;
		foreach (staticToken; Lexer.staticTokens) {
			result[to!dstring(staticToken)] = Lexer.Token.idFor(staticToken);
		}
		return result;
	}

	auto identifiers = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		// basic rules
		"simpleIdentifier",
		"_01234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM",
		"Aa1",
		"_1",
		"_",

		// distinction from string literals ( r" x" q" q{ )
		"r",
		"x",
		"q",
		"r_",
		"x_",
		"q_",

		// distinction from keywords
		"abstract_",
		"abstract1",
		"abstracta",
		"abstractA",
		"_abstract",
		"aabstract",
		"Aabstract",
	]);

	auto wysiwygStringLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		`r""`,
		`r"r"`,
		`r"string literal"`,
		`r"string literal with ŪŅİĆŌĐĒ symbols"`,
		`r"\"`,
		`r"\\"`,
	]);

	auto alternateWysiwygStringLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		r"``",
		r"`string literal`",
		r"`string literal with ŪŅİĆŌĐĒ symbols`",
		r"`\`",
		r"`\\`",
	]);

	auto doubleQuotedStringLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		`""`,
		`"string literal"`,
		`"string literal with ŪŅİĆŌĐĒ symbols"`,
		`"\\"`,
		`"\\\\"`,
		`"\""`,
		`"\"\""`,
	]);

	auto hexStringLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		`""`,
		`"0123456789ABCDEF"`,
		`"01 23	45 67 89 AB CD EF"`,
	]);

	auto delimitedStringLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		// identifier delimiters
		"q\"EOS\nEOS\"",

		// char delimiters
		`q"//`,

		// nesting delimiters
		`q"()"`,
		`q"[]"`,
		`q"<>"`,
		`q"{}"`,
	]);

	// TODO: commented '{' and '}'
	auto tokenStringLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		`q{}`,
		`q{"string literal"}`,
		`q{"string literal with ŪŅİĆŌĐĒ symbols"}`,
		`q{abstract}`,
		`q{;}`,
		`q{{{}{}}}`,
	]);

	auto characterLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		`'a'`,
		`'\''`,
		`'\U01234567'`,
	]);

	auto decimalIntegerLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		"0",
		"1",
		"12345678900987654321",
		// TODO: _
		// TODO: suffixes
	]);

	auto binaryIntegerLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		"0b0",
		"0b1",
		"0b101010101",
		"0b000101010101",
	]);

	auto hexIntegerLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		"0x0",
		"0x1",
		"0x0123456789ABCDEFabcdef",
	]);

	auto decimalFloatLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		".0",
		".1",
		"0.0",
		"0.1",
		"1.0",
		"1.1",
		"1234567890.0",
		"0.1234567890",

		//"0f",
		//"1f",
		//"12345678900987654321f",

		//"0d",
		//"1d",
		//"12345678900987654321d",
	]);

	auto hexFloatLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		"0x.0",
		"0x.1",
		"0x0.0",
		"0x0.1",
		"0x1.0",
		"0x1.1",
		"0x1234567890abcdef.0",
		"0x0.1234567890abcdef",
	]);

	auto imaginaryFloatLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		"0i",
		"1i",
	]);


// TODO: check unexpected end of file error
// TODO: CHECK ERRORS!
// TODO: check line breaks inside literals
// TODO: check suffixes (string, char, int, float, imaginary)
// TODO: _ в числах
// TODO: test  "1..2" - 3 tokens


	test("Identifiers                          ", identifiers);
	test("Literals / String / Wysiwyg          ", wysiwygStringLiterals);
	test("Literals / String / Alternate wysiwyg", alternateWysiwygStringLiterals);
	test("Literals / String / Double quoted    ", doubleQuotedStringLiterals);
	test("Literals / String / Hexadecimal      ", hexStringLiterals);
//test("Literals / String / Delimited        ", delimitedStringLiterals);
	test("Literals / String / Token            ", tokenStringLiterals);
	test("Literals / Character                 ", characterLiterals);
	test("Literals / Integer / Decimal         ", decimalIntegerLiterals);
	test("Literals / Integer / Binary          ", binaryIntegerLiterals);
	test("Literals / Integer / Hexadecimal     ", hexIntegerLiterals);
	test("Literals / Float / Decimal           ", decimalFloatLiterals);
	test("Literals / Float / Hexadecimal       ", hexFloatLiterals);
//test("Literals / Float / Imaginary         ", imaginaryFloatLiterals);
	test("Keywords and operators               ", staticTokens);
}
