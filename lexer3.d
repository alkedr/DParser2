module lexer2;

import std.algorithm;
import std.string : format;
import std.conv : to;
import std.ascii;
import std.stdio;
import std.range;
import std.array;


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
			IDENTIFIER,
			STRING_LITERAL,
			CHARACTER_LITERAL,
			INTEGER_LITERAL,
			FLOAT_LITERAL,
		}

		static uint idFor(string staticToken) {
			assert(staticTokens.countUntil(staticToken) != -1);
			auto result = staticTokens.countUntil(staticToken) + Type.max + 1;
			return cast(uint)result;
		}
	}


	Token nextToken() {
		auto commentBlockId = lexCommentsAndSkipWhitespaceAndLineBreaks();
		auto startPosition = position;
		auto id = lexToken();
		// TODO: detect __EOF__?
		return Token(&this, startPosition, position, commentBlockId, id);
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


	private Coordinates coordinates(size_t position) const {
		return Coordinates("", 0, 0); // TODO
	}


	private uint lexToken() {
		mixin(
			new CodeGenerator()
				.onStaticTokens!keywords(q{if (!isIdentifierChar(code[position])) return Token.idFor(`%s`);})
				.onStaticTokens!operators(q{return Token.idFor(`%s`);})
				// TODO: delimited string literal
				.on!`r"`(q{skipToChar!('"'); position++; return Token.Type.STRING_LITERAL;})
				.on!`x"`(q{skipToChar!('"'); position++; return Token.Type.STRING_LITERAL;})
				.on!`q"`(q{skipToChar!('"'); position++; return Token.Type.STRING_LITERAL;})
				.on!"q{"(q{skipTokenStringLiteral; return Token.Type.STRING_LITERAL;})
				.on!"`" (q{skipToChar!('`'); position++; return Token.Type.STRING_LITERAL;})
				.on!`"` (q{skipToCharWithEscapeSequences!'"'; position++; return Token.Type.STRING_LITERAL;})
				.on!"'" (q{skipToCharWithEscapeSequences!'\''; position++; return Token.Type.CHARACTER_LITERAL;})
				.on!"0" (q{return lexNumberLiteralThatStartsWithZero;})
				.onOneOfChars!"123456789"(q{return lexDecimalNumberLiteral;})
				.on!"."(
					new CodeGenerator()
						.onOneOfChars!"0123456789"(q{return lexDecimalFloatThatStartsWithDot;})
						.generateCode(q{return Token.idFor(`.`);})
				)
				.onOneOfChars!"\x00\x1A"(q{return Token.Type.END_OF_FILE;})
				.generateCode(q{})
		);

		if ((position == 0) || isIdentifierChar(code[position-1])) {
			skipCharsWhile!"isIdentifierChar(code[position])";
			return Token.Type.IDENTIFIER;
		} else {
			assert(0);
		}
	}

	private void skipTokenStringLiteral() {
		uint depth = 1;
		while (depth > 0) {
			auto token = nextToken;
			if (token.id == Token.idFor("{")) depth++;
			if (token.id == Token.idFor("}")) depth--;
		}
	}

	private uint lexDecimalNumberLiteral() {
		skipCharsWhile!"isDigit(code[position])";   // TODO: '_',  detect INT and FLOAT literals
		if ((code[position] == '.') && (isDigit(code[position+1]))) {
			position++;
			skipCharsWhile!"isDigit(code[position])";
			skipCharsWhile!"isAlpha(code[position])";
			return Token.Type.FLOAT_LITERAL;
		} else {
			skipCharsWhile!"isAlpha(code[position])";
			return Token.Type.INTEGER_LITERAL;
		}
	}

	private uint lexBinaryNumberLiteral() {
		skipCharsWhile!"(code[position] == '0') || (code[position] == '1')";
		return Token.Type.INTEGER_LITERAL;
	}

	private uint lexHexNumberLiteral() {
		skipCharsWhile!"isHexDigit(code[position])";
		if (code[position] == '.') {
			position++;
			skipCharsWhile!"isHexDigit(code[position])";
			return Token.Type.FLOAT_LITERAL;
		} else {
			return Token.Type.INTEGER_LITERAL;
		}
	}

	private uint lexDecimalFloatThatStartsWithDot() {
		skipCharsWhile!"isDigit(code[position])";
		skipCharsWhile!"isAlpha(code[position])";
		return Token.Type.FLOAT_LITERAL;
	}

	private uint lexNumberLiteralThatStartsWithZero() {
		switch (code[position++]) {
			case 'b': case 'B': return lexBinaryNumberLiteral;
			case 'x': case 'X': return lexHexNumberLiteral;
			case '.': return lexDecimalFloatThatStartsWithDot;
			default: position--; break;
		}
		return Token.Type.INTEGER_LITERAL;
	}


	// returns commentBlockId
	private uint lexCommentsAndSkipWhitespaceAndLineBreaks() {
		return 0;  // TODO
	}


	private void skipCharsWhile(string contition, string skipCode = "")() {
		while ((position < code.length) && (mixin(contition))) {   // TODO: newlines
			mixin(skipCode);
			position++;
		}
	}

	private void skipToChar(char terminator)() {
		skipCharsWhile!(format(`code[position] != '\x%2x'`, terminator));
	}

	private void skipToCharWithEscapeSequences(char terminator)() {
		skipCharsWhile!(format(`code[position] != '\x%2x'`, terminator),
			`if (code[position] == '\\') position++;`);
	}


	private static bool isIdentifierChar(dchar c) {
		return isAlphaNum(c) || (c == '_');
	}

	private static bool isIdentifierFirstChar(dchar c) {
		return isAlpha(c) || (c == '_');
	}




	private class CodeGenerator {
		private string code = "";
		private CodeGenerator[string] cases;

		private CodeGenerator getCodeGeneratorForCase(string caseString) {
			if (caseString !in cases) cases[caseString] = new CodeGenerator;
			return cases[caseString];
		}

		CodeGenerator on(string charSequence, string code) {
			if (charSequence.length == 0) {
				this.code = code;
			} else {
				getCodeGeneratorForCase(format(`case'\x%02X':`, charSequence[0])).on(charSequence[1..$], code);
			}
			return this;
		}

		CodeGenerator on(string charSequence)(string code) {
			return on(charSequence, code);
		}

		CodeGenerator onOneOfChars(string chars)(string code) {
			getCodeGeneratorForCase(chars.map!(c => format(`case'\x%02X':`, c)).join).code = code;
			return this;
		}

		CodeGenerator onStaticTokens(alias staticTokens)(string codeFormatString) {
			foreach (staticToken; staticTokens) {
				on(staticToken, format(codeFormatString, staticToken));
			}
			return this;
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

	auto identifiers = casesOf!(Lexer.Token.Type.IDENTIFIER)([
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

	auto wysiwygStringLiterals = casesOf!(Lexer.Token.Type.STRING_LITERAL)([
		`r""`,
		`r"string literal"`,
		`r"string literal with ŪŅİĆŌĐĒ symbols"`,
		`r"\"`,
		`r"\\"`,
		`r"r"`,
	]);

	auto alternateWysiwygStringLiterals = casesOf!(Lexer.Token.Type.STRING_LITERAL)([
		"``",
		"`string literal`",
		"`string literal with ŪŅİĆŌĐĒ symbols`",
		"`\\`",
		"`\\\\`",
	]);

	auto doubleQuotedStringLiterals = casesOf!(Lexer.Token.Type.STRING_LITERAL)([
		`""`,
		`"string literal"`,
		`"string literal with ŪŅİĆŌĐĒ symbols"`,
		`"\\"`,
		`"\\\\"`,
		`"\""`,
		`"\"\""`,
	]);

	auto hexStringLiterals = casesOf!(Lexer.Token.Type.STRING_LITERAL)([
		`""`,
		`"0123456789ABCDEF"`,
		`"01 23	45 67 89 AB CD EF"`,
	]);

	auto delimitedStringLiterals = casesOf!(Lexer.Token.Type.STRING_LITERAL)([
		// identifier delimiters
		`q"EOS\nEOS"`,

		// char delimiters
		`q"//`,

		// nesting delimiters
		`q"()"`,
		`q"[]"`,
		`q"<>"`,
		`q"{}"`,
	]);

	// TODO: commented '{' and '}'
	auto tokenStringLiterals = casesOf!(Lexer.Token.Type.STRING_LITERAL)([
		`q{}`,
		`q{"string literal"}`,
		`q{"string literal with ŪŅİĆŌĐĒ symbols"}`,
		`q{abstract}`,
		`q{;}`,
		`q{{{}{}}}`,
	]);

	auto characterLiterals = casesOf!(Lexer.Token.Type.CHARACTER_LITERAL)([
		`'a'`,
		`'\''`,
		`'\U01234567'`,
	]);

	auto decimalIntegerLiterals = casesOf!(Lexer.Token.Type.INTEGER_LITERAL)([
		"0",
		"1",
		"12345678900987654321",
	]);

	auto binaryIntegerLiterals = casesOf!(Lexer.Token.Type.INTEGER_LITERAL)([
		"0b0",
		"0b1",
		"0b101010101",
		"0b000101010101",
	]);

	auto hexIntegerLiterals = casesOf!(Lexer.Token.Type.INTEGER_LITERAL)([
		"0x0",
		"0x1",
		"0x0123456789ABCDEFabcdef",
	]);

	auto decimalFloatLiterals = casesOf!(Lexer.Token.Type.FLOAT_LITERAL)([
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

	auto hexFloatLiterals = casesOf!(Lexer.Token.Type.FLOAT_LITERAL)([
		"0x.0",
		"0x.1",
		"0x0.0",
		"0x0.1",
		"0x1.0",
		"0x1.1",
		"0x1234567890abcdef.0",
		"0x0.1234567890abcdef",
	]);

	auto imaginaryFloatLiterals = casesOf!(Lexer.Token.Type.FLOAT_LITERAL)([
		"0i",
		"1i",
	]);


// TODO: check unexpected end of file error
// TODO: CHECK ERRORS!
// TODO: check line breaks inside literals
// TODO: check suffixes (string, char, int, float, imaginary)
// TODO: _ в числах


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
