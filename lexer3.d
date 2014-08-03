module lexer2;

import std.algorithm : countUntil;
import std.string : format;
import std.conv : to;
import std.ascii;
import std.stdio;
import std.range;
import std.array : empty, join;

// TODO: compile-time switches for comments?
// TODO: store line and column in array in Lexer because Tokens will be part of AST classes?
struct Lexer {
	dstring code;
	Coordinates position;

	this(dstring code) {
		this.code = code ~ 0;
	}

	struct Coordinates {
		string fileName;
		size_t index;
		size_t line;
		size_t column;
	}

	struct Range {
		Coordinates start;
		Coordinates end;
	}

	struct Comment {
		Range range;
		dstring code;
	}

	struct Token {
		Range range;
		Comment[] comments;
		dstring code;
		uint id;

		enum Type : ubyte {
			END_OF_FILE,
			UNEXPECTED_END_OF_FILE,
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
					if (!topLevel) result = "switch(advance){";
					foreach (key, value; cases) {
						result ~= key ~ value.generateCode(false) ~ "break;";
					}
					if (!topLevel) result ~= "default:position.index--;position.column--;" ~ code ~ "break;}";
				}
				return result;
			}
		}

		Token result;
		result.range.start = position;
		result.id = Token.Type.DYNAMIC_TOKEN;

		do {
			skipWhitespaceAndLineBreaks;
			switch (advance) {
				mixin(new CodeGenerator()
					.onStaticTokens!keywords(q{result.id = lexKeywordOrIdentifier(Token.idFor!(`%s`));})
					.onStaticTokens!operators(q{result.id = Token.idFor!(`%s`);})
					//.on!`__EOF__`(q{ return Token.Type.END_OF_FILE; })
					.on!`r"`(q{ skipNext!'"'; })
					.on!`x"`(q{ skipNext!'"'; })
					.on!`q"`(q{ skipDelimitedStringLiteral; })
					.on!"q{"(q{ skipTokenStringLiteral; })
					.on!"."(q{ result.id = lexDecimalFloatLiteralThatStartsWithDotOrOperatorDot; })
					.on!"/*"(q{ result.comments ~= lexBlockComment; continue; })
					.on!"//"(q{ result.comments ~= lexLineComment; continue; })
					.on!"/+"(q{ result.comments ~= lexNestingBlockComment; continue; })
					.generateCode
				);

				case '`':
					skipNext!'`';
					break;

				case '"':
					skipNextWithEscapeSequences!'"';
					break;

				case '\'':
					skipNextWithEscapeSequences!'\'';
					break;

				case '0': .. case '9':
					skipNumberLiteral;
					break;

				case '\x00':
				case '\x1A':
					result.id = Token.Type.END_OF_FILE;

				default: break;
			}
		} while (0);

		if ((result.id == Token.Type.DYNAMIC_TOKEN) && ((position.index == 0) || isIdentifierChar(previousChar))) {
			skipCharsWhile!isIdentifierChar;
		}

		result.range.end = position;

		return result;
	}


	private dchar currentChar() { return code[position.index]; }
	private dchar advance() { position.column++; return code[position.index++]; }
	private dchar nextChar() { return code[position.index+1]; }
	private dchar previousChar() { return code[position.index-1]; }

	private void skipWhitespaceAndLineBreaks() {
		do {
			switch (advance) {
				case '\u000D':
					if (currentChar == '\u000A') advance;
				case '\u000A':
				case '\u2028':
				case '\u2029':
					position.line++;
					position.column = 0;
				case '\u0020':
				case '\u0009':
				case '\u000B':
				case '\u000C':
					continue;
				default:
					position.index--;
					position.column--;
					break;
			}
		} while (0);
	}

	private Comment lexBlockComment() {
		return Comment();
	}

	private Comment lexLineComment() {
		return Comment();
	}

	private Comment lexNestingBlockComment() {
		return Comment();
	}

	private uint lexKeywordOrIdentifier(uint keywordId) {
		if (isIdentifierChar(currentChar)) {
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
		if (previousChar == '0') {
			if ((currentChar == 'b') || (currentChar == 'B')) {
				advance;
				skipCharsWhile!isBinaryLiteralDigit;
				skipCharsWhile!isAlpha;
				return;
			} else if ((currentChar == 'x') || (currentChar == 'X')) {
				advance;
				skipCharsWhile!isHexLiteralDigit;
				if ((currentChar == '.') && (nextChar != '.')) {
					advance;
					skipCharsWhile!isHexLiteralDigit;
				}
				skipCharsWhile!isAlpha;
				return;
			}
		}
		skipCharsWhile!isDecimalLiteralDigit;
		if (currentChar == '.') {
			advance;
			skipCharsWhile!isDecimalLiteralDigit;
		}
		skipCharsWhile!isAlpha;
	}

	private uint lexDecimalFloatLiteralThatStartsWithDotOrOperatorDot() {
		if (isDigit(currentChar)) {
			skipCharsWhile!isDigit;
			skipCharsWhile!isAlpha;
			return Token.Type.DYNAMIC_TOKEN;
		} else {
			return Token.idFor(`.`);
		}
	}


	private void skipCharsWhile(alias contition, string skipCode = "")() {
		while ((position.index < code.length) && (contition(currentChar))) {   // TODO: newlines
			mixin(skipCode);
			advance;
		}
	}

	private void skipNext(char terminator, string skipCode = "")() {
		static bool notChar(dchar expected)(dchar actual) { return actual != expected; }
		skipCharsWhile!(notChar!terminator, skipCode);
		advance;
	}

	private void skipNextWithEscapeSequences(char terminator)() {
		skipNext!(terminator, `if (currentChar == '\\') advance;`);
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
	writeln("Lexer.Coordinates.sizeof: ", Lexer.Coordinates.sizeof);
	writeln("Lexer.Range.sizeof: ", Lexer.Range.sizeof);
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
			return format("%d(%d %d:%d - %d %d:%d)",
				token.id,
				token.range.start.index, token.range.start.line, token.range.start.column,
				token.range.end.index, token.range.end.line, token.range.end.column
				// TODO: comments and code
				);
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
		assertEquals(lexer.nextToken, Lexer.Token(Lexer.Range(Lexer.Coordinates("", 0, 0, 0), Lexer.Coordinates("", code.length, 0, code.length)), [], code, id));
		assertEquals(lexer.nextToken, Lexer.Token(Lexer.Range(Lexer.Coordinates("", code.length, 0, code.length), Lexer.Coordinates("", code.length+1, 0, code.length+1)), [], "\0", Lexer.Token.Type.END_OF_FILE));

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
		"y",
		"z",

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

	// TODO
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
		"1_2_3_4_5_6_7_8_9_0_0_9_8_7_6_5_4_3_2__1__",
		"1u",
		"1uL",
	]);

	auto binaryIntegerLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		"0b0",
		"0b1",
		"0b101010101",
		"0b000101010101",
		"0b1___010__1_0101",
		"0b1u",
		"0b1uL",
	]);

	auto hexIntegerLiterals = casesOf!(Lexer.Token.Type.DYNAMIC_TOKEN)([
		"0x0",
		"0x1",
		"0x0123456789ABCDEFabcdef",
		"0x01__23cd__ef",
		"0x1u",
		"0x1uL",
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

		"0f",
		"1f",
		"12345678900987654321f",

		"0d",
		"1d",
		"12345678900987654321d",
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
// TODO: check line breaks inside literals
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
	test("Literals / Float / Imaginary         ", imaginaryFloatLiterals);
	test("Keywords and operators               ", staticTokens);
}





unittest {
	static string[] sequence(string[][] items...) {
		if (items.length == 1) {
			return items[0];
		} else {
			string[] result;
			foreach (s1; items[0]) {
				foreach (s2; sequence(items[1..$])) {
					result ~= (s1 ~ s2);
				}
			}
			return result;
		}
	}


	// char classes
	auto whitespace = ["\u0020", "\u0009", "\u000B", "\u000C"];
	auto lineBreak = ["\u000D", "\u000A", "\u000D\u000A", "\u2028", "\u2029"];

	auto underscore = ["_"];

	auto binaryDigit = ["0", "1"];
	auto decimalDigit = binaryDigit ~ ["2", "3", "4", "5", "6", "7", "8", "9"];
	auto hexDigit = decimalDigit ~ ["a", "A", "b", "B", "c", "C", "d", "D", "e", "E", "f", "F"];

	auto binaryLiteralDigit = binaryDigit ~ underscore;
	auto decimalLiteralDigit = decimalDigit ~ underscore;
	auto hexLiteralDigit = hexDigit ~ underscore;

	auto smallLetter = iota(0, 26).map!(i => to!string([cast(char)('a' + i)])).array;
	auto bigLetter = iota(0, 26).map!(i => to!string([cast(char)('A' + i)])).array;
	auto letter = smallLetter ~ bigLetter;

	auto firstIdentifierChar = letter ~ underscore;
	auto identifierChar = letter ~ underscore ~ decimalDigit;

	// char sequences
	auto empty = [""];

	auto whitespaces = empty ~ whitespace ~ sequence(whitespace, whitespace);

	auto twoLineBreaks = sequence(lineBreak, whitespace, lineBreak);
	auto threeLineBreaks = sequence(twoLineBreaks, whitespace, lineBreak);

	auto identifier =
		firstIdentifierChar ~
		sequence(firstIdentifierChar, underscore ~ decimalDigit) ~
		sequence(empty ~ firstIdentifierChar, ["abstract"], identifierChar) ~
		sequence(firstIdentifierChar, ["abstract"])
		;

	//writeln(identifier);
	//writeln(identifier.length);



	//auto anythingButDoubleQuoteAndLineBreak = [
	//	``,
	//	`ŪŅİĆŌĐĒ symbols`,
	//];

	//auto wysiwygStringLiterals = [[`r"`], anythingButDoubleQuoteAndLineBreak, [`"`]];


	//test("Literals / String / Wysiwyg          ", wysiwygStringLiterals);

}
