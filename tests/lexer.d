module tests.lexer;

import lexer;
import std.stdio;
import std.ascii;
import std.algorithm;
import std.array;
import std.range : lockstep;


private {

	enum MAX_TOKENS_COUNT = 20;

	uint testsCount = 0;
	uint failedTestsCount = 0;

	static string escapeSequence(uint number, string s) {
		return format("\033[%dm%s\033[0m", number, s);
	}
	static string red(string s) { return escapeSequence(33, s); }
	static string green(string s) { return escapeSequence(32, s); }


	class TestCase {
		dstring input;
		Lexer.Token[] expectedTokens;

		this(dstring input, Lexer.Token[] expectedTokens...) {
			this.input = input;
			this.expectedTokens = expectedTokens;
		}

		Result run() {

			static string succesfulComparison(string expected, string actual) {
				return green(expected) ~ " " ~ green(actual);
			}
			static string failedComparison(string expected, string actual) {
				return green(expected) ~ " " ~ red(actual);
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

			auto actualTokens = getTokens_manualFront();
			auto result = new Result;
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

		class Result {
			bool failed = false;
			string[] report;

			override string toString() {
				return failed ? red("FAIL") ~ "\nq{\n" ~ to!string(input) ~ "\n}\n  " ~ report.join("\n  ") ~ "\n\n" : "SUCCESS";
			}
		}
	}

	TestCase unknown(ubyte type)(dstring code, string message) {
		auto expected = Lexer.Token(code, 0, Lexer.Token.UNKNOWN);
		expected.unknown.triedToParseAsType = type;
		expected.unknown.errorMessage = message;
		return new TestCase(code, [expected]);
	}

	TestCase whitespace(dstring code) {
		return new TestCase(code, []);
	}

	TestCase endOfLine(dstring code) {
		return new TestCase(code, []);
	}

	TestCase comment(Lexer.Token.Comment.Type type)(dstring code, dstring value) {
		auto expected = Lexer.Token(code, 0, Lexer.Token.COMMENT);
		expected.comment.type = type;
		expected.comment.value = value;
		return new TestCase(code, [expected]);
	}

	TestCase blockComment(dstring text) {
		return comment!(Lexer.Token.Comment.Type.BLOCK)("/*" ~ text ~ "*/", text);
	}

	TestCase lineComment(dstring text, dstring endOfLine) {
		return comment!(Lexer.Token.Comment.Type.LINE)("//" ~ text ~ endOfLine, text);
	}

	TestCase nestingBlockComment(dstring text) {
		return comment!(Lexer.Token.Comment.Type.NESTING_BLOCK)("/+" ~ text ~ "+/", text);
	}

	TestCase specialTokenSequence(dstring code) {
		return new TestCase(code, []);
	}

	TestCase identifier(dstring code) {
		return new TestCase(code, [Lexer.Token(code, 0, Lexer.Token.IDENTIFIER)]);
	}

	TestCase stringLiteral(Lexer.Token.StringLiteral.Type type)(dstring code, dstring value) {
		auto expected = Lexer.Token(code, 0, Lexer.Token.STRING_LITERAL);
		expected.stringLiteral.type = type;
		expected.stringLiteral.value = value;
		return new TestCase(code, [expected]);
	}

	TestCase wysiwygStringLiteral(dstring value) {
		return stringLiteral!(Lexer.Token.StringLiteral.Type.WYSIWYG)(`r"` ~ value ~ `"`, value);
	}

	TestCase alternateWysiwygStringLiteral(dstring value) {
		return stringLiteral!(Lexer.Token.StringLiteral.Type.ALTERNATE_WYSIWYG)('`' ~ value ~ '`', value);
	}

	TestCase doubleQuotedStringLiteral(dstring code, dstring value) {
		return stringLiteral!(Lexer.Token.StringLiteral.Type.DOUBLE_QUOTED)('"' ~ value ~ '"', value);
	}

	TestCase doubleQuotedStringLiteral(dstring code) {
		return doubleQuotedStringLiteral(code, code);
	}

	TestCase hexStringLiteral(dstring hexText, dstring value) {
		return stringLiteral!(Lexer.Token.StringLiteral.Type.HEX)(`x"` ~ hexText ~ `"`, value);
	}

	TestCase delimitedStringLiteral(dstring value, dstring delimiter) {
		return stringLiteral!(Lexer.Token.StringLiteral.Type.DELIMITED)(`q"` ~ delimiter ~ value ~ `"`, value);
	}

	TestCase tokenStringLiteral(dstring value) {
		return stringLiteral!(Lexer.Token.StringLiteral.Type.TOKEN)("q{" ~ value ~ "}", value);
	}

	TestCase characterLiteral(dstring code, dchar value) {
		auto expected = Lexer.Token(code, 0, Lexer.Token.CHARACTER_LITERAL);
		expected.characterLiteral.value = value;
		return new TestCase(code, [expected]);
	}

	TestCase integerLiteral(dstring code, BigInt value, bool hasLongSuffix, bool hasUnsignedSuffix) {
		auto expected = Lexer.Token(code, 0, Lexer.Token.CHARACTER_LITERAL);
		expected.integerLiteral.value = value;
		expected.integerLiteral.hasLongSuffix = hasLongSuffix;
		expected.integerLiteral.hasUnsignedSuffix = hasUnsignedSuffix;
		return new TestCase(code, [expected]);
	}

	TestCase floatLiteral(dstring code, BigInt mantissa, long exponent,
				Lexer.Token.FloatLiteral.TypeSuffix typeSuffix, bool hasImaginarySuffix
	) {
		auto expected = Lexer.Token(code, 0, Lexer.Token.CHARACTER_LITERAL);
		expected.floatLiteral.mantissa = mantissa;
		expected.floatLiteral.exponent = exponent;
		expected.floatLiteral.typeSuffix = typeSuffix;
		expected.floatLiteral.hasImaginarySuffix = hasImaginarySuffix;
		return new TestCase(code, [expected]);
	}

	TestCase keyword(dstring code) {
		return new TestCase(code, [Lexer.Token(code, 0, Lexer.Token.typeForStaticToken(to!string(code)))]);
	}

	TestCase operator(dstring code) {
		return new TestCase(code, [Lexer.Token(code, 0, Lexer.Token.typeForStaticToken(to!string(code)))]);
	}

	TestCase endOfFile(dstring code) {
		return new TestCase(code, [Lexer.Token(code, 0, Lexer.Token.END_OF_FILE)]);
	}

	// TODO: keywords, operators


	TestCase concatenateTwoTestCases(TestCase a, TestCase b) {
		foreach (expectedToken; b.expectedTokens) {
			expectedToken.position += a.input.length;
		}
		return new TestCase(a.input ~ b.input, a.expectedTokens ~ b.expectedTokens);
	}

	TestCase concatenateTestCases(TestCase[] testCases...) {
		assert(testCases.length > 0);
		if (testCases.length == 1) {
			return testCases[0];
		} else {
			return concatenateTwoTestCases(concatenateTestCases(testCases[0..$-1]), testCases[$-1]);
		}
	}

	TestCase[] generateAllCombinations(TestCase[] a, TestCase[] b) {
		TestCase[] result;
		foreach (testCaseA; a) {
			foreach (testCaseB; b) {
				result ~= concatenateTwoTestCases(testCaseA, testCaseB);
			}
		}
		return result;
	}

	uint test(string suiteName, TestCase[] testCases) {
		uint failedTestsCount = 0;
		foreach (testCase; testCases) {
			auto testCaseResult = testCase.run;
			if (testCaseResult.failed) {
				failedTestsCount++;
				writeln(testCaseResult);
			}
		}
		if (failedTestsCount == 0) {
			writeln(green(format("%s: all %d tests passed", suiteName, testCases.length)));
		}
		return failedTestsCount;
	}



	enum firstIdentifierChars = "_qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM";
	enum identifierChars = firstIdentifierChars ~ "01234567890";

}


static TestCase[] lineCommentWithAllPossibleLineBreaks(dstring text) {
	TestCase[] result;
	foreach (endOfLine; ["\u000D"d, "\u000A", "\u000D\u000A", "\u2028", "\u2029"]) {
		result ~= lineComment(text, endOfLine);
	}
	return result;
}


unittest {

	TestCase[] generateOneTwoAndThreeCharIdentifiers() {
		TestCase[] result;
		foreach (c1; firstIdentifierChars) {
			if (!c1.isLower) result ~= identifier([c1]);
			foreach (c2; identifierChars) {
				if (!c1.isLower || !c2.isLower) result ~= identifier([c1, c2]);
				foreach (c3; identifierChars) {
					if (!c1.isLower || !c2.isLower || !c3.isLower) result ~= identifier([c1, c2, c3]);
				}
			}
		}
		return result;
	}

	TestCase[] generateIdentifiersThatContainKeywords() {
		TestCase[] result;
		foreach (keyword; Lexer.keywords) {
			result ~= identifier(to!dstring("_" ~ keyword));
			result ~= identifier(to!dstring("a" ~ keyword));
			result ~= identifier(to!dstring("A" ~ keyword));
			result ~= identifier(to!dstring(keyword ~ "_"));
			result ~= identifier(to!dstring(keyword ~ "1"));
			result ~= identifier(to!dstring(keyword ~ "a"));
			result ~= identifier(to!dstring(keyword ~ "A"));
		}
		return result;
	}

	TestCase[] generateKeywords() {
		TestCase[] result;
		foreach (keywordCode; Lexer.keywords) {
			result ~= keyword(to!dstring(keywordCode));
		}
		return result;
	}


	TestCase[] casesOf(alias testCaseGenerator)(string[] cases) {
		TestCase[] result;
		foreach (code; cases) {
			result ~= testCaseGenerator(to!dstring(code));
		}
		return result;
	}

	auto whitespaces = casesOf!whitespace(["\u0020", "\u0009", "\u000B", "\u000C"]);

	auto endOfLines = casesOf!endOfLine(["\u000D", "\u000A", "\u000D\u000A", "\u2028", "\u2029"]);

	auto commonCommentTexts = [
		"comment",
		"comment with spaces",
		"comment with ŪŅİĆŌĐĒ symbols",
	];

	auto blockComments = casesOf!blockComment(commonCommentTexts ~ [
		"*",
		"**",
		"/*",
		"/**",
		"//*",
	]);
	auto lineComments = casesOf!lineCommentWithAllPossibleLineBreaks(commonCommentTexts ~ [
		"/",
		"//",
	]);
	auto nestingBlockComments = casesOf!nestingBlockComment(commonCommentTexts ~ [
		"+",
		"++",
		"/++/",
		"/+ comment +/",
		"qwe /+ rty +/ uio",
		"//+ comment +/",
		"qwe //+ rty +/ uio",
	]);

	auto specialTokenSequences = casesOf!specialTokenSequence([
		`#line 5\n`,
		`#line "filename" 5\n`,
	]);

	auto identifiers = casesOf!identifier([
		// basic rules
		"simpleIdentifier",
		"_01234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM",
		"Aa1",
		"_1",
		"_",

		// distinction from string literals ( r" x" q" q{ )
		"r",   // FIXME: will cause problems for generateAllCombinations
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

	auto keywords = casesOf!keyword([
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
		"uint", "ulong", "union", "unittest", "ushort", "version", "void",
		"volatile", "wchar", "while", "with", "__FILE__", "__MODULE__", "__LINE__",
		"__FUNCTION__", "__PRETTY_FUNCTION__", "__gshared", "__traits", "__vector",
		"__parameters",
	]);

	auto operators = casesOf!operator([
		",", "/", "/=", ".", "..", "...", "&", "&=", "&&", "|", "|=", "||", "-",
		"-=", "--", "+", "+=", "++", "<", "<=", "<<", "<<=", "<>", "<>=", ">", ">=",
		">>=", ">>>=", ">>", ">>>", "!", "!=", "!<>", "!<>=", "!<", "!<=", "!>",
		"!>=", "(", ")", "[", "]", "{", "}", "?", ",", ";", ":", "$", "=", "==",
		"*", "*=", "%", "%=", "^", "^=", "^^", "^^=", "~", "~=", "@", "=>"/*, "#"*/,   // WTF #??
	]);




	test("Whitespace", whitespaces);
	test("End of line", endOfLines);
	test("Block comments", blockComments);
	//test("Line comments", lineComments);
	//test("Nesting block comments", nestingBlockComments);
	//test("Special token sequences", specialTokenSequences);
	test("Identifiers", identifiers);
	//test("Wysiwyg string literals", wysiwygStringLiterals);
	//test("Alternate wysiwyg string literals", alternateWysiwygStringLiterals);
	//test("Double quoted string literals", doubleQuotedStringLiterals);
	//test("Hex string literals", hexStringLiterals);
	//test("Delimited string literals", delimitedStringLiterals);
	//test("Token string literals", tokenStringLiterals);
	//test("Character literals", characterLiterals);
	//test("Decimal integer literals", decimalIntegerLiterals);
	//test("Binary integer literals", binaryIntegerLiterals);
	//test("Hexadecimal integer literals", hexIntegerLiterals);
	//test("Decimal float literals", decimalFloatLiterals);
	//test("Hexadecimal float literals", hexFloatLiterals);
	//test("Imaginary float literals", imaginaryFloatLiterals);
	test("Keywords", keywords);
	test("Operators", operators);
}
