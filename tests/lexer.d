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

			string replaceUnprintable(dstring s) {
				string result;
				foreach (c; s) {
					result ~= (isPrintable(c) ? to!string(c) : format("#(%x)", c));
				}
				return result;
			}

			string tokenToString(Lexer.Token token) {
				string typeSpecific;
				//final switch (token.type) {
				//	case Lexer.Token.Type.STRING_LITERAL: typeSpecific = to!string(token.stringLiteral); break;
				//	case Lexer.Token.Type.CHARACTER_LITERAL: typeSpecific = to!string(token.characterLiteral); break;
				//	case Lexer.Token.Type.NUMBER_LITERAL: typeSpecific = to!string(token.numberLiteral); break;
				//	case Lexer.Token.Type.UNKNOWN: typeSpecific = to!string(token.unknown); break;
				//	case Lexer.Token.Type.IDENTIFIER: typeSpecific = to!string(token.identifier); break;
				//	case Lexer.Token.Type.KEYWORD: typeSpecific = to!string(token.keyword); break;
				//	case Lexer.Token.Type.OPERATOR: typeSpecific = to!string(token.operator); break;
				//	case Lexer.Token.Type.END_OF_FILE: typeSpecific = to!string(token.endOfFile); break;
				//}
				return format("%s(%d|%d:%d - %d|%d:%d <[%s]> %s)",
					token.type,
					token.position, 0, 0,
					token.position + token.asString.length, 0, 0,
					replaceUnprintable(token.asString),
					typeSpecific
				);
			}

			Lexer.Token[] getTokens_manualFront() {
				Lexer.Token[] result;
				auto lexer = new Lexer(input);
				do {
					result ~= lexer.nextToken;
				} while ((result.length < MAX_TOKENS_COUNT) && (result[$-1].type != Lexer.Token.Type.END_OF_FILE));
				return result[0..$-1];  // remove END_OF_FILE token  // FIXME
			}

			auto actualTokens = getTokens_manualFront();
			auto result = new Result;
			foreach (expected, actual; lockstep(expectedTokens, actualTokens)) {
				if (tokenToString(expected) == tokenToString(actual)) {
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
				return failed ? red("FAIL") ~ "\nq{\n" ~ to!string(input) ~ "\n}\n  " ~ report.join("\n  ") ~ "\n\n" : "";
			}
		}
	}

	//TestCase unknown(ubyte type)(dstring code, string message) {
	//	auto expected = Lexer.Token(code, 0, Lexer.Token.UNKNOWN);
	//	expected.unknown.triedToParseAsType = type;
	//	expected.unknown.errorMessage = message;
	//	return new TestCase(code, [expected]);
	//}

	TestCase whitespace(dstring code) {
		return new TestCase(code, []);
	}

	TestCase endOfLine(dstring code) {
		return new TestCase(code, []);
	}

	//TestCase comment(Lexer.Token.Comment.Type type)(dstring code, dstring value) {
	//	auto expected = Lexer.Token(code, 0, Lexer.Token.COMMENT);
	//	expected.comment.type = type;
	//	expected.comment.value = value;
	//	return new TestCase(code, [expected]);
	//}

	//TestCase blockComment(dstring text) {
	//	return comment!(Lexer.Token.Comment.Type.BLOCK)("/*" ~ text ~ "*/", text);
	//}

	//TestCase lineComment(dstring text, dstring endOfLine) {
	//	return comment!(Lexer.Token.Comment.Type.LINE)("//" ~ text ~ endOfLine, text);
	//}

	//TestCase nestingBlockComment(dstring text) {
	//	return comment!(Lexer.Token.Comment.Type.NESTING_BLOCK)("/+" ~ text ~ "+/", text);
	//}

	TestCase specialTokenSequence(dstring code) {
		return new TestCase(code, []);
	}

	TestCase identifier(dstring code, Lexer.Comment[] precedingComments = []) {
		return new TestCase(code, [Lexer.Token(code, 0, Lexer.Token.Type.IDENTIFIER, precedingComments)]);
	}

	TestCase stringLiteral(dstring code, dstring value, Lexer.Comment[] precedingComments = []) {
		auto expected = Lexer.Token(code, 0, Lexer.Token.Type.STRING_LITERAL, precedingComments);
		//expected.stringLiteral = value;
		return new TestCase(code, [expected]);
	}

	TestCase wysiwygStringLiteral(dstring value, Lexer.Comment[] precedingComments = []) {
		return stringLiteral(`r"` ~ value ~ `"`, value);
	}

	TestCase alternateWysiwygStringLiteral(dstring value, Lexer.Comment[] precedingComments = []) {
		return stringLiteral('`' ~ value ~ '`', value);
	}

	TestCase doubleQuotedStringLiteral(dstring code, dstring value, Lexer.Comment[] precedingComments = []) {
		return stringLiteral('"' ~ code ~ '"', value);
	}

	TestCase doubleQuotedStringLiteral(dstring code, Lexer.Comment[] precedingComments = []) {
		return doubleQuotedStringLiteral(code, code);
	}

	TestCase hexStringLiteral(dstring hexText, dstring value, Lexer.Comment[] precedingComments = []) {
		return stringLiteral(`x"` ~ hexText ~ `"`, value);
	}

	TestCase delimitedStringLiteral(dstring value, dstring openingDelimiter, dstring closingDelimiter, Lexer.Comment[] precedingComments = []) {
		return stringLiteral(`q"` ~ openingDelimiter ~ value ~ closingDelimiter ~ `"`, value);
	}

	TestCase tokenStringLiteral(dstring value, Lexer.Comment[] precedingComments = []) {
		return stringLiteral("q{" ~ value ~ "}", value);
	}

	TestCase characterLiteral(Lexer.Token.CharWidth charWidth = Lexer.Token.CharWidth.ONE_BYTE)(
					dstring code, dchar value, Lexer.Comment[] precedingComments = []
	) {
		auto expected = Lexer.Token(code, 0, Lexer.Token.Type.CHARACTER_LITERAL, precedingComments);
		expected.charWidth = charWidth;
		expected.characterLiteral = value;
		return new TestCase(code, [expected]);
	}

	TestCase numberLiteral(Lexer.Token.NumberLiteralType numberLiteralType = INT)(
				dstring code, BigInt mantissa, long exponent = 0, Lexer.Comment[] precedingComments = []
	) {
		auto expected = Lexer.Token(code, 0, Lexer.Token.Type.NUMBER_LITERAL, precedingComments);
		expected.numberLiteral.mantissa = mantissa;
		expected.numberLiteral.exponent = exponent;
		return new TestCase(code, [expected]);
	}

	TestCase keyword(dstring code, Lexer.Comment[] precedingComments = []) {
		auto expected = Lexer.Token(code, 0, Lexer.Token.Type.KEYWORD, precedingComments);
		expected.staticTokenId = Lexer.Token.staticTokenIdFor(to!string(code));
		return new TestCase(code, [expected]);
	}

	TestCase operator(dstring code, Lexer.Comment[] precedingComments = []) {
		auto expected = Lexer.Token(code, 0, Lexer.Token.Type.OPERATOR, precedingComments);
		expected.staticTokenId = Lexer.Token.staticTokenIdFor(to!string(code));
		return new TestCase(code, [expected]);
	}

	//TestCase endOfFile(dstring code) {
	//	return new TestCase(code, [Lexer.Token(code, 0, Lexer.Token.END_OF_FILE)]);
	//}

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
			writeln(green(format("%s: %3d tests passed", suiteName, testCases.length)));
		}
		return failedTestsCount;
	}



	enum firstIdentifierChars = "_qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM";
	enum identifierChars = firstIdentifierChars ~ "01234567890";

}






unittest {
	writeln("Lexer.Comment.sizeof: ", Lexer.Comment.sizeof);
	writeln("Lexer.Token.sizeof: ", Lexer.Token.sizeof);
	writeln("Lexer.Token2.sizeof: ", Lexer.Token2.sizeof);
}



//static TestCase[] lineCommentWithAllPossibleLineBreaks(dstring text) {
//	TestCase[] result;
//	foreach (endOfLine; ["\u000D"d, "\u000A", "\u000D\u000A", "\u2028", "\u2029"]) {
//		result ~= lineComment(text, endOfLine);
//	}
//	return result;
//}

static TestCase[] delimitedStringLiteralWithAllPossibleDelimiters(dstring text) {
	return [
		delimitedStringLiteral(text, "(", ")"),
		delimitedStringLiteral(text, "[", "]"),
		delimitedStringLiteral(text, "<", ">"),
		delimitedStringLiteral(text, "{", "}"),
	];
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

	auto lineBreaks = casesOf!endOfLine(["\u000D", "\u000A", "\u000D\u000A", "\u2028", "\u2029"]);

	auto commonCommentTexts = [
		"",
		"comment",
		"comment with spaces",
		"comment with ŪŅİĆŌĐĒ symbols",
	];

	//auto blockComments = casesOf!blockComment(commonCommentTexts ~ [
	//	"*",
	//	"**",
	//	"/*",
	//	"/**",
	//	"//*",
	//]);
	//auto lineComments = casesOf!lineCommentWithAllPossibleLineBreaks(commonCommentTexts ~ [
	//	"/",
	//	"//",
	//]);
	//auto nestingBlockComments = casesOf!nestingBlockComment(commonCommentTexts ~ [
	//	"+",
	//	"++",
	//	"/++/",
	//	"/+ comment +/",
	//	"qwe /+ rty +/ uio",
	//	"//+ comment +/",
	//	"qwe //+ rty +/ uio",
	//]);

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

	auto commonStringLiteralTexts = [
		"",
		"string literal",
		"string literal with ŪŅİĆŌĐĒ symbols",
	];

	// TODO: test string and char postfixes ('c', 'w', 'd')

	auto commonWysiwygStringLiteralTexts = commonStringLiteralTexts ~ [`\`];

	auto wysiwygStringLiterals = casesOf!wysiwygStringLiteral(commonWysiwygStringLiteralTexts);

	auto alternateWysiwygStringLiterals = casesOf!alternateWysiwygStringLiteral(commonWysiwygStringLiteralTexts);

	auto hexStringLiterals = [
		hexStringLiteral("", ""),
		hexStringLiteral("63", "c"),
		hexStringLiteral(" 63", "c"),
		hexStringLiteral("63 ", "c"),
		hexStringLiteral("63\u000D ", "c"),
		hexStringLiteral("63\u000A ", "c"),
		hexStringLiteral("63\u000D\u000A ", "c"),
		hexStringLiteral("63\u2028 ", "c"),
		hexStringLiteral("63\u2029 ", "c"),
		hexStringLiteral("636f6d6d656e74", "comment"),
		hexStringLiteral("63 6f 6d 6d 65 6e 74", "comment"),
		hexStringLiteral("63 6F 6D 6D 65 6E 74", "comment"),
	];

	auto delimitedStringLiterals = casesOf!delimitedStringLiteralWithAllPossibleDelimiters(commonWysiwygStringLiteralTexts);

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




	test("Whitespace                           ", whitespaces);
	test("End of line                          ", lineBreaks);
//test("Comments / Block                     ", blockComments);
//test("Comments / Line                      ", lineComments);
//test("Comments / Nesting block             ", nestingBlockComments);
//test("Special token sequences              ", specialTokenSequences);
	test("Identifiers                          ", identifiers);
	test("Literals / String / Wysiwyg          ", wysiwygStringLiterals);
	test("Literals / String / Alternate wysiwyg", alternateWysiwygStringLiterals);
//test("Literals / String / Double quoted    ", doubleQuotedStringLiterals);
	test("Literals / String / Hexadecimal      ", hexStringLiterals);
	test("Literals / String / Delimited        ", delimitedStringLiterals);
//test("Literals / String / Token            ", tokenStringLiterals);
//test("Literals / Character                 ", characterLiterals);
//test("Literals / Integer / Decimal         ", decimalIntegerLiterals);
//test("Literals / Integer / Binary          ", binaryIntegerLiterals);
//test("Literals / Integer / Hexadecimal     ", hexIntegerLiterals);
//test("Literals / Float / Decimal           ", decimalFloatLiterals);
//test("Literals / Float / Hexadecimal       ", hexFloatLiterals);
//test("Literals / Float / Imaginary         ", imaginaryFloatLiterals);
	test("Keywords                             ", keywords);
	test("Operators                            ", operators);
}
