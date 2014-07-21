module lexer2;

import std.bigint : BigInt;
import std.algorithm;
import std.string : format;
import std.conv : to;
import std.ascii;
import std.stdio;
import std.range;
import std.array;
import std.bitmanip;



struct Lexer {
	dstring code;
	size_t position;

	this(dstring code) {
		this.code = code ~ 0;
	}

	struct Coordinates {
		size_t line;
		size_t column;
	}

	struct Comment {
		dstring codeSlice;
		size_t position;

		enum Type : ubyte {
			UNKNOWN,
			BLOCK,
			LINE,
			NESTING_BLOCK,
		}

		Type type() const {
			if (codeSlice.length < 2) return Type.UNKNOWN;
			switch (codeSlice[1]) {
				case '*': return Type.BLOCK;
				case '/': return Type.LINE;
				case '+': return Type.NESTING_BLOCK;
				default: return Type.UNKNOWN;
			}
		}

		bool isDdoc() const {
			final switch (type) {
				case Type.UNKNOWN: return false;
				case Type.BLOCK: return codeSlice[2] == '*';
				case Type.LINE: return codeSlice[2] == '/';
				case Type.NESTING_BLOCK: return codeSlice[2] == '+';
			}
		}

		dstring text() const {
			return codeSlice[(isDdoc ? 3 : 2) .. $-(type == Type.LINE ? 0 : 2)];
		}
	}


	struct Token {
		dstring codeSlice;
		size_t position;
		mixin(bitfields!(
			ubyte, "id", 8,           // either enum Type or idFor("something")
			bool, "containsDot", 1,   // for numberLiteralType
			size_t, "firstPrecedingCommentPosition", 55    // 2^55 should be enough for everyone
		));

		this(ubyte id, bool containsDot = false) {
			this.id = id;
			this.containsDot = containsDot;
		}


		enum Type : ubyte {
			END_OF_FILE,
			IDENTIFIER,
			STRING_LITERAL,
			CHARACTER_LITERAL,
			NUMBER_LITERAL,
		}

		static ubyte idFor(string staticToken) {
			assert(staticTokens.countUntil(staticToken) != -1);
			auto result = staticTokens.countUntil(staticToken) + Type.max + 1;
			assert(result < 256);
			return cast(ubyte)result;
		}


		enum NumberLiteralType : ubyte {
			UNKNOWN,
			INT,
			LONG,
			UNSIGNED,
			LONG_UNSINGED,
			FLOAT,
			REAL,
			IMAGINARY,
			FLOAT_IMAGINARY,
			REAL_IMAGINARY,

			UNKNOWN_SUFFIX,
			ILLEGAL_SUFFIX,
		}

		NumberLiteralType numberLiteralType() {
			// Possible number literal suffixes:
			//  - L - long or real
			//  - u U  - unsigned int
			//  - Lu LU uL UL  - unsigned long
			//  - f F  - float
			//  - i - imaginary
			//  - fi Fi  - float imaginary
			//  - Li  - real imaginary
			if (id != Type.NUMBER_LITERAL) return NumberLiteralType.UNKNOWN;
			bool isLongOrReal = false;
			bool isUnsigned = false;
			bool isFloat = false;
			bool isImaginary = false;
			foreach_reverse (c; codeSlice) {
				switch (c) {
					case 'L': isLongOrReal = true; break;
					case 'i':  isImaginary = true; break;
					case 'u': case 'U': isUnsigned = true; break;
					case 'f': case 'F': isFloat    = true; break;
					case '0': .. case '9':
						if (isUnsigned && (containsDot || isFloat || isImaginary)) return NumberLiteralType.ILLEGAL_SUFFIX;
						if (isLongOrReal && isFloat) return NumberLiteralType.ILLEGAL_SUFFIX;
						if (isImaginary) {
							if (isLongOrReal) return isLongOrReal ? NumberLiteralType.REAL_IMAGINARY : NumberLiteralType.FLOAT_IMAGINARY;
						} else {
							if (containsDot) {
								return isLongOrReal ? NumberLiteralType.REAL : NumberLiteralType.FLOAT;
							} else {
								if (isUnsigned) {
									return isLongOrReal ? NumberLiteralType.LONG_UNSINGED : NumberLiteralType.UNSIGNED;
								} else {
									return isLongOrReal ? NumberLiteralType.LONG : NumberLiteralType.INT;
								}
							}
						}
						return NumberLiteralType.ILLEGAL_SUFFIX;
					default:
						return NumberLiteralType.UNKNOWN_SUFFIX;
				}
			}
			return NumberLiteralType.UNKNOWN_SUFFIX;
		}


		enum CharWidth : ubyte {
			UNKNOWN,
			ONE_BYTE,
			TWO_BYTES,
			FOUR_BYTES,
		}

		CharWidth charWidth() {
			assert(0);
		}
	}


	Token nextToken() {
		auto firstPrecedingCommentPosition = lexCommentsAndSkipWhitespaceAndLineBreaks();
		ulong startPosition = position;
		Token result = lexToken();
		result.position = startPosition;
		result.firstPrecedingCommentPosition = firstPrecedingCommentPosition;
		result.codeSlice = code[startPosition .. position];
		return result;
	}


	Comment[] commentsPrecedingToken(Token token) {
		return []; // TODO
	}

	Coordinates coordinates(Token token) {
		return Coordinates(0, 0); // TODO
	}


	unittest {
		//writeln(generateStaticTokensLexingCode(["a", "ab", "ba", "ac"], "found %s;"));
		//char c = 'a';
		//string code = "ac" ~ 0;
		//uint position;
		//switch (code[position]) {
		//	mixin(generateStaticTokensLexingCode(["a", "b", "ab", "ba", "ac"], "writeln(`%s`);"));
		//	default:
		//}
	}


	private Token lexToken() {
		//writeln(__FUNCTION__, " ", code, " ", position);
		if (isIdentifierFirstChar(code[position])) {
			mixin(
				new CodeGenerator()  // TODO: generate cases
					.onStaticTokens!keywords(q{return lexKeywordOrIdentifier(Token.idFor(`%s`));})
					.on!`r"`(q{skipToChar!('"'); position++; return Token(Token.Type.STRING_LITERAL);})
					.on!`x"`(q{skipToChar!('"'); position++; return Token(Token.Type.STRING_LITERAL);})
					.on!`q"`(q{skipToChar!('"'); position++; return Token(Token.Type.STRING_LITERAL);})
					.on!"q{"(q{return lexTokenStringLiteral;})
					.on!"__EOF__"(q{return Token(Token.Type.END_OF_FILE);})
					.generateCode(q{})
			);
			return lexIdentifier;
		} else {
			mixin(
				new CodeGenerator()
					.onStaticTokens!operators(q{return Token(Token.idFor(`%s`));})
					.on!"`" (q{skipToChar!('`'); position++; return Token(Token.Type.STRING_LITERAL);})
					.on!`"` (q{skipToChar!('"'); position++; return Token(Token.Type.STRING_LITERAL);})
					.on!"'" (q{skipToChar!('\''); position++; return Token(Token.Type.CHARACTER_LITERAL);})
					.on!"0" (q{return lexNumberLiteralThatStartsWithZero;})
					.onOneOfChars!"123456789"(q{return lexDecimalNumberLiteral;})
					.on!"."(
						new CodeGenerator()
							.onOneOfChars!"123456789"(q{return lexDecimalFloatThatStartsWithDot;})
							.generateCode(q{return Token(Token.idFor(`.`));})
					)
					.onOneOfChars!"\x00\x1A"(q{return Token(Token.Type.END_OF_FILE);})
					.generateCode(q{return lexIdentifier;})  // FIXME: not lexIdentifier
			);
		}
	}

	Token lexKeywordOrIdentifier(ubyte id) {
		//writeln(__FUNCTION__, " ", id, " ", position, " ", code);
		if (isIdentifierChar(code[position])) {
			return lexIdentifier;
		} else {
			return Token(id);
		}
	}

	Token lexIdentifier() {
		skipCharsWhile!"isIdentifierChar(code[position])";
		return Token(Token.Type.IDENTIFIER);
	}

	Token lexTokenStringLiteral() {
		uint depth = 1;
		while (depth > 0) {
			auto token = nextToken;
			if (token.id == Token.idFor("{")) depth++;
			if (token.id == Token.idFor("}")) depth--;
		}
		return Token(Token.Type.STRING_LITERAL);
	}

	Token lexDecimalNumberLiteral() {
		bool containsDot = false;
		skipCharsWhile!"isDigit(code[position])";
		if ((code[position] == '.') && (isDigit(code[position+1]))) {
			containsDot = true;
			position++;
			skipCharsWhile!"isDigit(code[position])";
		}
		skipCharsWhile!"isAlpha(code[position])";
		return Token(Token.Type.NUMBER_LITERAL, containsDot);
	}

	Token lexDecimalFloatThatStartsWithDot() {
		skipCharsWhile!"isDigit(code[position])";
		skipCharsWhile!"isAlpha(code[position])";
		return Token(Token.Type.NUMBER_LITERAL, true);
	}

	Token lexNumberLiteralThatStartsWithZero() {
		switch (code[position++]) {
			case 'b': skipCharsWhile!"(code[position] == '0') || (code[position] == '1')"; break;
			case 'x': skipCharsWhile!"isHexDigit(code[position])"; break;
			default:
				// error
		}
		skipCharsWhile!"isAlpha(code[position])";
		return Token(Token.Type.NUMBER_LITERAL, false);
	}


	// returns position of first encountered comment
	size_t lexCommentsAndSkipWhitespaceAndLineBreaks() {
		return 0;  // TODO
	}


	void skipCharsWhile(string contition)() {
		while ((position < code.length) && (mixin(contition))) {
			position++;
		}
	}

	void skipToChar(char terminator)() {
		skipCharsWhile!(format(`code[position] != '\x%2x'`, terminator));
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



//unittest {
//	string[] tokens = Lexer.keywords;

//	foreach (tokenString; tokens) {
//		auto lexer = Lexer(to!dstring(tokenString));
//		auto id = lexer.nextToken.id;
//		writeln(tokenString, " - ", id);
//		assert(id == Lexer.Token.idFor(tokenString));
//	}
//}
