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
	Coordinates currentCoordinates;

	this(dstring code) {
		this.code = code ~ 0;
	}

	immutable struct Coordinates {
		size_t position;
		size_t line;
		size_t column;
	}

	struct Comment {
		dstring code;
		Coordinates start;
		Coordinates end;
	}

	struct Token {
		dstring code;
		Coordinates start;
		Coordinates end;
		Comment[] comments;
		immutable uint id;

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
		Comment[] comments = lexCommentsAndSkipWhitespaceAndLineBreaks();
		auto startPosition = position;
		auto id = lexToken();
		// TODO: detect __EOF__?
		return Token(&this, startPosition, position, commentBlockId, id);
	}



	private struct LineNumerationChange {
		size_t startFromLine;
		size_t newLineNumber;
	}


	private Coordinates coordinates(size_t position) const {
		return Coordinates(0, 0); // TODO
	}


	private uint lexToken() {
		if (isIdentifierFirstChar(code[position])) {
			mixin(
				new CodeGenerator()
					.onStaticTokens!keywords(q{if (!isIdentifierChar(code[position])) return Token.idFor(`%s`);})
					.on!`r"`(q{skipToChar!('"'); position++; return Token.Type.STRING_LITERAL;})
					.on!`x"`(q{skipToChar!('"'); position++; return Token.Type.STRING_LITERAL;})
					.on!`q"`(q{skipToChar!('"'); position++; return Token.Type.STRING_LITERAL;})
					.on!"q{"(q{skipTokenStringLiteral; return Token.Type.STRING_LITERAL;})
					// TODO: delimited string literal
					.generateCode(q{})
			);
			skipCharsWhile!"isIdentifierChar(code[position])";
			return Token.Type.IDENTIFIER;
		} else {
			mixin(
				new CodeGenerator()
					.onStaticTokens!operators(q{return Token.idFor(`%s`);})
					.on!"`" (q{skipToChar!('`'); position++; return Token.Type.STRING_LITERAL;})
					.on!`"` (q{skipToChar!('"'); position++; return Token.Type.STRING_LITERAL;})
					.on!"'" (q{skipToChar!('\''); position++; return Token.Type.CHARACTER_LITERAL;})
					.on!"0" (q{return lexNumberLiteralThatStartsWithZero;})
					.onOneOfChars!"123456789"(q{return lexDecimalNumberLiteral;})
					.on!"."(
						new CodeGenerator()
							.onOneOfChars!"123456789"(q{return lexDecimalFloatThatStartsWithDot;})
							.generateCode(q{return Token.idFor(`.`);})
					)
					.onOneOfChars!"\x00\x1A"(q{return Token.Type.END_OF_FILE;})
					.generateCode(q{assert(0);})  // FIXME
			);
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
		}
		skipCharsWhile!"isAlpha(code[position])";
		return Token.Type.INTEGER_LITERAL;
	}

	private uint lexDecimalFloatThatStartsWithDot() {
		skipCharsWhile!"isDigit(code[position])";
		skipCharsWhile!"isAlpha(code[position])";
		return Token.Type.FLOAT_LITERAL;
	}

	private uint lexNumberLiteralThatStartsWithZero() {
		switch (code[position++]) {
			case 'b': skipCharsWhile!"(code[position] == '0') || (code[position] == '1')"; break;
			case 'x': skipCharsWhile!"isHexDigit(code[position])"; break;
			default:
				// error
		}
		skipCharsWhile!"isAlpha(code[position])";
		return Token.Type.INTEGER_LITERAL;
	}


	// returns commentBlockId
	private Comment[] lexCommentsAndSkipWhitespaceAndLineBreaks() {
		return [];  // TODO
	}


	private void skipCharsWhile(string contition)() {
		while ((position < code.length) && (mixin(contition))) {   // TODO: newlines
			position++;
		}
	}

	private void skipToChar(char terminator)() {
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


