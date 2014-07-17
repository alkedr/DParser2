module lexer2;

import std.bigint : BigInt;
import std.algorithm : countUntil, map, join;
import std.string : format;
import std.conv : to;
import std.ascii;
import std.stdio;



struct Lexer {
	dstring code;


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
			if (text.length == 0) return false;
			if (type == Type.BLOCK) return text[0] == '*';
			if (type == Type.LINE) return text[0] == '/';
			if (type == Type.NESTING_BLOCK) return text[0] == '+';
			return false;
		}

		dstring text() const {
			uint startOffset = isDdoc ? 3 : 2;
			uint endOffset = type == Type.LINE ? 1 : 2;
			return codeSlice[startOffset .. $-endOffset];
		}
	}


	struct Token {
		dstring codeSlice;
		size_t firstPrecedingCommentPosition;
		size_t position;
		union {
			struct {
				Type type;
				ubyte staticTokenId;
			}
			ushort id;
		}
		// to distinguish between integer and float literals
		// if containsDot and suffixes conflict, containsDot wins
		private bool containsDot;


		enum Type : ubyte {
			END_OF_FILE,        // codeSlice.length == 0
			IDENTIFIER,         // first char is a-zA-Z_ and staticTokenId == 0
			STRING_LITERAL,     // first char is ` " or first chars are r" x" q" q{
			CHARACTER_LITERAL,  // first char is '
			NUMBER_LITERAL,     // (first char is a number) or (first char is . and second char is a number)
			KEYWORD,            // first char is a-z and staticTokenId > 0
			OPERATOR,           // first char is not a-z and staticTokenId > 0
		}

		// Possible number literal suffixes:
		//  - L - long or real
		//  - u U  - unsigned int
		//  - Lu LU uL UL  - unsigned long
		//  - f F  - float
		//  - i - imaginary
		//  - fi Fi  - float imaginary
		//  - Li  - real imaginary

		auto x = 2.4U;

		enum NumberLiteralType : ubyte {
			UNKNOWN,         // type is not NUMBER_LITERAL
			INT,             // doesn't contain dot and suffixes f F L i
			LONG,            // doesn't contain dot and suffixes f F L i, contains suffix
			UNSIGNED,
			LONG_UNSINGED,
			FLOAT,
			REAL,
			IMAGINARY,
			FLOAT_IMAGINARY,
			REAL_IMAGINARY,
		}

		Comment[] comments() {
			assert(0);
		}

	}


}


unittest {
	writeln("Lexer.Comment.sizeof: ", Lexer.Comment.sizeof);
	writeln("Lexer.Token.sizeof: ", Lexer.Token.sizeof);
}
