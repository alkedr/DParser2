module lexer;

import std.bigint : BigInt;
import std.algorithm : countUntil, map, join;
import std.string : format;
import std.conv : to;
import std.stdio;


struct Lexer {
	struct Token {
		struct StringLiteral { dstring value; }
		struct CharacterLiteral { dchar value; }
		struct IntegerLiteral { BigInt value; }
		struct FloatLiteral { BigInt mantissa; long exponent; }

		dstring asString;
		ulong position;
		ubyte type;
		union {
			StringLiteral stringLiteral;
			CharacterLiteral characterLiteral;
			IntegerLiteral integerLiteral;
			FloatLiteral floatLiteral;
		}

		enum {
			UNKNOWN = 0,
			END_OF_FILE = 1,
			IDENTIFIER = 2,
			STRING_LITERAL = 3,
			CHARACTER_LITERAL = 4,
			INTEGER_LITERAL = 5,
			FLOAT_LITERAL = 6,
			__LAST_DYNAMIC_TOKEN = 6
		}
		template STATIC(string s) {
			static assert(staticTokens.countUntil(s) != -1);
			enum STATIC = __LAST_DYNAMIC_TOKEN + 1 + staticTokens.countUntil(s);
		}
	}

	dstring code;
	Token currentToken;

	this(dstring code) {
		this.code = code ~ 0;
		popFront();
	}

	private ulong position;
	private ulong[] lineBeginPositions = [ 0 ];

	Token front() { return currentToken; }
	Token moveFront() { return currentToken; }
	bool empty() { return currentToken.type == Token.END_OF_FILE; }

	void popFront() {
		skipWhitespaceLineBreaksAndComments();
		currentToken.position = position;
if(isIdentifierFirstChar(code[position])){do{if(code[position+0]=='\U0000005F'){if(code[position+1]=='\U0000005F'){if(code[position+2]=='\U00000044'){if(code[position+3]=='\U00000041'){if(code[position+4]=='\U00000054'){if(code[position+5]=='\U00000045'){if(code[position+6]=='\U0000005F'){if(code[position+7]=='\U0000005F'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`__DATE__`;position+=8;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U0000004C'){if(code[position+3]=='\U00000049'){if(code[position+4]=='\U0000004E'){if(code[position+5]=='\U00000045'){if(code[position+6]=='\U0000005F'){if(code[position+7]=='\U0000005F'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`__LINE__`;position+=8;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U00000070'){if(code[position+3]=='\U00000061'){if(code[position+4]=='\U00000072'){if(code[position+5]=='\U00000061'){if(code[position+6]=='\U0000006D'){if(code[position+7]=='\U00000065'){if(code[position+8]=='\U00000074'){if(code[position+9]=='\U00000065'){if(code[position+10]=='\U00000072'){if(code[position+11]=='\U00000073'){{if (!isIdentifierChar(code[position+12])){currentToken.type=Token.STATIC!`__parameters`;position+=12;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U00000050'){if(code[position+3]=='\U00000052'){if(code[position+4]=='\U00000045'){if(code[position+5]=='\U00000054'){if(code[position+6]=='\U00000054'){if(code[position+7]=='\U00000059'){if(code[position+8]=='\U0000005F'){if(code[position+9]=='\U00000046'){if(code[position+10]=='\U00000055'){if(code[position+11]=='\U0000004E'){if(code[position+12]=='\U00000043'){if(code[position+13]=='\U00000054'){if(code[position+14]=='\U00000049'){if(code[position+15]=='\U0000004F'){if(code[position+16]=='\U0000004E'){if(code[position+17]=='\U0000005F'){if(code[position+18]=='\U0000005F'){{if (!isIdentifierChar(code[position+19])){currentToken.type=Token.STATIC!`__PRETTY_FUNCTION__`;position+=19;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U00000054'){if(code[position+3]=='\U00000049'){if(code[position+4]=='\U0000004D'){if(code[position+5]=='\U00000045'){if(code[position+6]=='\U0000005F'){if(code[position+7]=='\U0000005F'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`__TIME__`;position+=8;break;}}}else {}}else if(code[position+6]=='\U00000053'){if(code[position+7]=='\U00000054'){if(code[position+8]=='\U00000041'){if(code[position+9]=='\U0000004D'){if(code[position+10]=='\U00000050'){if(code[position+11]=='\U0000005F'){if(code[position+12]=='\U0000005F'){{if (!isIdentifierChar(code[position+13])){currentToken.type=Token.STATIC!`__TIMESTAMP__`;position+=13;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U00000074'){if(code[position+3]=='\U00000072'){if(code[position+4]=='\U00000061'){if(code[position+5]=='\U00000069'){if(code[position+6]=='\U00000074'){if(code[position+7]=='\U00000073'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`__traits`;position+=8;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U00000045'){if(code[position+3]=='\U0000004F'){if(code[position+4]=='\U00000046'){if(code[position+5]=='\U0000005F'){if(code[position+6]=='\U0000005F'){{lexEofToken;break;}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U0000004D'){if(code[position+3]=='\U0000004F'){if(code[position+4]=='\U00000044'){if(code[position+5]=='\U00000055'){if(code[position+6]=='\U0000004C'){if(code[position+7]=='\U00000045'){if(code[position+8]=='\U0000005F'){if(code[position+9]=='\U0000005F'){{if (!isIdentifierChar(code[position+10])){currentToken.type=Token.STATIC!`__MODULE__`;position+=10;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U00000046'){if(code[position+3]=='\U00000049'){if(code[position+4]=='\U0000004C'){if(code[position+5]=='\U00000045'){if(code[position+6]=='\U0000005F'){if(code[position+7]=='\U0000005F'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`__FILE__`;position+=8;break;}}}else {}}else {}}else {}}else {}}else if(code[position+3]=='\U00000055'){if(code[position+4]=='\U0000004E'){if(code[position+5]=='\U00000043'){if(code[position+6]=='\U00000054'){if(code[position+7]=='\U00000049'){if(code[position+8]=='\U0000004F'){if(code[position+9]=='\U0000004E'){if(code[position+10]=='\U0000005F'){if(code[position+11]=='\U0000005F'){{if (!isIdentifierChar(code[position+12])){currentToken.type=Token.STATIC!`__FUNCTION__`;position+=12;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U00000076'){if(code[position+3]=='\U00000065'){if(code[position+4]=='\U00000063'){if(code[position+5]=='\U00000074'){if(code[position+6]=='\U0000006F'){if(code[position+7]=='\U00000072'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`__vector`;position+=8;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U00000056'){if(code[position+3]=='\U00000045'){if(code[position+4]=='\U0000004E'){if(code[position+5]=='\U00000044'){if(code[position+6]=='\U0000004F'){if(code[position+7]=='\U00000052'){if(code[position+8]=='\U0000005F'){if(code[position+9]=='\U0000005F'){{if (!isIdentifierChar(code[position+10])){currentToken.type=Token.STATIC!`__VENDOR__`;position+=10;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+4]=='\U00000052'){if(code[position+5]=='\U00000053'){if(code[position+6]=='\U00000049'){if(code[position+7]=='\U0000004F'){if(code[position+8]=='\U0000004E'){if(code[position+9]=='\U0000005F'){if(code[position+10]=='\U0000005F'){{if (!isIdentifierChar(code[position+11])){currentToken.type=Token.STATIC!`__VERSION__`;position+=11;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U00000067'){if(code[position+3]=='\U00000073'){if(code[position+4]=='\U00000068'){if(code[position+5]=='\U00000061'){if(code[position+6]=='\U00000072'){if(code[position+7]=='\U00000065'){if(code[position+8]=='\U00000064'){{if (!isIdentifierChar(code[position+9])){currentToken.type=Token.STATIC!`__gshared`;position+=9;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+0]=='\U00000061'){if(code[position+1]=='\U0000006C'){if(code[position+2]=='\U00000069'){if(code[position+3]=='\U00000061'){if(code[position+4]=='\U00000073'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`alias`;position+=5;break;}}}else {}}else if(code[position+3]=='\U00000067'){if(code[position+4]=='\U0000006E'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`align`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000075'){if(code[position+2]=='\U00000074'){if(code[position+3]=='\U0000006F'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`auto`;position+=4;break;}}}else {}}else {}}else if(code[position+1]=='\U00000062'){if(code[position+2]=='\U00000073'){if(code[position+3]=='\U00000074'){if(code[position+4]=='\U00000072'){if(code[position+5]=='\U00000061'){if(code[position+6]=='\U00000063'){if(code[position+7]=='\U00000074'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`abstract`;position+=8;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U00000073'){if(code[position+2]=='\U0000006D'){{if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`asm`;position+=3;break;}}}else if(code[position+2]=='\U00000073'){if(code[position+3]=='\U00000065'){if(code[position+4]=='\U00000072'){if(code[position+5]=='\U00000074'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`assert`;position+=6;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+0]=='\U00000062'){if(code[position+1]=='\U00000079'){if(code[position+2]=='\U00000074'){if(code[position+3]=='\U00000065'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`byte`;position+=4;break;}}}else {}}else {}}else if(code[position+1]=='\U00000072'){if(code[position+2]=='\U00000065'){if(code[position+3]=='\U00000061'){if(code[position+4]=='\U0000006B'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`break`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U0000006F'){if(code[position+2]=='\U00000064'){if(code[position+3]=='\U00000079'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`body`;position+=4;break;}}}else {}}else if(code[position+2]=='\U0000006F'){if(code[position+3]=='\U0000006C'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`bool`;position+=4;break;}}}else {}}else {}}else {}}else if(code[position+0]=='\U00000063'){if(code[position+1]=='\U00000064'){if(code[position+2]=='\U0000006F'){if(code[position+3]=='\U00000075'){if(code[position+4]=='\U00000062'){if(code[position+5]=='\U0000006C'){if(code[position+6]=='\U00000065'){{if (!isIdentifierChar(code[position+7])){currentToken.type=Token.STATIC!`cdouble`;position+=7;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U0000006C'){if(code[position+2]=='\U00000061'){if(code[position+3]=='\U00000073'){if(code[position+4]=='\U00000073'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`class`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000061'){if(code[position+2]=='\U00000074'){if(code[position+3]=='\U00000063'){if(code[position+4]=='\U00000068'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`catch`;position+=5;break;}}}else {}}else {}}else if(code[position+2]=='\U00000073'){if(code[position+3]=='\U00000074'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`cast`;position+=4;break;}}}else if(code[position+3]=='\U00000065'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`case`;position+=4;break;}}}else {}}else {}}else if(code[position+1]=='\U00000065'){if(code[position+2]=='\U0000006E'){if(code[position+3]=='\U00000074'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`cent`;position+=4;break;}}}else {}}else {}}else if(code[position+1]=='\U00000066'){if(code[position+2]=='\U0000006C'){if(code[position+3]=='\U0000006F'){if(code[position+4]=='\U00000061'){if(code[position+5]=='\U00000074'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`cfloat`;position+=6;break;}}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U00000072'){if(code[position+2]=='\U00000065'){if(code[position+3]=='\U00000061'){if(code[position+4]=='\U0000006C'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`creal`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U0000006F'){if(code[position+2]=='\U0000006E'){if(code[position+3]=='\U00000074'){if(code[position+4]=='\U00000069'){if(code[position+5]=='\U0000006E'){if(code[position+6]=='\U00000075'){if(code[position+7]=='\U00000065'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`continue`;position+=8;break;}}}else {}}else {}}else {}}else {}}else if(code[position+3]=='\U00000073'){if(code[position+4]=='\U00000074'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`const`;position+=5;break;}}}else {}}else {}}else {}}else {}}else if(code[position+0]=='\U00000064'){if(code[position+1]=='\U00000065'){if(code[position+2]=='\U0000006C'){if(code[position+3]=='\U00000065'){if(code[position+4]=='\U00000074'){if(code[position+5]=='\U00000065'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`delete`;position+=6;break;}}}else {}}else if(code[position+4]=='\U00000067'){if(code[position+5]=='\U00000061'){if(code[position+6]=='\U00000074'){if(code[position+7]=='\U00000065'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`delegate`;position+=8;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U00000070'){if(code[position+3]=='\U00000072'){if(code[position+4]=='\U00000065'){if(code[position+5]=='\U00000063'){if(code[position+6]=='\U00000061'){if(code[position+7]=='\U00000074'){if(code[position+8]=='\U00000065'){if(code[position+9]=='\U00000064'){{if (!isIdentifierChar(code[position+10])){currentToken.type=Token.STATIC!`deprecated`;position+=10;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U00000062'){if(code[position+3]=='\U00000075'){if(code[position+4]=='\U00000067'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`debug`;position+=5;break;}}}else {}}else {}}else if(code[position+2]=='\U00000066'){if(code[position+3]=='\U00000061'){if(code[position+4]=='\U00000075'){if(code[position+5]=='\U0000006C'){if(code[position+6]=='\U00000074'){{if (!isIdentifierChar(code[position+7])){currentToken.type=Token.STATIC!`default`;position+=7;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U00000063'){if(code[position+2]=='\U00000068'){if(code[position+3]=='\U00000061'){if(code[position+4]=='\U00000072'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`dchar`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U0000006F'){if(code[position+2]=='\U00000075'){if(code[position+3]=='\U00000062'){if(code[position+4]=='\U0000006C'){if(code[position+5]=='\U00000065'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`double`;position+=6;break;}}}else {}}else {}}else {}}else {if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`do`;position+=2;break;}}}else {}}else if(code[position+0]=='\U00000065'){if(code[position+1]=='\U0000006C'){if(code[position+2]=='\U00000073'){if(code[position+3]=='\U00000065'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`else`;position+=4;break;}}}else {}}else {}}else if(code[position+1]=='\U00000078'){if(code[position+2]=='\U00000070'){if(code[position+3]=='\U0000006F'){if(code[position+4]=='\U00000072'){if(code[position+5]=='\U00000074'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`export`;position+=6;break;}}}else {}}else {}}else {}}else if(code[position+2]=='\U00000074'){if(code[position+3]=='\U00000065'){if(code[position+4]=='\U00000072'){if(code[position+5]=='\U0000006E'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`extern`;position+=6;break;}}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U0000006E'){if(code[position+2]=='\U00000075'){if(code[position+3]=='\U0000006D'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`enum`;position+=4;break;}}}else {}}else {}}else {}}else if(code[position+0]=='\U00000066'){if(code[position+1]=='\U0000006C'){if(code[position+2]=='\U0000006F'){if(code[position+3]=='\U00000061'){if(code[position+4]=='\U00000074'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`float`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000061'){if(code[position+2]=='\U0000006C'){if(code[position+3]=='\U00000073'){if(code[position+4]=='\U00000065'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`false`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000069'){if(code[position+2]=='\U0000006E'){if(code[position+3]=='\U00000061'){if(code[position+4]=='\U0000006C'){if(code[position+5]=='\U0000006C'){if(code[position+6]=='\U00000079'){{if (!isIdentifierChar(code[position+7])){currentToken.type=Token.STATIC!`finally`;position+=7;break;}}}else {}}else {if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`final`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000075'){if(code[position+2]=='\U0000006E'){if(code[position+3]=='\U00000063'){if(code[position+4]=='\U00000074'){if(code[position+5]=='\U00000069'){if(code[position+6]=='\U0000006F'){if(code[position+7]=='\U0000006E'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`function`;position+=8;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U0000006F'){if(code[position+2]=='\U00000072'){if(code[position+3]=='\U00000065'){if(code[position+4]=='\U00000061'){if(code[position+5]=='\U00000063'){if(code[position+6]=='\U00000068'){if(code[position+7]=='\U0000005F'){if(code[position+8]=='\U00000072'){if(code[position+9]=='\U00000065'){if(code[position+10]=='\U00000076'){if(code[position+11]=='\U00000065'){if(code[position+12]=='\U00000072'){if(code[position+13]=='\U00000073'){if(code[position+14]=='\U00000065'){{if (!isIdentifierChar(code[position+15])){currentToken.type=Token.STATIC!`foreach_reverse`;position+=15;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {if (!isIdentifierChar(code[position+7])){currentToken.type=Token.STATIC!`foreach`;position+=7;break;}}}else {}}else {}}else {}}else {if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`for`;position+=3;break;}}}else {}}else {}}else if(code[position+0]=='\U00000067'){if(code[position+1]=='\U0000006F'){if(code[position+2]=='\U00000074'){if(code[position+3]=='\U0000006F'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`goto`;position+=4;break;}}}else {}}else {}}else {}}else if(code[position+0]=='\U00000069'){if(code[position+1]=='\U00000064'){if(code[position+2]=='\U0000006F'){if(code[position+3]=='\U00000075'){if(code[position+4]=='\U00000062'){if(code[position+5]=='\U0000006C'){if(code[position+6]=='\U00000065'){{if (!isIdentifierChar(code[position+7])){currentToken.type=Token.STATIC!`idouble`;position+=7;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U0000006D'){if(code[position+2]=='\U00000070'){if(code[position+3]=='\U0000006F'){if(code[position+4]=='\U00000072'){if(code[position+5]=='\U00000074'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`import`;position+=6;break;}}}else {}}else {}}else {}}else if(code[position+2]=='\U0000006D'){if(code[position+3]=='\U00000075'){if(code[position+4]=='\U00000074'){if(code[position+5]=='\U00000061'){if(code[position+6]=='\U00000062'){if(code[position+7]=='\U0000006C'){if(code[position+8]=='\U00000065'){{if (!isIdentifierChar(code[position+9])){currentToken.type=Token.STATIC!`immutable`;position+=9;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U00000066'){if(code[position+2]=='\U0000006C'){if(code[position+3]=='\U0000006F'){if(code[position+4]=='\U00000061'){if(code[position+5]=='\U00000074'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`ifloat`;position+=6;break;}}}else {}}else {}}else {}}else {if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`if`;position+=2;break;}}}else if(code[position+1]=='\U0000006E'){if(code[position+2]=='\U00000074'){if(code[position+3]=='\U00000065'){if(code[position+4]=='\U00000072'){if(code[position+5]=='\U00000066'){if(code[position+6]=='\U00000061'){if(code[position+7]=='\U00000063'){if(code[position+8]=='\U00000065'){{if (!isIdentifierChar(code[position+9])){currentToken.type=Token.STATIC!`interface`;position+=9;break;}}}else {}}else {}}else {}}else {}}else {}}else {if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`int`;position+=3;break;}}}else if(code[position+2]=='\U00000076'){if(code[position+3]=='\U00000061'){if(code[position+4]=='\U00000072'){if(code[position+5]=='\U00000069'){if(code[position+6]=='\U00000061'){if(code[position+7]=='\U0000006E'){if(code[position+8]=='\U00000074'){{if (!isIdentifierChar(code[position+9])){currentToken.type=Token.STATIC!`invariant`;position+=9;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U0000006F'){if(code[position+3]=='\U00000075'){if(code[position+4]=='\U00000074'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`inout`;position+=5;break;}}}else {}}else {}}else {if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`in`;position+=2;break;}}}else if(code[position+1]=='\U00000072'){if(code[position+2]=='\U00000065'){if(code[position+3]=='\U00000061'){if(code[position+4]=='\U0000006C'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`ireal`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000073'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`is`;position+=2;break;}}}else {}}else if(code[position+0]=='\U0000006C'){if(code[position+1]=='\U00000061'){if(code[position+2]=='\U0000007A'){if(code[position+3]=='\U00000079'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`lazy`;position+=4;break;}}}else {}}else {}}else if(code[position+1]=='\U0000006F'){if(code[position+2]=='\U0000006E'){if(code[position+3]=='\U00000067'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`long`;position+=4;break;}}}else {}}else {}}else {}}else if(code[position+0]=='\U0000006D'){if(code[position+1]=='\U00000061'){if(code[position+2]=='\U00000063'){if(code[position+3]=='\U00000072'){if(code[position+4]=='\U0000006F'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`macro`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000069'){if(code[position+2]=='\U00000078'){if(code[position+3]=='\U00000069'){if(code[position+4]=='\U0000006E'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`mixin`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U0000006F'){if(code[position+2]=='\U00000064'){if(code[position+3]=='\U00000075'){if(code[position+4]=='\U0000006C'){if(code[position+5]=='\U00000065'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`module`;position+=6;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+0]=='\U0000006E'){if(code[position+1]=='\U00000065'){if(code[position+2]=='\U00000077'){{if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`new`;position+=3;break;}}}else {}}else if(code[position+1]=='\U00000075'){if(code[position+2]=='\U0000006C'){if(code[position+3]=='\U0000006C'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`null`;position+=4;break;}}}else {}}else {}}else if(code[position+1]=='\U0000006F'){if(code[position+2]=='\U00000074'){if(code[position+3]=='\U00000068'){if(code[position+4]=='\U00000072'){if(code[position+5]=='\U0000006F'){if(code[position+6]=='\U00000077'){{if (!isIdentifierChar(code[position+7])){currentToken.type=Token.STATIC!`nothrow`;position+=7;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+0]=='\U0000006F'){if(code[position+1]=='\U00000075'){if(code[position+2]=='\U00000074'){{if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`out`;position+=3;break;}}}else {}}else if(code[position+1]=='\U00000076'){if(code[position+2]=='\U00000065'){if(code[position+3]=='\U00000072'){if(code[position+4]=='\U00000072'){if(code[position+5]=='\U00000069'){if(code[position+6]=='\U00000064'){if(code[position+7]=='\U00000065'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`override`;position+=8;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+0]=='\U00000070'){if(code[position+1]=='\U00000061'){if(code[position+2]=='\U00000063'){if(code[position+3]=='\U0000006B'){if(code[position+4]=='\U00000061'){if(code[position+5]=='\U00000067'){if(code[position+6]=='\U00000065'){{if (!isIdentifierChar(code[position+7])){currentToken.type=Token.STATIC!`package`;position+=7;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U00000075'){if(code[position+2]=='\U00000062'){if(code[position+3]=='\U0000006C'){if(code[position+4]=='\U00000069'){if(code[position+5]=='\U00000063'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`public`;position+=6;break;}}}else {}}else {}}else {}}else if(code[position+2]=='\U00000072'){if(code[position+3]=='\U00000065'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`pure`;position+=4;break;}}}else {}}else {}}else if(code[position+1]=='\U00000072'){if(code[position+2]=='\U00000061'){if(code[position+3]=='\U00000067'){if(code[position+4]=='\U0000006D'){if(code[position+5]=='\U00000061'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`pragma`;position+=6;break;}}}else {}}else {}}else {}}else if(code[position+2]=='\U00000069'){if(code[position+3]=='\U00000076'){if(code[position+4]=='\U00000061'){if(code[position+5]=='\U00000074'){if(code[position+6]=='\U00000065'){{if (!isIdentifierChar(code[position+7])){currentToken.type=Token.STATIC!`private`;position+=7;break;}}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U0000006F'){if(code[position+3]=='\U00000074'){if(code[position+4]=='\U00000065'){if(code[position+5]=='\U00000063'){if(code[position+6]=='\U00000074'){if(code[position+7]=='\U00000065'){if(code[position+8]=='\U00000064'){{if (!isIdentifierChar(code[position+9])){currentToken.type=Token.STATIC!`protected`;position+=9;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+0]=='\U00000072'){if(code[position+1]=='\U00000065'){if(code[position+2]=='\U00000074'){if(code[position+3]=='\U00000075'){if(code[position+4]=='\U00000072'){if(code[position+5]=='\U0000006E'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`return`;position+=6;break;}}}else {}}else {}}else {}}else if(code[position+2]=='\U00000061'){if(code[position+3]=='\U0000006C'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`real`;position+=4;break;}}}else {}}else if(code[position+2]=='\U00000066'){{if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`ref`;position+=3;break;}}}else {}}else {}}else if(code[position+0]=='\U00000073'){if(code[position+1]=='\U00000068'){if(code[position+2]=='\U00000061'){if(code[position+3]=='\U00000072'){if(code[position+4]=='\U00000065'){if(code[position+5]=='\U00000064'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`shared`;position+=6;break;}}}else {}}else {}}else {}}else if(code[position+2]=='\U0000006F'){if(code[position+3]=='\U00000072'){if(code[position+4]=='\U00000074'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`short`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000074'){if(code[position+2]=='\U00000061'){if(code[position+3]=='\U00000074'){if(code[position+4]=='\U00000069'){if(code[position+5]=='\U00000063'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`static`;position+=6;break;}}}else {}}else {}}else {}}else if(code[position+2]=='\U00000072'){if(code[position+3]=='\U00000075'){if(code[position+4]=='\U00000063'){if(code[position+5]=='\U00000074'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`struct`;position+=6;break;}}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U00000075'){if(code[position+2]=='\U00000070'){if(code[position+3]=='\U00000065'){if(code[position+4]=='\U00000072'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`super`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000079'){if(code[position+2]=='\U0000006E'){if(code[position+3]=='\U00000063'){if(code[position+4]=='\U00000068'){if(code[position+5]=='\U00000072'){if(code[position+6]=='\U0000006F'){if(code[position+7]=='\U0000006E'){if(code[position+8]=='\U00000069'){if(code[position+9]=='\U0000007A'){if(code[position+10]=='\U00000065'){if(code[position+11]=='\U00000064'){{if (!isIdentifierChar(code[position+12])){currentToken.type=Token.STATIC!`synchronized`;position+=12;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U00000063'){if(code[position+2]=='\U0000006F'){if(code[position+3]=='\U00000070'){if(code[position+4]=='\U00000065'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`scope`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000077'){if(code[position+2]=='\U00000069'){if(code[position+3]=='\U00000074'){if(code[position+4]=='\U00000063'){if(code[position+5]=='\U00000068'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`switch`;position+=6;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+0]=='\U00000074'){if(code[position+1]=='\U00000068'){if(code[position+2]=='\U00000069'){if(code[position+3]=='\U00000073'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`this`;position+=4;break;}}}else {}}else if(code[position+2]=='\U00000072'){if(code[position+3]=='\U0000006F'){if(code[position+4]=='\U00000077'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`throw`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000065'){if(code[position+2]=='\U0000006D'){if(code[position+3]=='\U00000070'){if(code[position+4]=='\U0000006C'){if(code[position+5]=='\U00000061'){if(code[position+6]=='\U00000074'){if(code[position+7]=='\U00000065'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`template`;position+=8;break;}}}else {}}else {}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U00000079'){if(code[position+2]=='\U00000070'){if(code[position+3]=='\U00000065'){if(code[position+4]=='\U00000064'){if(code[position+5]=='\U00000065'){if(code[position+6]=='\U00000066'){{if (!isIdentifierChar(code[position+7])){currentToken.type=Token.STATIC!`typedef`;position+=7;break;}}}else {}}else {}}else if(code[position+4]=='\U00000069'){if(code[position+5]=='\U00000064'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`typeid`;position+=6;break;}}}else {}}else if(code[position+4]=='\U0000006F'){if(code[position+5]=='\U00000066'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`typeof`;position+=6;break;}}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U00000072'){if(code[position+2]=='\U00000075'){if(code[position+3]=='\U00000065'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`true`;position+=4;break;}}}else {}}else if(code[position+2]=='\U00000079'){{if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`try`;position+=3;break;}}}else {}}else {}}else if(code[position+0]=='\U00000075'){if(code[position+1]=='\U0000006C'){if(code[position+2]=='\U0000006F'){if(code[position+3]=='\U0000006E'){if(code[position+4]=='\U00000067'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`ulong`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000069'){if(code[position+2]=='\U0000006E'){if(code[position+3]=='\U00000074'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`uint`;position+=4;break;}}}else {}}else {}}else if(code[position+1]=='\U00000062'){if(code[position+2]=='\U00000079'){if(code[position+3]=='\U00000074'){if(code[position+4]=='\U00000065'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`ubyte`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U0000006E'){if(code[position+2]=='\U00000069'){if(code[position+3]=='\U00000074'){if(code[position+4]=='\U00000074'){if(code[position+5]=='\U00000065'){if(code[position+6]=='\U00000073'){if(code[position+7]=='\U00000074'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`unittest`;position+=8;break;}}}else {}}else {}}else {}}else {}}else if(code[position+3]=='\U0000006F'){if(code[position+4]=='\U0000006E'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`union`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000063'){if(code[position+2]=='\U00000065'){if(code[position+3]=='\U0000006E'){if(code[position+4]=='\U00000074'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`ucent`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000073'){if(code[position+2]=='\U00000068'){if(code[position+3]=='\U0000006F'){if(code[position+4]=='\U00000072'){if(code[position+5]=='\U00000074'){{if (!isIdentifierChar(code[position+6])){currentToken.type=Token.STATIC!`ushort`;position+=6;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+0]=='\U00000076'){if(code[position+1]=='\U00000065'){if(code[position+2]=='\U00000072'){if(code[position+3]=='\U00000073'){if(code[position+4]=='\U00000069'){if(code[position+5]=='\U0000006F'){if(code[position+6]=='\U0000006E'){{if (!isIdentifierChar(code[position+7])){currentToken.type=Token.STATIC!`version`;position+=7;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U00000069'){if(code[position+2]=='\U00000072'){if(code[position+3]=='\U00000074'){if(code[position+4]=='\U00000075'){if(code[position+5]=='\U00000061'){if(code[position+6]=='\U0000006C'){{if (!isIdentifierChar(code[position+7])){currentToken.type=Token.STATIC!`virtual`;position+=7;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+1]=='\U0000006F'){if(code[position+2]=='\U0000006C'){if(code[position+3]=='\U00000061'){if(code[position+4]=='\U00000074'){if(code[position+5]=='\U00000069'){if(code[position+6]=='\U0000006C'){if(code[position+7]=='\U00000065'){{if (!isIdentifierChar(code[position+8])){currentToken.type=Token.STATIC!`volatile`;position+=8;break;}}}else {}}else {}}else {}}else {}}else {}}else if(code[position+2]=='\U00000069'){if(code[position+3]=='\U00000064'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`void`;position+=4;break;}}}else {}}else {}}else {}}else if(code[position+0]=='\U00000077'){if(code[position+1]=='\U00000068'){if(code[position+2]=='\U00000069'){if(code[position+3]=='\U0000006C'){if(code[position+4]=='\U00000065'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`while`;position+=5;break;}}}else {}}else {}}else {}}else if(code[position+1]=='\U00000069'){if(code[position+2]=='\U00000074'){if(code[position+3]=='\U00000068'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`with`;position+=4;break;}}}else {}}else {}}else if(code[position+1]=='\U00000063'){if(code[position+2]=='\U00000068'){if(code[position+3]=='\U00000061'){if(code[position+4]=='\U00000072'){{if (!isIdentifierChar(code[position+5])){currentToken.type=Token.STATIC!`wchar`;position+=5;break;}}}else {}}else {}}else {}}else {}}else {}lexIdentifier;}while(0);}else{do{if(code[position+0]=='\U00000000'){{lexEofToken;break;}}else if(code[position+0]=='\U0000003E'){if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`>=`;position+=2;break;}}}else if(code[position+1]=='\U0000003E'){if(code[position+2]=='\U0000003D'){{if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`>>=`;position+=3;break;}}}else if(code[position+2]=='\U0000003E'){if(code[position+3]=='\U0000003D'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`>>>=`;position+=4;break;}}}else {if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`>>>`;position+=3;break;}}}else {if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`>>`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`>`;position+=1;break;}}}else if(code[position+0]=='\U0000005D'){{if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`]`;position+=1;break;}}}else if(code[position+0]=='\U0000007C'){if(code[position+1]=='\U0000007C'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`||`;position+=2;break;}}}else if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`|=`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`|`;position+=1;break;}}}else if(code[position+0]=='\U0000003F'){{if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`?`;position+=1;break;}}}else if(code[position+0]=='\U0000005E'){if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`^=`;position+=2;break;}}}else if(code[position+1]=='\U0000005E'){if(code[position+2]=='\U0000003D'){{if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`^^=`;position+=3;break;}}}else {if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`^^`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`^`;position+=1;break;}}}else if(code[position+0]=='\U0000007D'){{if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`}`;position+=1;break;}}}else if(code[position+0]=='\U00000021'){if(code[position+1]=='\U0000003C'){if(code[position+2]=='\U0000003D'){{if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`!<=`;position+=3;break;}}}else if(code[position+2]=='\U0000003E'){if(code[position+3]=='\U0000003D'){{if (!isIdentifierChar(code[position+4])){currentToken.type=Token.STATIC!`!<>=`;position+=4;break;}}}else {if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`!<>`;position+=3;break;}}}else {if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`!<`;position+=2;break;}}}else if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`!=`;position+=2;break;}}}else if(code[position+1]=='\U0000003E'){if(code[position+2]=='\U0000003D'){{if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`!>=`;position+=3;break;}}}else {if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`!>`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`!`;position+=1;break;}}}else if(code[position+0]=='\U00000040'){{if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`@`;position+=1;break;}}}else if(code[position+0]=='\U0000007E'){if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`~=`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`~`;position+=1;break;}}}else if(code[position+0]=='\U00000024'){{if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`$`;position+=1;break;}}}else if(code[position+0]=='\U00000025'){if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`%=`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`%`;position+=1;break;}}}else if(code[position+0]=='\U00000026'){if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`&=`;position+=2;break;}}}else if(code[position+1]=='\U00000026'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`&&`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`&`;position+=1;break;}}}else if(code[position+0]=='\U00000028'){{if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`(`;position+=1;break;}}}else if(code[position+0]=='\U00000029'){{if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`)`;position+=1;break;}}}else if(code[position+0]=='\U0000002A'){if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`*=`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`*`;position+=1;break;}}}else if(code[position+0]=='\U0000002B'){if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`+=`;position+=2;break;}}}else if(code[position+1]=='\U0000002B'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`++`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`+`;position+=1;break;}}}else if(code[position+0]=='\U0000002C'){{if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`,`;position+=1;break;}}}else if(code[position+0]=='\U0000002D'){if(code[position+1]=='\U0000002D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`--`;position+=2;break;}}}else if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`-=`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`-`;position+=1;break;}}}else if(code[position+0]=='\U0000002E'){if(code[position+1]=='\U0000002E'){if(code[position+2]=='\U0000002E'){{if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`...`;position+=3;break;}}}else {if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`..`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`.`;position+=1;break;}}}else if(code[position+0]=='\U0000002F'){if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`/=`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`/`;position+=1;break;}}}else if(code[position+0]=='\U0000001A'){{lexEofToken;break;}}else if(code[position+0]=='\U0000003A'){{if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`:`;position+=1;break;}}}else if(code[position+0]=='\U0000003B'){{if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`;`;position+=1;break;}}}else if(code[position+0]=='\U0000003C'){if(code[position+1]=='\U0000003C'){if(code[position+2]=='\U0000003D'){{if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`<<=`;position+=3;break;}}}else {if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`<<`;position+=2;break;}}}else if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`<=`;position+=2;break;}}}else if(code[position+1]=='\U0000003E'){if(code[position+2]=='\U0000003D'){{if (!isIdentifierChar(code[position+3])){currentToken.type=Token.STATIC!`<>=`;position+=3;break;}}}else {if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`<>`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`<`;position+=1;break;}}}else if(code[position+0]=='\U0000005B'){{if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`[`;position+=1;break;}}}else if(code[position+0]=='\U0000003D'){if(code[position+1]=='\U0000003D'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`==`;position+=2;break;}}}else if(code[position+1]=='\U0000003E'){{if (!isIdentifierChar(code[position+2])){currentToken.type=Token.STATIC!`=>`;position+=2;break;}}}else {if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`=`;position+=1;break;}}}else if(code[position+0]=='\U0000007B'){{if (!isIdentifierChar(code[position+1])){currentToken.type=Token.STATIC!`{`;position+=1;break;}}}else {}lexUnknown;}while(0);}
		//mixin(generateLexerCode);
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


	unittest {
		writeln(generateLexerCode);
	}


	private static dstring generateLexerCode() {
		return generateCode(
			staticTokens,
			[
				["\u0000", "\u001A", "__EOF__"]: "lexEofToken;",
				//["`"]: "lexAlternateWysiwygStringLiteral;",
				//[`"`]: "lexDoubleQuotedStringLiteral;",
				//[`r"`]: "lexWysiwygStringLiteral;",
				//[`x"`]: "lexHexStringLiteral;",
				//[`q"`]: "lexDelimitedStringLiteral;",
				//["q{"]: "lexTokenStringLiteral;",
				//["'"]: "lexSingleQuotedCharacterLiteral;",
				//["0b", "0B"]: "lexBinaryNumberLiteral;",
				//["0x", "0X"]: "lexHexNumberLiteral;",
				//["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]: "lexDecimalNumberLiteral;",
				//["#"]: "lexSpecialTokenSequence;",
			]
		);
	}

	private struct Rule {
		dstring code;
		Rule[dchar] choices;

		void insert(dstring charSequence, dstring code) {
			if (charSequence.length == 0) {
				this.code = code;
			} else {
				if (charSequence[0] !in choices) choices[charSequence[0]] = Rule();
				choices[charSequence[0]].insert(charSequence[1..$], code);
			}
		}

		dstring generateCode(int depth = 0) {
			dstring result = "";
			foreach (key, value; choices) {
				result ~= to!dstring(format("if(code[position+%d]=='\\U%08X'){%s}else ",
						depth, key, choices[to!dchar(key)].generateCode(depth+1)));
			}
			return result ~ "{" ~ code ~ "}";
		}
	}

	private static dstring generateCode(dstring[] staticTokens, dstring[dstring[]] lexRules) {
		Rule identifiersAndKeywords;
		Rule operatorsAndLiterals;

		void add(dstring charSequence, dstring code) {
			if (isIdentifierFirstChar(charSequence[0])) {
				identifiersAndKeywords.insert(charSequence, code);
			} else {
				operatorsAndLiterals.insert(charSequence, code);
			}
		}

		foreach (charSequences, code; lexRules) {
			foreach (charSequence; charSequences) {
				add(charSequence, code ~ "break;");
			}
		}
		foreach (charSequence; staticTokens) {
			add(
				charSequence,
				to!dstring(format(
					"if (!isIdentifierChar(code[position+%d])){" ~
						"currentToken.type=Token.STATIC!`%s`;" ~
						"position+=%d;" ~
						"break;" ~
					"}",
					charSequence.length, charSequence, charSequence.length
				))
			);
		}

		return
			"if(isIdentifierFirstChar(code[position])){" ~
				"do{" ~
					identifiersAndKeywords.generateCode ~
					"lexIdentifier;" ~
				"}while(0);" ~
			"}else{" ~
				"do{" ~
					operatorsAndLiterals.generateCode ~
					"lexUnknown;" ~
				"}while(0);" ~
			"}";
	}

	private void lexEofToken() {
		currentToken.type = Token.END_OF_FILE;
	}

	private void lexIdentifier() {
		currentToken.type = Token.IDENTIFIER;
		do {
			position++;
		} while (isIdentifierChar(code[position]));
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

	private enum staticTokens = [
		",", ".", "..", "...", "/", "/=", "!", "!<", "!<=", "!<>", "!<>=", "!=",
		"!>", "!>=", "$", "%", "%=", "&", "&&", "&=", "(", ")", "*", "*=", "+", "++",
		"+=", "-", "--", "-=", ":", ";", "<", "<<", "<<=", "<=", "<>", "<>=", "=",
		"==", "=>", ">", ">=", ">>", ">>=", ">>>", ">>>=", "?", "@", "[", "]", "^",
		"^=", "^^", "^^=", "{", "|", "|=", "||", "}", "~", "~=",
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
		"__VENDOR__", "__VERSION__"
	];
}


unittest {
	writeln(Lexer.Token.FLOAT_LITERAL);
	writeln(Lexer.Token.STATIC!",");
	writeln(Lexer.Token.STATIC!"__VERSION__");
}


unittest {

	import std.range : lockstep;

	enum MAX_TOKENS_COUNT = 20;


	void test(dstring input, Lexer.Token[] expectedTokens) {

		static string red(string s) { return "\033[31m" ~ s ~ "\033[0m"; }
		static string green(string s) { return "\033[32m" ~ s ~ "\033[0m"; }
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
				return failed ? red("FAIL") ~ "\nq{\n" ~ to!string(input) ~ "\n}\n  " ~ report.join("\n  ") ~ "\n" : "";
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


		//writeln(check(getTokens_foreach));
		//writeln(check(getTokens_foreachWithCounter));
		writeln(check(getTokens_manualFront));
		//writeln(check(getTokens_manualMoveFront));
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
}
