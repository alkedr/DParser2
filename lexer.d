module lexer;

import std.bigint : BigInt;
import std.algorithm : countUntil, map, join;
import std.string : format;
import std.conv : to;
import std.ascii;
import std.stdio;


struct Lexer {


	struct Token2 {
		dstring asString;
		ulong position;
		private ushort typeAndSubtype;
		private Literal literal;

		Type type() {
			return cast(Type)(typeAndSubtype & TYPE_MASK);
		};

		/// same as type() but each keyword and operator has it's own code
		uint code() {
			return typeAndSubtype & CODE_MASK;
		}




		//private static string generateLiteralPropertiesCode(Type type, string name) {
		//	return format("
		//		@property decltype(literal.%1$s) %1$s()
		//		in { assert(type == Type.%2$s); }
		//		body { return literal.%1$s; }

		//		@property decltype(literal.%1$s) %1$s(decltype(literal.%1$s) newValue)
		//		in { assert(type == Type.%2$s); }
		//		body { return literal.%1$s = newValue; }",
		//		name, type);
		//}

		//private static string generateTypeBitfieldEnumPropertiesCode(E)(Type type, string name, uint offset, uint width) {
		//	return format("
		//		@property %1$s %2$s()
		//		in { assert(type == Type.%5$s); }
		//		body { return cast(%1$s)(typeAndSubtype & %3$s); }

		//		@property %1$s %2$s(%1$s newValue)
		//		in { assert(type == Type.%5$s); }
		//		body {
		//			auto result = %2$s;
		//			typeAndSubtype &= ~%3$s;
		//			typeAndSubtype |= (newValue << %4$s);
		//			return result;
		//		}",
		//		E.stringof, name, ((2 ^^ width) - 1) << offset, offset, type);
		//}

		//private static string generatePropertiesCode(Type type, string[] code) {
		//	return code.map!(s => format(s, type)).join;
		//}

		//private static string bitfieldEnum(E)(string name, uint offset, uint width) {
		//	return format("
		//		@property %1$s %2$s()
		//		in { assert(type == Type.%5$s); }
		//		body { return cast(%1$s)(typeAndSubtype & %3$s); }

		//		@property %1$s %2$s(%1$s newValue)
		//		in { assert(type == Type.%5$s); }
		//		body {
		//			auto result = %2$s;
		//			typeAndSubtype &= ~%3$s;
		//			typeAndSubtype |= (newValue << %4$s);
		//			return result;
		//		}",
		//		E.stringof, name, ((2 ^^ width) - 1) << offset, offset, "%1$s"
		//	);
		//}

		//private static string literalValue(string name) {
		//	return format("
		//		@property typeof(literal.%1$s) %1$s()
		//		in { assert(type == Type.%2$s); }
		//		body { return literal.%1$s; }

		//		@property typeof(literal.%1$s) %1$s(typeof(literal.%1$s) newValue)
		//		in { assert(type == Type.%2$s); }
		//		body { return literal.%1$s = newValue; }",
		//		name, "%1$s"
		//	);
		//}

		//mixin(generatePropertiesCode(Type.STRING_LITERAL, [
		//	bitfieldEnum!StringLiteralType("stringLiteralType", 8, 8),
		//	bitfieldEnum!StringLiteralType("stringLiteralCharWidth", 16, 8),
		//	literalValue("stringValue"),
		//]));

		//mixin(generatePropertiesCode(Type.CHAR_LITERAL, [
		//	bitfieldEnum!StringLiteralType("characterLiteralCharWidth", 16, 8),
		//	literalValue("stringValue"),
		//]));

		//mixin(generateTypeBitfieldEnumPropertiesCode!StringLiteralType(Type.STRING_LITERAL, "stringLiteralType", 8, 8));
		//mixin(generateTypeBitfieldEnumPropertiesCode!CharWidth(Type.STRING_LITERAL, "stringLiteralCharWidth", 8, 8));
		//mixin(generateLiteralPropertiesCode(Type.STRING_LITERAL, "stringLiteralCharWidth", 8, 8));
		//mixin(generateTypeBitfieldEnumPropertiesCode!StringLiteralType("stringLiteralType", 8, 8));
		//mixin(generateTypeBitfieldEnumPropertiesCode!StringLiteralType("stringLiteralType", 8, 8));
		//mixin(generateTypeBitfieldEnumPropertiesCode!StringLiteralType("stringLiteralType", 8, 8));


		//@property StringLiteralType stringLiteralType() {
		//	return subType!StringLiteralType;
		//}
		//@property StringLiteralType stringLiteralType(StringLiteralType newValue) {
		//	return subType!StringLiteralType(newValue);
		//}
		//@property CharWidth stringLiteralCharWidth() { return charWidth; }
		//@property dstring stringLiteralValue() { return literal.stringValue;}

		CharWidth charLiteralCharWidth() { return charWidth; }

		IntegerLiteralType integerLiteralType() { return subType!IntegerLiteralType; }
		bool integerLiteralIsLong() {
			return (integerLiteralType == IntegerLiteralType.LONG)
			    || (integerLiteralType == IntegerLiteralType.LONG_UNSINGED);
		}
		bool integerLiteralIsUnsigned() {
			return (integerLiteralType == IntegerLiteralType.UNSIGNED)
			    || (integerLiteralType == IntegerLiteralType.LONG_UNSINGED);
		}

		FloatLiteralType floatLiteralType() {
			return cast(FloatLiteralType)(typeAndSubtype & SUBTYPE_MASK);
		}

		union {
			struct {
				Type type;
				ubyte staticTokenId;
			}
			ushort code;
		}
		union {
			StringLiteralType stringLiteralType;
			IntegerLiteralType stringLiteralType;
			FloatLiteralType stringLiteralType;
			CharWidth charWidth;
		}
		union {
			dstring stringLiteralValue;
			dchar characterLiteralValue;
			BigInt integerLiteralValue;
			struct {
				BigInt floatLiteralMantissa;
				long floatLiteralExponent;
			}
		};


		private enum TYPE_MASK       = 0x0000F;
		private enum CODE_MASK       = 0x00FFF;
		private enum SUBTYPE_MASK    = 0x0F000;
		private enum CHAR_WIDTH_MASK = 0xF0000;

		private T subType(T)() {
			return cast(T)(typeAndSubtype & SUBTYPE_MASK);
		};

		private CharWidth charWidth() {
			return cast(CharWidth)(typeAndSubtype & CHAR_WIDTH_MASK);
		};

		enum Type : ubyte {
			UNKNOWN,
			IDENTIFIER,
			STRING_LITERAL,
			CHARACTER_LITERAL,
			INTEGER_LITERAL,
			FLOAT_LITERAL,
			KEYWORD,
			OPERATOR,
		}

		enum StringLiteralType {
			UNKNOWN,
			WYSIWYG,
			ALTERNATE_WYSIWYG,
			DOUBLE_QUOTED,
			HEX,
			DELIMITED,
			TOKEN,
		}

		enum CharWidth : ubyte {
			UNKNOWN,
			ONE_BYTE,
			TWO_BYTES,
			FOUR_BYTES,
		}

		enum IntegerLiteralType : ubyte {
			UNKNOWN,
			INT,
			LONG,
			UNSIGNED,
			LONG_UNSINGED,
		}

		enum FloatLiteralType : ubyte {
			UNKNOWN,
			FLOAT,
			REAL,
			IMAGINARY,
			FLOAT_IMAGINARY,
			REAL_IMAGINARY,
		}

		struct StringLiteral {
			enum Type {
				UNKNOWN,
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
				UNKNOWN,
				NONE,
				FLOAT,
				REAL,
			}
			BigInt mantissa;
			long exponent;
			TypeSuffix typeSuffix;
			bool hasImaginarySuffix;
		}

	}






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
		//writeln(code);
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
		currentToken.comment.type = Token.Comment.Type.BLOCK;
		while ((code[position] != '*') || (code[position+1] != '/')) {
			position++;
		}
		currentToken.comment.value = code[currentToken.position+2 .. position];
		position += 2;
	}

	private void lexLineComment() {
		currentToken.type = Token.COMMENT;
		currentToken.comment.type = Token.Comment.Type.LINE;
		while (true) {
			switch (code[position++]) {
				case '\u000D':
					currentToken.comment.value = code[currentToken.position+2 .. position-1];
					if (code[position] == '\u000A') position++;
					return;
				case '\u000A':
				case '\u2028':
				case '\u2029':
					currentToken.comment.value = code[currentToken.position+2 .. position-1];
					return;
				default:
			}
		};
	}

	private void lexNestingBlockComment() {
		currentToken.type = Token.COMMENT;
		//currentToken.comment.type = Token.Comment.Type.NESTING_BLOCK;
		int depth = 1;
		while (depth > 0) {
			if ((code[position] == '/') && (code[position+1] == '+')) {
				depth++;
				position += 2;
			} else if ((code[position] == '+') && (code[position+1] == '/')) {
				depth--;
				position += 2;
			} else {
				position++;
			}
		}
		currentToken.comment.value = code[currentToken.position+2 .. position-2];
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
		currentToken.stringLiteral.type = Token.StringLiteral.Type.WYSIWYG;
		while (code[position] != '"') position++;
		currentToken.stringLiteral.value = code[currentToken.position+2 .. position];
		position++;
		lexOptionalStringLiteralPostfix;
	}

	private void lexAlternateWysiwygStringLiteral() {
		currentToken.type = Token.STRING_LITERAL;
		currentToken.stringLiteral.type = Token.StringLiteral.Type.ALTERNATE_WYSIWYG;
		while (code[position] != '`') position++;
		currentToken.stringLiteral.value = code[currentToken.position+1 .. position];
		position++;
		lexOptionalStringLiteralPostfix;
	}

	private void lexDoubleQuotedStringLiteral() {
		currentToken.type = Token.STRING_LITERAL;
		currentToken.stringLiteral.type = Token.StringLiteral.Type.DOUBLE_QUOTED;
		assert(0);
		lexOptionalStringLiteralPostfix;
	}

	private void lexHexStringLiteral() {
		ubyte hexDigitToInt(dchar c) {
			if (isDigit(code[position])) return cast(ubyte)(c - '0');
			if (isLower(code[position])) return cast(ubyte)(c - 'a' + 10);
			if (isUpper(code[position])) return cast(ubyte)(c - 'A' + 10);
			throw new Exception("hexDigitToInt");
		}

		currentToken.type = Token.STRING_LITERAL;
		currentToken.stringLiteral.type = Token.StringLiteral.Type.HEX;
		bool hexCharIsRemembered = false;
		ushort c = 0;
		while (code[position] != '"') {
			if (isHexDigit(code[position])) {
				if (hexCharIsRemembered) {
					currentToken.stringLiteral.value ~= ((c << 4) | hexDigitToInt(code[position]));
					c = 0;
					hexCharIsRemembered = false;
				} else {
					c = cast(ushort)(hexDigitToInt(code[position]));
					hexCharIsRemembered = true;
				}
			}
			position++;
		}
		position++;
		lexOptionalStringLiteralPostfix;
	}

	private void lexDelimitedStringLiteral() {
		currentToken.type = Token.STRING_LITERAL;
		currentToken.stringLiteral.type = Token.StringLiteral.Type.DELIMITED;
		dchar closingDelimiter;
		switch (code[position++]) {
			case '[': closingDelimiter = ']'; break;
			case '(': closingDelimiter = ')'; break;
			case '<': closingDelimiter = '>'; break;
			case '{': closingDelimiter = '}'; break;
			default : throw new Exception("unknown delimiter " ~ to!string(code[position-1]));
		}
		while ((code[position] != closingDelimiter) || (code[position+1] != '"')) {
			position++;
		}
		currentToken.comment.value = code[currentToken.position+3 .. position];
		position += 2;
		lexOptionalStringLiteralPostfix;
	}

	private void lexTokenStringLiteral() {
		currentToken.type = Token.STRING_LITERAL;
		currentToken.stringLiteral.type = Token.StringLiteral.Type.TOKEN;
		assert(0);
		lexOptionalStringLiteralPostfix;
	}

	private void lexOptionalStringLiteralPostfix() {
		// TODO
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

