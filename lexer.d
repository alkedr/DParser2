module lexer;

import std.bigint : BigInt;
import std.algorithm : countUntil, map, join;
import std.string : format;
import std.conv : to;
import std.ascii;
import std.stdio;


struct Lexer {

	struct Comment {
		dstring asString;
		ulong position;
		Type type;
		bool isDdoc;

		enum Type {
			UNKNOWN,
			BLOCK,
			LINE,
			NESTING_BLOCK,
		}

		dstring text() {
			uint startOffset = isDdoc ? 3 : 2;
			uint endOffset = type == Type.LINE ? 1 : 2;
			return asString[startOffset .. $-endOffset];
		}
	}

	struct Token {
		dstring asString;
		ulong position;
		union {
			struct {
				Type type;
				ubyte staticTokenId;
			}
			ushort code;
		}
		union {
			struct {
				StringLiteralType stringLiteralType;
				CharWidth charWidth;
			}
			NumberLiteralType numberLiteralType;
		}
		union {
			Empty unknown;
			Empty identifier;
			dstring stringLiteral;
			dchar characterLiteral;
			NumberLiteral numberLiteral;
			Empty keyword;
			Empty operator;
			Empty endOfFile;
		};
		Comment[] precedingComments;

		struct Empty {}
		struct NumberLiteral {
			BigInt mantissa;
			long exponent;
		}

		enum Type : ubyte {
			UNKNOWN,
			IDENTIFIER,
			STRING_LITERAL,
			CHARACTER_LITERAL,
			NUMBER_LITERAL,
			KEYWORD,
			OPERATOR,
			END_OF_FILE,   // not a token, terminates token sequence, contains comments after last token
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
		}

		static ubyte staticTokenIdFor(string staticToken) {
			assert(staticTokens.countUntil(staticToken) != -1);
			auto result = staticTokens.countUntil(staticToken);
			assert(result < 256);
			return cast(ubyte)result;
		}
	}


/*
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
	}*/

	dstring code;     // TODO: InputRange
	Token currentToken;
	void delegate(string errorMessage) errorCallback;

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
	private bool isEmpty = false;

	// to find line number for position find nearest preceding lineBeginPosition
	// and add lineNumberDelta of nearest preceding lineSpecialToken
	private ulong[] lineBeginPositions = [ 0 ];    // physical lines (#line has no effect here)
	private LineSpecialToken[] lineSpecialTokens = [{0, 0, ""}];  // sorted by lineStartPosition

	Token front() { return currentToken; }
	Token moveFront() { return currentToken; }
	bool empty() { return isEmpty; }//position >= code.length-1; }
	//bool empty() { return currentToken.type == Token.END_OF_FILE; }

	void popFront() {
		skipWhitespaceLineBreaksAndComments();
		currentToken = Token.init;
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
				on(keyword, "lexKeywordOrIdentifier(Token.staticTokenIdFor(`" ~ keyword ~ "`));");
			}
			return this;
		}

		CodeGenerator withOperators(alias operators)() {
			foreach (operator; operators) {
				on(operator, "lexOperator(Token.staticTokenIdFor(`" ~ operator ~ "`));");
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
		//currentToken.type = Token.COMMENT;
		//currentToken.comment.type = Token.Comment.Type.BLOCK;
		assert(0);
		//while ((code[position] != '*') || (code[position+1] != '/')) {
		//	position++;
		//}
		//currentToken.comment.value = code[currentToken.position+2 .. position];
		//position += 2;
	}

	private void lexLineComment() {
		//currentToken.type = Token.COMMENT;
		//currentToken.comment.type = Token.Comment.Type.LINE;
		assert(0);
		//while (true) {
		//	switch (code[position++]) {
		//		case '\u000D':
		//			currentToken.comment.value = code[currentToken.position+2 .. position-1];
		//			if (code[position] == '\u000A') position++;
		//			return;
		//		case '\u000A':
		//		case '\u2028':
		//		case '\u2029':
		//			currentToken.comment.value = code[currentToken.position+2 .. position-1];
		//			return;
		//		default:
		//	}
		//};
	}

	private void lexNestingBlockComment() {
		//currentToken.type = Token.COMMENT;
		//currentToken.comment.type = Token.Comment.Type.NESTING_BLOCK;
		assert(0);
		//int depth = 1;
		//while (depth > 0) {
		//	if ((code[position] == '/') && (code[position+1] == '+')) {
		//		depth++;
		//		position += 2;
		//	} else if ((code[position] == '+') && (code[position+1] == '/')) {
		//		depth--;
		//		position += 2;
		//	} else {
		//		position++;
		//	}
		//}
		//currentToken.comment.value = code[currentToken.position+2 .. position-2];
	}

	private void lexLineSpecialTokenSequence() {
		assert(0); // TODO: move to skipWhitespaceAndLineBreaks?
	}

	private void lexKeywordOrIdentifier(ubyte staticTokenId) {
		if (isIdentifierChar(code[position])) {
			lexIdentifier;
		} else {
			currentToken.type = Token.Type.KEYWORD;
			currentToken.staticTokenId = staticTokenId;
		}
	}

	private void lexOperator(ubyte staticTokenId) {
		currentToken.type = Token.Type.OPERATOR;
		currentToken.staticTokenId = staticTokenId;
	}

	private void lexWysiwygStringLiteral() {
		currentToken.type = Token.Type.STRING_LITERAL;
		currentToken.stringLiteralType = Token.StringLiteralType.WYSIWYG;
		while (code[position] != '"') position++;
		currentToken.stringLiteral = code[currentToken.position+2 .. position];
		position++;
		lexOptionalStringLiteralPostfix;
	}

	private void lexAlternateWysiwygStringLiteral() {
		currentToken.type = Token.Type.STRING_LITERAL;
		currentToken.stringLiteralType = Token.StringLiteralType.ALTERNATE_WYSIWYG;
		while (code[position] != '`') position++;
		currentToken.stringLiteral = code[currentToken.position+1 .. position];
		position++;
		lexOptionalStringLiteralPostfix;
	}

	private void lexDoubleQuotedStringLiteral() {
		currentToken.type = Token.Type.STRING_LITERAL;
		currentToken.stringLiteralType = Token.StringLiteralType.DOUBLE_QUOTED;
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

		currentToken.type = Token.Type.STRING_LITERAL;
		currentToken.stringLiteralType = Token.StringLiteralType.HEX;
		bool hexCharIsRemembered = false;
		ushort c = 0;
		while (code[position] != '"') {
			if (isHexDigit(code[position])) {
				if (hexCharIsRemembered) {
					currentToken.stringLiteral ~= ((c << 4) | hexDigitToInt(code[position]));
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
		currentToken.type = Token.Type.STRING_LITERAL;
		currentToken.stringLiteralType = Token.StringLiteralType.DELIMITED;
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
		currentToken.stringLiteral = code[currentToken.position+3 .. position];
		position += 2;
		lexOptionalStringLiteralPostfix;
	}

	private void lexTokenStringLiteral() {
		currentToken.type = Token.Type.STRING_LITERAL;
		currentToken.stringLiteralType = Token.StringLiteralType.TOKEN;
		assert(0);
		lexOptionalStringLiteralPostfix;
	}

	private void lexOptionalStringLiteralPostfix() {
		// TODO
	}

	private void lexSingleQuotedCharacterLiteral() {
		currentToken.type = Token.Type.CHARACTER_LITERAL;
		assert(0);
	}

	private void lexNumberLiteralThatStartsWithZero() {
		currentToken.type = Token.Type.NUMBER_LITERAL;
		assert(0);
	}

	private void lexBinaryNumberLiteral() {
		currentToken.type = Token.Type.NUMBER_LITERAL;
		assert(0);
	}

	private void lexHexNumberLiteral() {
		currentToken.type = Token.Type.NUMBER_LITERAL;
		assert(0);
	}

	private void lexDecimalNumberLiteral() {
		currentToken.type = Token.Type.NUMBER_LITERAL;
		assert(0);
	}

	private void lexEofToken() {
		isEmpty = true;
	}

	private void lexIdentifier() {
		currentToken.type = Token.Type.IDENTIFIER;
		while (isIdentifierChar(code[position])) {
			position++;
		}
	}

	private void lexUnknown() {
		currentToken.type = Token.Type.UNKNOWN;
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
		writeln(Lexer.Token.staticTokenIdFor(staticTokens[0]));
		writeln(Lexer.Token.staticTokenIdFor(staticTokens[$-1]));
	}
}

