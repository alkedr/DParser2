module lexer;

import std.bigint : BigInt;
import std.algorithm : countUntil, map, join;
import std.string : format;
import std.conv : to;
import std.ascii;
import std.stdio;



// TODO: use currentChar, nextChar, advance и пр.



struct Lexer {

	struct Comment {
		dstring asString;
		ulong position;

		enum Type : ubyte {
			UNKNOWN,
			BLOCK,
			LINE,
			NESTING_BLOCK,
		}

		Type type() const {
			if (asString.length < 2) return Type.UNKNOWN;
			switch (asString[1]) {
				case '*': return Type.BLOCK;
				case '/': return Type.LINE;
				case '+': return Type.NESTING_BLOCK;
				default: return Type.UNKNOWN;
			}
		}

		dstring text() const {
			uint startOffset = isDdoc ? 3 : 2;
			uint endOffset = type == Type.LINE ? 1 : 2;
			return asString[startOffset .. $-endOffset];
		}

		bool isDdoc() const {
			if (text.length == 0) return false;
			if (type == Type.BLOCK) return text[0] == '*';
			if (type == Type.LINE) return text[0] == '/';
			if (type == Type.NESTING_BLOCK) return text[0] == '+';
			return false;
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

		bool empty() const {
			return asString.length == 0;
		}

		StringLiteralType stringLiteralType() const {
			if (asString.length >= 2) {
				if (asString[0] == 'r') return StringLiteralType.WYSIWYG;
				if (asString[0] == '`') return StringLiteralType.ALTERNATE_WYSIWYG;
				if (asString[0] == '"') return StringLiteralType.DOUBLE_QUOTED;
				if (asString[0] == 'x') return StringLiteralType.HEX;
				if (asString[0] == 'q') {
					if (asString[1] == '"') return StringLiteralType.DELIMITED;
					if (asString[1] == '{') return StringLiteralType.TOKEN;
				}
			}
			return StringLiteralType.UNKNOWN;
		}

		CharWidth charWidth() const {
			if (empty) return CharWidth.UNKNOWN;
			if (asString[$-1] == 'c') return CharWidth.ONE_BYTE;
			if (asString[$-1] == 'w') return CharWidth.TWO_BYTES;
			if (asString[$-1] == 'd') return CharWidth.FOUR_BYTES;
			return CharWidth.ONE_BYTE;
		}

		struct Empty {}
		struct NumberLiteral {
			BigInt mantissa;
			long exponent;
			NumberLiteralType numberLiteralType;
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

	dstring code;     // TODO: InputRange
	void delegate(string errorMessage) errorCallback;

	this(dstring code) {
		//writeln(code);
		this.code = code ~ 0;
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

	Token nextToken() {
		skipWhitespaceLineBreaksAndComments();
		ulong startPosition = position;
		mixin(
			new CodeGenerator()
				.withKeywords!keywords
				.withOperators!operators

				//.on!"/*"("return lexBlockComment;")
				//.on!"//"("return lexLineComment;")
				//.on!"/+"("return lexNestingBlockComment;")

				//.on!"#line"("lexLineSpecialTokenSequence;")

				.on!`r"`("return lexWysiwygStringLiteral(startPosition);")
				.on!"`"("return lexAlternateWysiwygStringLiteral(startPosition);")
				.on!`"`("return lexDoubleQuotedStringLiteral(startPosition);")
				.on!`x"`("return lexHexStringLiteral(startPosition);")
				.on!`q"`("return lexDelimitedStringLiteral(startPosition);")
				.on!"q{"("return lexTokenStringLiteral(startPosition);")

				.on!"'"("return lexSingleQuotedCharacterLiteral(startPosition);")

				.on!"0"("return lexNumberLiteralThatStartsWithZero(startPosition);")
				.onNonZeroDigit("return lexDecimalNumberLiteral(startPosition);")

				.onEndOfFile("return lexEofToken(startPosition);")

				.generateCode("return lexIdentifier(startPosition);")
		);
		//currentToken.asString = code[currentToken.position .. position];
	}


	private class CodeGenerator {
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

		//CodeGenerator onWhitespace(string code) {
		//	getCodeGeneratorForCase(
		//		`case'\u0020':` ~
		//		`case'\u0009':` ~
		//		`case'\u000B':` ~
		//		`case'\u000C':`
		//	).code = code;
		//	return this;
		//}

		//CodeGenerator onEndOfLine(string code) {
		//	getCodeGeneratorForCase(
		//		`case'\u000D':` ~
		//			`if (code[position] == '\u000A') position++;` ~
		//		`case'\u000A':` ~
		//		`case'\u2028':` ~
		//		`case'\u2029':`
		//	).code = code;
		//	return this;
		//}

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
				on(keyword, "return lexKeywordOrIdentifier(startPosition, Token.staticTokenIdFor(`" ~ keyword ~ "`));");
			}
			return this;
		}

		CodeGenerator withOperators(alias operators)() {
			foreach (operator; operators) {
				on(operator, "return lexOperator(startPosition, Token.staticTokenIdFor(`" ~ operator ~ "`));");
			}
			return this;
		}

		CodeGenerator withEscapeSequences(dchar[string] escapeSequences) {
			foreach (escapeSequence, c; escapeSequences) {
				on(escapeSequence, format(`return'\x%08X';`, c));
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

	private Token lexKeywordOrIdentifier(ulong startPosition, ubyte staticTokenId) {
		if (isIdentifierChar(code[position])) {
			return lexIdentifier(startPosition);
		} else {
			return lexKeyword(startPosition, staticTokenId);
		}
	}

	private Token constructToken(Token.Type type, ulong rangeStart, ulong rangeEnd) {
		Token result;
		result.asString = code[rangeStart .. rangeEnd];
		result.position = rangeStart;
		result.type = type;
		return result;
	}

	private Token constructStaticToken(Token.Type type, ulong startPosition, ubyte staticTokenId) {
		Token result = constructToken(type, startPosition, position);
		result.staticTokenId = staticTokenId;
		return result;
	}

	private Token constructStringLiteralToken(ulong startPosition, dstring value) {
		Token result = constructToken(Token.Type.STRING_LITERAL, startPosition, position);
		result.stringLiteral = value;
		return result;
	}

	private Token constructEndOfFileToken(ulong startPosition) {
		return constructToken(Token.Type.END_OF_FILE, startPosition, position);
	}

	private Token constructIdentifierToken(ulong startPosition) {
		return constructToken(Token.Type.IDENTIFIER, startPosition, position);
	}

	private Token lexKeyword(ulong startPosition, ubyte staticTokenId) {
		return constructStaticToken(Token.Type.KEYWORD, startPosition, staticTokenId);
	}

	private Token lexOperator(ulong startPosition, ubyte staticTokenId) {
		return constructStaticToken(Token.Type.OPERATOR, startPosition, staticTokenId);
	}

	private dstring lexWysiwygStringLiteralValue(dchar finishChar) {
		dstring result;
		while (code[position] != finishChar) {
			result ~= code[position];
			position++;
		}
		position++;
		if ((code[position] == 'c') || (code[position] == 'w') || (code[position] == 'd')) position++;
		return result;
	}

	private Token lexGeneralWysiwygStringLiteral(ulong startPosition, Token.StringLiteralType stringLiteralType, dchar finishChar) {
		return constructStringLiteralToken(startPosition, lexWysiwygStringLiteralValue(finishChar));
	}

	private Token lexWysiwygStringLiteral(ulong startPosition) {
		return lexGeneralWysiwygStringLiteral(startPosition, Token.StringLiteralType.WYSIWYG, '"');
	}

	private Token lexAlternateWysiwygStringLiteral(ulong startPosition) {
		return lexGeneralWysiwygStringLiteral(startPosition, Token.StringLiteralType.ALTERNATE_WYSIWYG, '`');
	}

	// returns dstring because escape sequence may contain more than one code point
	// See HTML5 spec: http://www.w3.org/TR/html5/syntax.html#named-character-references
	private dstring lexCharacterOrEscapeSequence() {
		if (code[position++] == '\\') {

			// need code generator with backtracking (if no match return to first symbol and return it)
			//mixin(new CodeGenerator().withEscapeSequences().generateCode("return code[position-1];"));
		}
		return [code[position-1]];


		//mixin(new CodeGenerator()
		//	.withNamedCharacterEntities(namedCharacterEntities)
		//	.on(`\`, "return lexReverseSlashEscapeSequence;")

		//	\'
		//	\"
		//	\?
		//	\\
		//	\0
		//	\a
		//	\b
		//	\f
		//	\n
		//	\r
		//	\t
		//	\v
		//	\x HexDigit HexDigit
		//	\ OctalDigit
		//	\ OctalDigit OctalDigit
		//	\ OctalDigit OctalDigit OctalDigit
		//	\u HexDigit HexDigit HexDigit HexDigit
		//	\U HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit
		//	.generateCode("return code[position-1];")   // неправильно, во вложенных свичах попадание в default - ошибка
		//)
	}

	//private dchar

	private Token lexDoubleQuotedStringLiteral(ulong startPosition) {
		assert(0);
	}

	private Token lexHexStringLiteral(ulong startPosition) {
		ubyte hexDigitToInt(dchar c) {
			if (isDigit(code[position])) return cast(ubyte)(c - '0');
			if (isLower(code[position])) return cast(ubyte)(c - 'a' + 10);
			if (isUpper(code[position])) return cast(ubyte)(c - 'A' + 10);
			throw new Exception("hexDigitToInt");
		}

		dstring value;
		//currentToken.type = Token.Type.STRING_LITERAL;
		bool hexCharIsRemembered = false;
		ushort c = 0;
		while (code[position] != '"') {
			if (isHexDigit(code[position])) {
				if (hexCharIsRemembered) {
					value ~= ((c << 4) | hexDigitToInt(code[position]));
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
		return constructStringLiteralToken(startPosition, value);
	}

	private Token lexDelimitedStringLiteral(ulong startPosition) {
		//currentToken.type = Token.Type.STRING_LITERAL;
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
		auto value = code[startPosition+3 .. position];
		position += 2;
		return constructStringLiteralToken(startPosition, value);
	}

	private Token lexTokenStringLiteral(ulong startPosition) {
		assert(0);
	}

	private Token lexSingleQuotedCharacterLiteral(ulong startPosition) {
		assert(0);
	}

	private Token lexNumberLiteralThatStartsWithZero(ulong startPosition) {
		assert(0);
	}

	private Token lexBinaryNumberLiteral(ulong startPosition) {
		assert(0);
	}

	private Token lexHexNumberLiteral(ulong startPosition) {
		assert(0);
	}

	private Token lexDecimalNumberLiteral(ulong startPosition) {
		assert(0);
	}

	private Token lexEofToken(ulong startPosition) {
		return constructEndOfFileToken(startPosition);
	}

	private Token lexIdentifier(ulong startPosition) {
		while (isIdentifierChar(code[position])) {
			position++;
		}
		return constructIdentifierToken(startPosition);
	}

	private Token lexUnknown(ulong startPosition) {
		//currentToken.type = Token.Type.UNKNOWN;
		//position++;
		return constructToken(Token.Type.UNKNOWN, startPosition, position);
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

