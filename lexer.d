module lexer;

import std.bigint : BigInt;
import std.algorithm : countUntil, map, join;
import std.string : format;
import std.conv : to;
import std.ascii;
import std.stdio;



// TODO: use currentChar, nextChar, advance и пр.
// TODO: конвертить все строки в string?

// ОПТИМИЗАЦИЯ ДЛЯ ПОДСВЕТКИ СИНТАКСИСА И АВТОДОПОЛНЕНИЯ
// Для подсветки и автодополнения не нужно парсить значения литералов, только типы
// TODO: лексер не парсит литералы, а только находит их начало и конец и определяет их тип
// (убрать union { значения литералов })



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

	struct Token2 {
		Lexer * lexer;
		size_t firstPrecedingCommentPosition;
		size_t position;
		size_t length;    // for keywords and operators holds (-staticTokenId), for other tokens length
	}

	struct Token {
		Comment[] precedingComments;
		dstring asString;
		ulong position;
		union {
			struct {
				Type type;
				ubyte staticTokenId;
			}
			ushort code;
		}
		NumberLiteralType numberLiteralType;

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

		this(dstring asString, ulong position, Type type, Comment[] precedingComments) {
			this.asString = asString;
			this.position = position;
			this.type = type;
			this.precedingComments = precedingComments;
		}

		this(Type type) {
			this.type = type;
		}

		this(NumberLiteralType numberLiteralType) {
			this.type = Token.Type.NUMBER_LITERAL;
			this.numberLiteralType = numberLiteralType;
		}

		this(Type type, ubyte staticTokenId) {
			assert((type == Token.Type.KEYWORD) || (type == Token.Type.OPERATOR));
			this.type = type;
			this.staticTokenId = staticTokenId;
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
		Comment[] comments = skipWhitespaceLineBreaksAndLexComments();
		ulong startPosition = position;
		Token result = lexToken();
		result.asString = code[startPosition .. position];
		result.position = startPosition;
		result.precedingComments = comments;
		return result;
	}

	// position must be on first char of token
	// returns Token without asString, position and comments
	// only type and subtype are set
	// updates position
	private Token lexToken() {
		if (isIdentifierFirstChar(code[position])) {
			mixin(
				new CodeGenerator()
					.onStaticTokens!keywords(q{return lexKeywordOrIdentifier(Token.staticTokenIdFor(`%s`));})
					.on!`r"`(q{return lexChars(`"`, Token.Type.STRING_LITERAL);})
					.on!`x"`(q{return lexChars(`"`, Token.Type.STRING_LITERAL);})
					.on!`q"`(q{return lexChars(`"`, Token.Type.STRING_LITERAL);})
					.on!"q{"(q{return lexTokenStringLiteral;})
					.on!"__EOF__"(q{return lexEndOfFile;})
					.generateCode(q{return lexIdentifier;})
			);
		} else {
			mixin(
				new CodeGenerator()
					.onStaticTokens!operators(q{return lexOperator(Token.staticTokenIdFor(`%s`));})
					.on!"`" (q{return lexChars("`", Token.Type.STRING_LITERAL);})
					.on!`"` (q{return lexChars(`"`, Token.Type.STRING_LITERAL);})
					.on!"'" (q{return lexChars("'", Token.Type.CHARACTER_LITERAL);})
					.on!"0" (q{return lexNumberLiteralThatStartsWithZero;})
					.on!('1', '2', '3', '4', '5', '6', '7', '8', '9')(q{return lexDecimalNumberLiteral;})
					.on!"."(
						new CodeGenerator()
							.on!('1', '2', '3', '4', '5', '6', '7', '8', '9')(q{return lexDecimalFloatThatStartsWithDot;})
							.generateCode(q{return lexOperator(Token.staticTokenIdFor(`.`));})
					)
					.on!('\u0000', '\u001A')(q{return lexEndOfFile;})
					.generateCode(q{return lexIdentifier;})  // FIXME: not lexIdentifier
			);
		}
	}

	private Token lexCharsWhileCurrentChar(alias predicate)(Token.Type resultType) {
		while ((position < code.length) && predicate(code[position])) {
			position++;
		}
		return Token(resultType);
	}

	private Token lexChars(dstring finishSequence, Token.Type resultType) {
		while (position + finishSequence.length <= code.length) {
			if (code[position .. position+finishSequence.length] == finishSequence) {
				position += finishSequence.length;
				return Token(resultType);
			}
			position++;
		}
		throw new Exception("Unexpected end of file");
	}


	private Token lexKeywordOrIdentifier(ubyte staticTokenId) {
		if (isIdentifierChar(code[position])) {
			return lexIdentifier;
		} else {
			return lexKeyword(staticTokenId);
		}
	}

	private Token lexKeyword(ubyte staticTokenId) {
		return Token(Token.Type.KEYWORD, staticTokenId);
	}

	private Token lexOperator(ubyte staticTokenId) {
		return Token(Token.Type.OPERATOR, staticTokenId);
	}

	//private Token lexStringLiteral(alias lexValueMethod)() {
	//	dstring value = lexValueMethod();
	//	if ((code[position] == 'c') || (code[position] == 'w') || (code[position] == 'd')) position++;
	//	return Token(Token.Type.STRING_LITERAL);
	//}

	// TODO: write general function that copies chars until (string finishSequence)

	//private dstring lexGeneralWysiwygStringLiteralValue(dchar finishChar) {
	//	dstring result;
	//	while (code[position] != finishChar) {
	//		result ~= code[position];
	//		position++;
	//	}
	//	position++;
	//	return result;
	//}

	//private dstring lexWysiwygStringLiteralValue() {
	//	return lexGeneralWysiwygStringLiteralValue('"');
	//}

	//private dstring lexAlternateWysiwygStringLiteralValue() {
	//	return lexGeneralWysiwygStringLiteralValue('`');
	//}

	// returns dstring because escape sequence may contain more than one code point
	// See HTML5 spec: http://www.w3.org/TR/html5/syntax.html#named-character-references
	//private dstring lexCharacterOrEscapeSequence() {
	//	if (code[position++] == '\\') {

			// need code generator with backtracking (if no match return to first symbol and return it)
			//mixin(new CodeGenerator().withEscapeSequences().generateCode("throw new Exception(`unknown escape sequence`)");
		//}
		//return [code[position-1]];


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
	//}

	private Token lexTokenStringLiteral() {
		assert(0);
	}

	//private dstring lexDoubleQuotedStringLiteralValue() {
	//	assert(0);
	//}

	//private dstring lexHexStringLiteralValue() {
	//	ubyte hexDigitToInt(dchar c) {
	//		if (isDigit(code[position])) return cast(ubyte)(c - '0');
	//		if (isLower(code[position])) return cast(ubyte)(c - 'a' + 10);
	//		if (isUpper(code[position])) return cast(ubyte)(c - 'A' + 10);
	//		throw new Exception("hexDigitToInt");
	//	}

	//	dstring value;
	//	bool hexCharIsRemembered = false;
	//	ushort c = 0;
	//	while (code[position] != '"') {
	//		if (isHexDigit(code[position])) {
	//			if (hexCharIsRemembered) {
	//				value ~= ((c << 4) | hexDigitToInt(code[position]));
	//				c = 0;
	//				hexCharIsRemembered = false;
	//			} else {
	//				c = cast(ushort)(hexDigitToInt(code[position]));
	//				hexCharIsRemembered = true;
	//			}
	//		}
	//		position++;
	//	}
	//	if (hexCharIsRemembered) {
	//		throw new Exception("uneven hex digits count in hex string literal");
	//	}
	//	position++;
	//	return value;
	//}

	//private dstring lexDelimitedStringLiteralValue() {
	//	dchar closingDelimiter;
	//	switch (code[position++]) {
	//		case '[': closingDelimiter = ']'; break;
	//		case '(': closingDelimiter = ')'; break;
	//		case '<': closingDelimiter = '>'; break;
	//		case '{': closingDelimiter = '}'; break;
	//		default : throw new Exception("unknown delimiter " ~ to!string(code[position-1]));
	//	}
	//	dstring result;
	//	while ((code[position] != closingDelimiter) || (code[position+1] != '"')) {
	//		result ~= code[position];
	//		position++;
	//	}
	//	position += 2;
	//	return result;
	//}

	//private dstring lexTokenStringLiteralValue() {
	//	assert(0);
	//}

	//private Token lexSingleQuotedCharacterLiteral() {
	//	assert(0);
	//}

	private Token lexNumberLiteralThatStartsWithZero() {
		switch (code[position]) {
			case 'x': return lexHexNumberLiteral;
			case 'b': return lexBinaryIntegerLiteral;
			case '0': .. case '9': throw new Exception("decimal number literals can't have leading zeros");
			//case '.' return lex
			default:
		}
		assert(0);
	}

	private Token lexBinaryIntegerLiteral() {
		assert(0);
	}

	private Token lexHexNumberLiteral() {
		assert(0);
	}

	private Token lexDecimalNumberLiteral() {
		assert(0);
	}

	private Token lexDecimalFloatThatStartsWithDot() {
		assert(0);
	}

	private Token lexEndOfFile() {
		return Token(Token.Type.END_OF_FILE);
	}

	private Token lexIdentifier() {
		return lexCharsWhileCurrentChar!(isIdentifierChar)(Token.Type.IDENTIFIER);
		//while (isIdentifierChar(code[position])) {
		//	position++;
		//}
		//return Token(Token.Type.IDENTIFIER);
	}

	private Token lexUnknown() {
		return Token(Token.Type.UNKNOWN);
	}

	private Comment[] skipWhitespaceLineBreaksAndLexComments() {
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
				default: return [];
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



	private class CodeGenerator {
		private string code = "";
		private CodeGenerator[string] cases;

		//private CodeGenerator on(string charSequence, string code) {
		//	if (charSequence.length == 0) {
		//		this.code = code;
		//	} else {
		//		if (charSequence[0] !in cases) cases[charSequence[0]] = new CodeGenerator;
		//		cases[charSequence[0]].on(charSequence[1..$], code);
		//	}
		//}



		//CodeGenerator onCase(string caseString, CodeGenerator cg) {
		//	cases[caseString] = cg;
		//}

		//CodeGenerator onChars(char[] chars, CodeGenerator cg) {
		//	cases[chars.map!(c => format(`case'\x%02X':`, c)).join] = cg;
		//}

		//CodeGenerator onCharSequence(string charSequence, CodeGenerator cg) {
		//	assert(charSequence.length != 0);
		//	if (charSequence.length == 1) {
		//		cases[charSequence] = cg;
		//	}
		//	cases[caseString] = cg;
		//}




		//private CodeGenerator lastChild = null;

		//public CodeGenerator onCase(string caseString) {
		//	lastChild = new CodeGenerator;
		//	cases[caseString] = lastChild;
		//	return this;
		//}

		//public CodeGenerator followedBy(string caseString) {
		//	lastChild.on(caseString);
		//	lastChild = lastChild.lastChild;
		//}

		//public CodeGenerator code(string newCode) {
		//	this.code = newCode;
		//	return this;
		//}






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

		CodeGenerator on(charSequences...)(string code) {
			static if (charSequences.length == 1) {
				return on(charSequences[0], code);
			} else {
				getCodeGeneratorForCase([charSequences].map!(c => format(`case'\x%02X':`, c)).join).code = code;
				return this;
			}
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


	unittest {
		writeln(Lexer.Token.staticTokenIdFor(staticTokens[0]));
		writeln(Lexer.Token.staticTokenIdFor(staticTokens[$-1]));
	}
}

