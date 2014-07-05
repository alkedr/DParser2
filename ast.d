import lexer;


private struct ParseToken(uint tokenType) { string fieldName; }
private struct ParseList(uint separatorTokenType, contents) { string fieldName; }
private struct Sequence(Args...) { string fieldName; Args args; }
private struct Choice(Args...) { string fieldName; Args args; }
private struct Optional(Args...) { string fieldName; Args args; }

private auto tok(string s)(string fieldName = "") {
	assert(Token.staticTokens.countUntil(s) != -1);
	return ParseToken!(Token.Type.STATIC_TOKEN | (Token.staticTokens.countUntil(s) << 8))(fieldName);
}

private auto identifier(string fieldName) {
	return ParseToken!(Token.Type.IDENTIFIER)(fieldName);
}

private auto stringLiteral(string fieldName) {
	return ParseToken!(Token.Type.STRING_LITERAL)(fieldName);
}

private auto characterLiteral(string fieldName) {
	return ParseToken!(Token.Type.CHARACTER_LITERAL)(fieldName);
}

private auto integerLiteral(string fieldName) {
	return ParseToken!(Token.Type.INTEGER_LITERAL)(fieldName);
}

private auto floatLiteral(string fieldName) {
	return ParseToken!(Token.Type.FLOAT_LITERAL)(fieldName);
}

private auto sequence(Args...)(Args args) {
	return Sequence!Args(args);
}

private auto choice(Args...)(Args args) {
	return Choice!Args(args);
}

private auto optional(Args...)(Args args) {
	return Optional!Args(args);
}


// generated ast classes
class TopLevelDeclaration {}

