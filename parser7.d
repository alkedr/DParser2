import std.stdio;
import std.typecons;
import std.algorithm;
import std.conv;
import std.string;
import std.array;



class ASTNode(Args...) {
}



struct StartToken { string fieldName; }
struct Char(char c) { string fieldName; }
struct EndToken { string fieldName; }
struct Identifier { string fieldName; }
struct Field(T : ASTNode!(Args), Args...) { string fieldName; }
struct LoopStart {}
struct LoopContinue(string separator, T : ASTNode!(Args), Args...) { string fieldName; }
struct LoopEnd {}
struct Sequence(Args...) { Args args; }
struct Choice(Args...) { Args args; }
struct MoveValue(string from, string to) {}

auto charSequence(string s, A...)(A a) {
	static if (s.length > 0) {
		return charSequence!(s[1..$], A, Char!(s[0]))(a, Char!(s[0])());
	} else {
		return sequence(a);
	}
}

auto keyword(string s)(string fieldName = s ~ "Keyword") {
	return sequence(StartToken(fieldName), charSequence!(s)(fieldName), EndToken(fieldName));
}

auto identifier(string fieldName) {
	return sequence(StartToken(fieldName), Identifier(fieldName), EndToken(fieldName));
}

auto field(T : ASTNode!(Args), Args...)(string fieldName) {
	return Field!(T)(fieldName);
}

auto listWhile(string separator, T : ASTNode!(Args), Args...)(string fieldName) {
	return optional(keyword(separator), listDoWhile!(separator, T)(fieldName));
}

auto listDoWhile(string separator, T : ASTNode!(Args), Args...)(string fieldName) {
	return sequence(
		LoopStart!(separator, T)(fieldName),
		sequence(Args),
		optional(keyword(separator), LoopContinue!(separator, T)(fieldName))
	);

	return sequence(
		ListDoWhile!(separator, T)(fieldName),
		optional(keyword(separator), ListWhileEnd!(separator, T)(fieldName))
	);

	return ListDoWhile!(separator, T)(fieldName);
}

auto dotList(T : ASTNode!(nodes), nodes...)(string fieldName) {
	return listDoWhile!(".", T)(fieldName);
}

auto commaList(T : ASTNode!(nodes), nodes...)(string fieldName) {
	return listDoWhile!(",", T)(fieldName);
}

auto sequence(Args...)(Args args) {
	return Sequence!Args(args);
}

auto choice(Args...)(Args args) {
	return Choice!Args(args);
}

auto move(string from, string to)() {
	return MoveValue!(from, to)();
}

//auto optional(A...)(A a) {
//	return choice(a, Empty());
//}



unittest {
	class TestASTNode : ASTNode!(identifier("test")) {}
	writeln(keyword!"qwe");
	writeln(identifier("qwe"));
	writeln(field!TestASTNode("qwe"));
	writeln(dotList!TestASTNode("qwe"));
	writeln(commaList!TestASTNode("qwe"));
	writeln(sequence(keyword!"a", keyword!"b"));
	writeln(choice(keyword!"a", keyword!"b"));
	//writeln(optional(keyword!"a"));
}
