
import std.stdio;
import std.typecons;
import std.algorithm;
import std.conv;
import std.string;


/*
struct ParserGenerator {

	enum Type {
		SWITCH,
		CHOICE,
		SEQUENCE,
		LIST
	}

	Type type;

	ParserGenerator[dchar] rules;  // SWITCH
	ParserGenerator[] subpgs;  // CHOICE and SEQUENCE
	ParserGenerator item;
	ParserGenerator separator;

	string action;  // if nothing else matches

	string[][string] fields;  // name -> types
}


ParserGenerator keyword(string s, ParserGenerator pg = ParserGenerator()) {
	pg.type = ParserGenerator.Type.SWITCH;
	if (s.length == 0) return pg;
	pg[s[0]] = keyword(s[1..$])
	return pg;
}

ParserGenerator pgFor(string s) {
	return keyword(s);
}

ParserGenerator pgFor(ParserGenerator pg) {
	return pg;
}

ParserGenerator choice(args...)() {
	ParserGenerator pg;
	pg.type = ParserGenerator.Type.CHOICE;
	foreach (arg; args) {
		pg.subpgs ~= pgFor(arg);
	}
	return pg;
}

ParserGenerator sequence(args...)() {
	ParserGenerator pg;
	pg.type = ParserGenerator.Type.SEQUENCE;
	foreach (arg; args) {
		pg.subpgs ~= pgFor(arg);
	}
	return pg;
}

ParserGenerator list(alias item, alias separator)() {
	ParserGenerator pg;
	pg.type = ParserGenerator.Type.LIST;
	pg.item = item;
	pg.separator = separator;
	return pg;
}

ParserGenerator field(T)(string name) {
	ParserGenerator pg = parserForType!T;
	pg.fields[name] = [T];
	return pg;
}*/

/*
struct ParserData {

}


class ParserGenerator {
	abstract string code() const;
}

class Switch : ParserGenerator {
	ParserGenerator[dchar] rules;


	override string code() const {

	}
}

class Choice : ParserGenerator {
	ParserGenerator[] subpgs;

	override string code() const {

	}
}

class Sequence : ParserGenerator {
	ParserGenerator[] subpgs;

	override string code() const {

	}
}

class List : ParserGenerator {
	ParserGenerator item;
	ParserGenerator separator;

	override string code() const {

	}
}

class Field : ParserGenerator {
	string[][string] fields;

	override string code() const {

	}
}

*/


//struct ParserData {
//	string code;
//	string[][string] fields;
//}



class Cursor {
	const(dstring) text;
	size_t index;
	//Comment[] comments;

	this(const(dstring) text) {
		this.text = text ~ 0;
	}

	dchar currentChar() {
		return text[index];
	}

	void advance() {
		advanceNoSkip();
		skipWhitespaceAndLineBreaksAndParseComments();
	}

	void advanceNoSkip() {
	}

	void skipWhitespaceAndLineBreaksAndParseComments() {
	}
}





struct CharGroup {
	string allowedFirstChars;
	string allowedChars;
}

struct Field(T : node!(args), args...) {
	string fieldName;
}

struct List(T : node!(args), args...) {
	string fieldName;
	string separator;
}

struct Sequence(args...) {
}

struct Choice(args...) {
}


class node(args...) {

	static string[][string] getFields(args...)() {
		string[][string] result;
		foreach (arg; args) getFieldsImpl(arg, result);
		return result;
	}

	static void getFieldsImpl(T : Sequence!(args), args...)(T t, ref string[][string] result) {
		foreach (arg; args) getFieldsImpl(arg, result);
	}

	static void getFieldsImpl(T : Choice!(args), args...)(T t, ref string[][string] result) {
		foreach (arg; args) getFieldsImpl(arg, result);
	}

	static void getFieldsImpl(T : List!U, U : node!(args), args...)(T t, ref string[][string] result) {
		if (t.fieldName !in result) result[t.fieldName] = [];
		result[t.fieldName] ~= U.stringof ~ "[]";
	}

	static void getFieldsImpl(T : Field!U, U : node!(args), args...)(T t, ref string[][string] result) {
		if (t.fieldName !in result) result[t.fieldName] = [];
		result[t.fieldName] ~= U.stringof;
	}

	static void getFieldsImpl(CharGroup t, ref string[][string] result) {
	}

	static void getFieldsImpl(string s, ref string[][string] result) {
	}

	static string generateFieldsCode() {
		string result;
		auto map = getFields!(args);
		foreach (name, types; map) {
			assert(types.length == 1);  // FIXME
			result ~= types[0] ~ " " ~ name ~ ";";
		}
		return result;
	}

	mixin(generateFieldsCode);
}


auto field(T)(string fieldName) { return Field!T(fieldName); }
auto list(T)(string fieldName, string separator) { return List!T(fieldName, separator);}
auto dotList(T)(string fieldName) { return list!(T)(fieldName, "."); }
auto commaList(T)(string fieldName) { return list!(T)(fieldName, ","); }
auto optional(args...)() { return Choice!(Sequence!(args)(), "")(); }
auto charGroup(string firstChars, string chars = firstChars)() { return CharGroup(firstChars, chars); }


class Identifier : node!(
	charGroup!(
		"abcdefghigklmnopqrstuvwsyzABCDEFGHIGKLMNOPQRSTUVWSYZ_",
		"abcdefghigklmnopqrstuvwsyzABCDEFGHIGKLMNOPQRSTUVWSYZ_0123456789"
	)
){};

class ModuleName : node!(
	dotList!Identifier("parts")
){};

class ImportBinding : node!(
	optional!(field!Identifier("aliasName"), "="),
	field!Identifier("symbolName")
){};

class Import : node!(
	optional!(field!Identifier("aliasName"), "="),
	field!ModuleName("moduleName"),
	optional!(":", commaList!(ImportBinding)("bindings"))
){};

class ImportDeclaration : node!(
	"import",
	commaList!(Import)("imports")
){};





struct ParserGenerator {
	void add(T : node!(args), args...)() {
		add(args);
	}

	string code() const {
		if (rules.length == 0) return variableDeclarations ~ action;
		auto result = variableDeclarations ~ "switch(getCurrentCharAndAdvanceNoSkip()){";
		foreach (key, value; rules) {
			result ~= format("case'\\U%08X':{\n%s}break;\n", key, value.code);
		}
		return result ~ format("default:{%s}break;\n}\n", action);
	}

private:
	string variableDeclarations = "";
	ParserGenerator[dchar] rules;
	string action = "";

	string indent;
	int tempVarCount;


	ParserGenerator nodeVariable(string type) {
		variableDeclarations ~= format("%sauto t%d = new %s;\n", indent, tempVarCount, type);
		return this;
	}

	void addRule(dchar c, ParserGenerator pg) {
		//if (c in rules) {
		//	rules[c].add(pg);
		//} else {
		//	rules[c] = pg;
		//}
	}

	//ParserGenerator add(ParserGenerator pg) {
	//	foreach (key, value; pg.rules) {
	//		addRule(key, value);
	//	}
	//	return this;
	//}


	void add(T : Sequence!(args), args...)(T t) {
		if (args.length > 0) {
			add(args[0], Sequence!(args[1..$]));
		}
	}


	void add(U, T : Sequence!(args), args...)(T t, U u) {
		if (args.length > 0) {
			add(args[0], Sequence!(args[1..$] ~ u));
		} else {

		}
	}

	void add(T : Choice!(args), args...)(T t) {
		//foreach (arg; args) {
		//	add(arg);
		//}
		//return this;
	}

	void add(T : List!(node!(args)), args...)(T t, ParserGenerator nextItemInSequence) {
		//return this;
	}

	void add(T : Field!(node!(args)), args...)(T t, ParserGenerator nextItemInSequence) {
		//return this;
	}

	void add(CharGroup t, ParserGenerator nextItemInSequence) {
		//return this;
	}

	void add(string t, ParserGenerator nextItemInSequence) {
		//if (t.length > 0) {
		//	addRule(t[0], ParserGenerator().add(t[1..$], nextItemInSequence));
		//} else {
		//	action = "dsdgvsvb";
		//}
		//return this;
	}



/*
	ParserGenerator add(ParserGenerator pg) {
		foreach (key, value; pg.rules) {
			addRule(key, value);
		}
		return this;
	}

	ParserGenerator add(T : Sequence!(args), args...)(T t, ParserGenerator nextItemInSequence) {
		static if (args.length > 0) {
			add(args[0], ParserGenerator.add(Sequence!(args[1..$])(), nextItemInSequence));
		}
		return this;
	}

	ParserGenerator add(T : Choice!(args), args...)(T t, ParserGenerator nextItemInSequence) {
		foreach (arg; args) {   // TODO: iterate in reverse order, override
			add(arg, nextItemInSequence);
		}
		return this;
	}

	ParserGenerator add(T : List!(node!(args)), args...)(T t, ParserGenerator nextItemInSequence) {
		return this;
	}

	ParserGenerator add(T : Field!(node!(args)), args...)(T t, ParserGenerator nextItemInSequence) {
		return this;
	}

	ParserGenerator add(CharGroup t, ParserGenerator nextItemInSequence) {
		return this;
	}

	ParserGenerator add(string t, ParserGenerator nextItemInSequence) {
		if (t.length > 0) {
			addRule(t[0], ParserGenerator().add(t[1..$], nextItemInSequence));
		} else {
			action = "dsdgvsvb";
		}
		return this;
	}*/

}


/*
ParserGenerator createParserGenerator(T : node!(args), args...)(ParserGenerator pg = ParserGenerator()) {
	foreach (arg; args) {
		pg = pg.add(arg);
	}
	return pg;
}

ParserGenerator createParserGenerator(T : Sequence!(args), args...)(T t, ParserGenerator pg) {
	writeln("Sequence {");
	foreach (arg; args) dump(arg);
	writeln("}");
}

ParserGenerator createParserGenerator(T : Choice!(args), args...)(T t, ParserGenerator pg) {
	writeln("Choice {");
	foreach (arg; args) dump(arg);
	writeln("}");
}

ParserGenerator createParserGenerator(T : List!(node!(args)), args...)(T t, ParserGenerator pg) {
	writeln("List " ~ "node!" ~ args.stringof ~ " " ~ t.fieldName ~ " " ~ t.separator ~ " {");
	dump!(node!(args));
	writeln("}");
}

ParserGenerator createParserGenerator(T : Field!(node!(args)), args...)(T t, ParserGenerator pg) {
	writeln("Field " ~ "node!" ~ args.stringof ~ " " ~ t.fieldName ~ " {");
	dump!(node!(args));
	writeln("}");
}

ParserGenerator createParserGenerator(CharGroup t, ParserGenerator pg) {
	writeln("CharGroup {");
	writeln("  allowedFirstChars = ", t.allowedFirstChars);
	writeln("  allowedChars = ", t.allowedChars);
	writeln("}");
}

ParserGenerator createParserGenerator(string s, ParserGenerator pg) {
	writeln("String '" ~ s ~ "'");
}
*/



void dump(T : node!(args), args...)() {
	writeln("Node { Sequence {");
	foreach (arg; args) dump(arg);
	writeln("}}");
}

void dump(T : Sequence!(args), args...)(T t) {
	writeln("Sequence {");
	foreach (arg; args) dump(arg);
	writeln("}");
}

void dump(T : Choice!(args), args...)(T t) {
	writeln("Choice {");
	foreach (arg; args) dump(arg);
	writeln("}");
}

void dump(T : List!(node!(args)), args...)(T t) {
	writeln("List " ~ "node!" ~ args.stringof ~ " " ~ t.fieldName ~ " " ~ t.separator ~ " {");
	dump!(node!(args));
	writeln("}");
}

void dump(T : Field!(node!(args)), args...)(T t) {
	writeln("Field " ~ "node!" ~ args.stringof ~ " " ~ t.fieldName ~ " {");
	dump!(node!(args));
	writeln("}");
}

void dump(CharGroup t) {
	writeln("CharGroup {");
	writeln("  allowedFirstChars = ", t.allowedFirstChars);
	writeln("  allowedChars = ", t.allowedChars);
	writeln("}");
}

void dump(string s) {
	writeln("String '" ~ s ~ "'");
}



/*
string generateCode(T : node!(args), args...)(string indent = "", int tmpVarsCount = 0) {
	string result = indent ~ T.stringof ~ " t" ~ to!string(tmpVarsCount) ~ ";\n";
	foreach (arg; args) result ~= generateCode(arg, indent, tmpVarsCount+1);
	return result;
}

string generateCode(T : Sequence!(args), args...)(T t, string indent, int tmpVarsCount) {
	string result;
	foreach (arg; args) result ~= generateCode(arg, indent, tmpVarsCount);
	return result;
}

string generateCode(T : Choice!(args), args...)(T t, string indent, int tmpVarsCount) {
	string result = indent ~ "Choice {\n";
	foreach (arg; args) {
		result ~= indent ~ "  {\n" ~ generateCode(arg, indent~"    ", tmpVarsCount) ~ indent ~ "  }\n";
	}
	return result ~ indent ~ "}\n";
}

string generateCode(T : List!U, U : node!(args), args...)(T t, string indent, int tmpVarsCount) {
	return indent ~ "List {\n" ~
		generateCode!U(indent~"  ", tmpVarsCount) ~
		indent ~ "  " ~ t.fieldName ~ " ~= t" ~ to!string(tmpVarsCount) ~ ";\n" ~
		indent ~ "}\n";
}

string generateCode(T : Field!U, U : node!(args), args...)(T t, string indent, int tmpVarsCount) {
	return generateCode!U(indent, tmpVarsCount) ~
		indent ~ t.fieldName ~ " = t" ~ to!string(tmpVarsCount) ~ ";\n";
	//return indent ~ "Field\n";
}

string generateCode(CharGroup t, string indent, int tmpVarsCount) {
	return indent ~ "t" ~ to!string(tmpVarsCount-1) ~ " = Identifier\n";
}

string generateCode(string s, string indent, int tmpVarsCount) {
	return indent ~ "\"" ~ s ~ "\"\n";
}
*/



string generateCode(T : node!(args), args...)(string indent = "", int tmpVarsCount = 0) {
	string result;
	foreach (arg; args) result ~= generateCode(arg, indent, tmpVarsCount+1) ~ " ";
	return result[0..$-1];
}

string generateCode(T : Sequence!(args), args...)(T t, string indent, int tmpVarsCount) {
	string result;
	foreach (arg; args) result ~= generateCode(arg, indent, tmpVarsCount) ~ " ";
	return result[0..$-1];
}

string generateCode(T : Choice!(args), args...)(T t, string indent, int tmpVarsCount) {
	string result = "[";
	foreach (arg; args) {
		result ~= generateCode(arg, indent~"  ", tmpVarsCount) ~ "|";
	}
	return result[0..$-1] ~"]";
}

string generateCode(T : List!U, U : node!(args), args...)(T t, string indent, int tmpVarsCount) {
	return "(" ~ generateCode!U(indent, tmpVarsCount) ~ ")";
}

string generateCode(T : Field!U, U : node!(args), args...)(T t, string indent, int tmpVarsCount) {
	return generateCode!U(indent, tmpVarsCount);
	//return indent ~ "Field\n";
}

string generateCode(CharGroup t, string indent, int tmpVarsCount) {
	return "Identifier";
}

string generateCode(string s, string indent, int tmpVarsCount) {
	return "\"" ~ s ~ "\"z";
}



string generateCode(T : node!(args), args...)(string indent = "", int tmpVarsCount = 0) {
	string result;
	foreach (arg; args) result ~= generateCode(arg, indent, tmpVarsCount+1) ~ " ";
	return result[0..$-1];
}

unittest {
	//writeln(ParserGenerator().add!ImportDeclaration);
	//writeln(generateParser!(ImportDeclaration)("", 0, ParserGenerator()).code);

	//writeln(generateCode!ImportDeclaration);
	//writeln(Identifier.generateFieldsCode);
	//writeln(ModuleName.generateFieldsCode);
	//writeln(ImportBinding.generateFieldsCode);
	//writeln(Import.generateFieldsCode);
	//writeln(ImportDeclaration.generateFieldsCode);
}




// Sequence - взять парсер, добавить ко всем его листьям следующий парсер, ко всем листьям результата добавить следующий, и т. д.
// Choice - добавить все парсеры в один корень
// Field -
// List -
// CharGroup -




/*
class ParserGenerator {
}

class CharSequenceParserGenerator {
	dstring allowedFirstChars;
	dstring allowedChars;

	string fieldName;
}

class StringParserGenerator {
	dstring s;
}

class ListParserGenerator {
	ParserGenerator itemParser;

	string fieldName;
}




void main() {
	writeln(getFields!ModuleName);
}
*/












/*

void f() {
	switch (currentChar) {case 'i':advanceNoSkip();
		switch (currentChar) {case 'm':advanceNoSkip();
			switch (currentChar) {case 'p':advanceNoSkip();
				switch (currentChar) {case 'o':advanceNoSkip();
					switch (currentChar) {case 'r':advanceNoSkip();
						switch (currentChar) {case 't':advanceNoSkip();

							switch (currentChar) {case ' ':  // WS, LB and comments
								skipCrap();

								auto t0 = new Identifier;
								t0.index = currentPosition;
								l0: switch (currentChar) {
									case 'IdentifierFirstChar':	advanceNoSkip(); goto l0;
									case '.':
								}
								t0.text = text[t0.index, currentPosition];

							}

						}
					}
				}
			}
		}
	}
}
*/



/*
class Action {
	abstract string getCode() const;
}


class RulesAction : Action {
	Action[dchar] rules;
	Action onNoMatch;

	override string getCode() const {
		// do { switch {} } while (false);
	}
}

class CodeAction : Action {
	string code;

	override string getCode() const {
		return code;
	}
}

class LoopAction : Action {
	override string getCode() const {
		// continue
	}
}

class FieldAction : Action {

}
*/



/*

class Cursor {

}

class TextRange {

}


class Parser {
protected:

	void parse(Cursor cursor) {

	}

	void onKeyword_module(Cursor cursor, TextRange textRange) {}
	void onKeyword_import(Cursor cursor, TextRange textRange) {}

	void onIdentifier(Cursor cursor, TextRange textRange) {}

}


class ListParser(string separator) : Parser {

	override void parse(Cursor cursor) {

	}

}


class TopLevelDaclarationParser : Parser {
protected:

	override void onKeyword_module(Cursor cursor, TextRange textRange) {

		static class ModuleNameParser : Parser {
			public TextRange[] packageNames;
			protected final override void onIdentifier(Cursor cursor, TextRange textRange) {
				packageNames ~= textRange;
			}
		}

		scope auto moduleNameParser = new ModuleNameParser();
		moduleNameParser.parse(cursor);
	}

	override void onKeyword_import(Cursor cursor, TextRange textRange) {}

	override void onIdentifier(Cursor cursor, TextRange textRange) {}

}




*/

/*


auto parseTopLevelDeclaration() {
	skipCrap();
	auto begin = currentPosition;
	switch (currentChar) {
		case 'i':
			switch (currentChar) {
				case 'm':
					switch (currentChar) {
						case 'p':
							switch (currentChar) {
								case 'o':
									switch (currentChar) {
										case 'r':
											switch (currentChar) {
												case 't':
													switch (currentChar) {
														case identifierChar:
															return on_identifier(finishParsingIdentifier(begin));
														default:
															return on_import();
													}
											}
									}
							}
					}
			}
	}
}
*/
