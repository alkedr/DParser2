import std.stdio;
import std.typecons;
import std.algorithm;
import std.conv;
import std.string;
import std.array;



struct TokenParser {
	enum Type {
		EMPTY,
		CHOICE,
		IDENTIFIER,
		CHAR,
		LIST,
	}

	Type type;

	// everything will be saved in this field
	string fieldType;
	string fieldName;

	bool isOptional;


	// for CHAR
	// charCondition must contain '%s' that will be replaced with currentChar expression
	string charCondition;
	// if (isFirstChar == true) whitespace, comments and line breaks will be skipped
	bool isFirstChar;

	// for LIST
	char listSeparator;

	// for CHOICE
	TokenParser[] subparsers;

	TokenParser * nextInSequence;


	TokenParser peek() {
		TokenParser[] peeks;
		if (!isEmpty()) {
			peeks ~= this;
			peeks[0].nextInSequence = null;
			peeks[0].subparsers = [];
		}


		 = map!(parser => parser.peek())(subparsers).array;
		if (isOptional && (nextInSequence !is null)) {
			peeks ~= (*nextInSequence).peek();
		}
		if (peeks.length == 0) return emptyParser();
		if (peeks.length == 1) return peeks[0];
		foreach (parser; peeks[1..$]) {
			if (!peeks[0].equalTo(parser)) return emptyParser();
		}
		return peeks[0];



		final switch (type) {
			case TokenParser.Type.EMPTY:
				break;
			//case TokenParser.Type.SEQUENCE:
				// peek & pop optionals
				//break;
			case TokenParser.Type.CHOICE:
				// peek & pop
				break;
			case TokenParser.Type.IDENTIFIER:
				break;
			case TokenParser.Type.CHAR:
				// peek & pop char sequences
				break;
			case TokenParser.Type.LIST:
				break;
		}
		return TokenParser();
	}

	TokenParser pop() {
		final switch (type) {
			case TokenParser.Type.EMPTY:
				break;
			//case TokenParser.Type.SEQUENCE:
				// peek & pop optionals
				//break;
			case TokenParser.Type.CHOICE:
				// peek & pop
				break;
			case TokenParser.Type.IDENTIFIER:
				break;
			case TokenParser.Type.CHAR:
				// peek & pop char sequences
				break;
			case TokenParser.Type.LIST:
				break;
		}
		return TokenParser();
	}

	// TODO: invariant - соответствие типа и заполненности других полей
}


auto sequence(TokenParser[] parsers...) {
	TokenParser result;
	//result.type = TokenParser.Type.SEQUENCE;
	result.subparsers = parsers;
	return result;
}

auto choice(TokenParser[] parsers...) {
	TokenParser result;
	result.type = TokenParser.Type.CHOICE;
	result.subparsers = parsers;
	return result;
}

auto optional(TokenParser[] parsers...) {
	TokenParser result;
	//result.type = TokenParser.Type.SEQUENCE;
	result.subparsers = parsers;
	result.isOptional = true;
	return result;
}

auto identifier(string fieldName) {
	TokenParser result;
	result.type = TokenParser.Type.IDENTIFIER;
	result.fieldType = "Identifier";
	result.fieldName = fieldName;
	return result;
}

auto charParser(string charCondition) {
	TokenParser result;
	result.type = TokenParser.Type.CHAR;
	result.charCondition = charCondition;
	return result;
}

auto charSequence(string s)(string fieldName = "") {
	TokenParser[] charParsers;
	foreach (c; s) {
		charParsers ~= charParser("%s=='" ~ c ~ "'");
	}
	charParsers[0].isFirstChar = true;
	return sequence(charParsers);
}

auto keyword(string s)() {
	TokenParser result = charSequence!s;
	result.subparsers ~= charParser("!isIdentifierChar(%s)");
	return result;
}

private auto list(T : Node!(args), args...)(string fieldName, string separator) {
	TokenParser result = sequence(args);
	result.type = TokenParser.Type.LIST;
	result.fieldType = T.stringof ~ "[]";
	result.fieldName = fieldName;
	return result;
}

auto dotList(itemParserSequence...)(string fieldName) {
	return list!itemParserSequence(fieldName, ".");
}

auto commaList(itemParserSequence...)(string fieldName) {
	return list!itemParserSequence(fieldName, ",");
}

auto field(T : Node!(args), args...)(string fieldName) {
	TokenParser result = sequence(args);
	result.fieldType = T.stringof;
	result.fieldName = fieldName;
	return result;
}



class Node(args...) {
	private static string generateFieldsCode(TokenParser[] parsers...) {
		string result;
		foreach (parser; parsers) {
			if ((parser.fieldType.length > 0) && (parser.fieldName.length > 0)) {
				result ~= parser.fieldType ~ " " ~ parser.fieldName ~ ";";
			} else {
				result ~= generateFieldsCode(parser.subparsers);
			}
		}
		return result;
	}

	public static string generateFieldsCodeDebug() { return generateFieldsCode(args); }

	mixin(generateFieldsCode(args));
}


struct Token {

}

class Identifier : Node!(
){};

class ModuleName : Node!(
	dotList!Identifier("parts")
){};

class ImportBinding : Node!(
	optional(field!Identifier("aliasName"), charSequence!"="),
	field!Identifier("symbolName")
){};

class Import : Node!(
	optional(field!Identifier("aliasName"), charSequence!"="),
	field!ModuleName("moduleName"),
	optional(charSequence!":", commaList!ImportBinding("bindings"))
){};

class ImportDeclaration : Node!(
	keyword!"import",
	commaList!Import("imports")
){};

class ModuleDeclaration : Node!(
	keyword!"module",
	field!ModuleName("moduleName")
){};


unittest {
	writeln(ModuleDeclaration.generateFieldsCodeDebug);
	writeln(ImportDeclaration.generateFieldsCodeDebug);
	writeln(Import.generateFieldsCodeDebug);
	writeln(ImportBinding.generateFieldsCodeDebug);
	writeln(ModuleName.generateFieldsCodeDebug);
}



string generateCode(TokenParser parser) {
	string result;
	final switch (parser.type) {
		case TokenParser.Type.EMPTY:
			return "";
		//case TokenParser.Type.SEQUENCE:
			// peek & pop optionals
			//break;
		case TokenParser.Type.CHOICE:
			// peek & pop
			break;
		case TokenParser.Type.IDENTIFIER:
			break;
		case TokenParser.Type.CHAR:
			return format(`if(%s){\%s}else{\%s}`, parser.charCondition);
		case TokenParser.Type.LIST:
			break;
	}
	return result;
}

string generateCode(T : Node!(args), args...)() {
	return generateCode(sequence(args));
}

unittest {
	writeln(generateCode!ModuleDeclaration);
	writeln(generateCode!ImportDeclaration);
	writeln(generateCode!Import);
	writeln(generateCode!ImportBinding);
	writeln(generateCode!ModuleName);
}




/*

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
	optional!(":", commaList!ImportBinding("bindings"))
){};

class ImportDeclaration : node!(
	"import",
	commaList!Import("imports")
){};

class ModuleDeclaration : node!(
	"module",
	field!ModuleName("moduleName")
){};


class TopLevelDaclaration : node!(
	choice!(
		field!ImportDeclaration("importDeclaration"),
		field!ModuleDeclaration("moduleDeclaration")
	)
){};




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
*/
