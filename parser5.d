import std.stdio;
import std.typecons;
import std.algorithm;
import std.conv;
import std.string;
import std.array;


class Parser {
	public abstract string code(string storage, string innerCode) const;
	public abstract bool isCompatible(const(Parser) other) const;
}

class IdentifierParser : Parser {
	public override string code(string storage, string innerCode) const {
		return format("if(isFirstIdentifierChar(currentChar)){%s=parseIdentifier;%s}", storage, innerCode);
	}

	public override bool isCompatible(const(Parser) otherParser) const {
		return (cast(const(IdentifierParser))otherParser !is null);
	}
}

class TokenBeginParser : Parser {
	public override string code(string storage, string innerCode) const {
		return innerCode;
	}

	public override bool isCompatible(const(Parser) otherParser) const {
		return (cast(const(TokenBeginParser))otherParser !is null);
	}
}

class CharParser : Parser {
	public char c;

	public override string code(string storage, string innerCode) const {
		return format("if(currentChar='%c'){%s}", c, innerCode);
	}

	public override bool isCompatible(const(Parser) otherParser) const {
		return (cast(const(CharParser))otherParser !is null) && ((cast(const(CharParser))otherParser).c == c);
	}
}

class TokenEndParser : Parser {
	public override string code(string storage, string innerCode) const {
		return innerCode;
	}

	public override bool isCompatible(const(Parser) otherParser) const {
		return (cast(const(TokenEndParser))otherParser !is null);
	}
}


class Node {
	public int tmpVarIndex;
	public string fieldName;
	public Node prev;
	public Node next;

	/// returns list of choices
	public abstract const(Parser)[] peek() const;

	/// pops nodes with parsers compatible to parser
	public abstract void pop(const(Parser) parser, int tmpVarIndex);

	public abstract string code() const;
}

class SimpleNode : Node {
	public Parser parser;

	public override const(Parser)[] peek() const {
		return [ parser ];
	}

	public override void pop(const(Parser) parser, int tmpVarIndex) {
		if (this.parser == parser) {
			this.parser = null;
			this.tmpVarIndex = tmpVarIndex;
		}
	}

	public override string code() const {
		// Если парсер есть, то он будет парсить прямо в нужную структуру
		// Если парсера нет, значит мы его pop'нули, и он сохранил результат во временной переменной
		return parser ? parser.code(fieldName, next ? next.code : "") : format("%s = tmp%d;\n", fieldName, tmpVarIndex);  // TODO: arrays, __traits compiles
	}
}

class ComplexNode : Node {
	public abstract inout(Node)[] choices() inout;

	public override const(Parser)[] peek() const {
		return join(map!((const Node n) => n.peek)(choices));
	}

	public override void pop(const(Parser) parser, int tmpVarIndex) {
		foreach (choice; choices) choice.pop(parser, tmpVarIndex);
	}
}

class ListNode : ComplexNode {
	public Node contents;

	public override inout(Node)[] choices() inout {
		return [ contents ];
	}

	public override string code() const {
		return null;
	}
}

class OptionalNode : ComplexNode {
	public Node contents;

	public override inout(Node)[] choices() inout {
		return [ contents, next ];
	}

	public override string code() const {
		return null;
	}
}

class ChoiceNode : ComplexNode {
	public Node[] _choices;

	public override inout(Node)[] choices() inout {
		return _choices;
	}

	public override string code() const {
		return null;
	}
}







// FIXME: handle or at least detect choice of optionals
/*
struct Node {
	enum Type {
		START_KEYWORD,
		KEYWORD_CHAR,
		END_KEYWORD,
		IDENTIFIER,
		LIST,
		OPTIONAL,
		CHOICE,
	}

	// common
	public Type type;
	public string fieldType;
	public string fieldName;
	public Node* prev;
	public Node* next;
	public Node* popped;

	// for START_KEYWORD, KEYWORD_CHAR, END_KEYWORD
	public string keyword;
	// for KEYWORD_CHAR
	public char c;

	// for LIST
	public char separator;

	// for LIST and OPTIONAL
	public Node* content;

	// for CHOICE
	public Node[] choices;


	private const(Node)[] peek() const {
		switch (type) {
			case Type.LIST    : return [ *content ];
			case Type.OPTIONAL: return [ *content, *next ];
			case Type.CHOICE  : return choices;
			default: return [ this ];
		}
	}

	public void pop(Node node) {
		if (type == Type.LIST    ) {

		} else
		if (type == Type.OPTIONAL) return [ content, next ];
		if (type == Type.CHOICE  ) return choices;
		assert(node == this);
		if (prev !is null) {
			prevChoice.choices = replace(prevChoice.choices, [this], [next]);
			prevChoice.choices = remove!((Node n) => n == null)(prevChoice.choices);
			if (prevOptional.content == this) prevOptional.content = next;
			if (prev.next == this) prev.next = next;
		}
		if (next !is null) next.prev = prev;
	}

	public void transform() {
		next.transform();
		const(Node)[] peekResult = peek();
		if (peekResult.length > 1) {
			// TODO: pop
		}
	}
}*/














/+


class Node {
	public string fieldType;
	public string fieldName;
	public Node prev;
	public Node next;

	public const(Node)[] peek() const {
		return [ this ];
	}

	public void pop(Node node) {
		if (node == this) {
			if (prev !is null) {
				if (auto prevChoice = cast(ChoiceNode)prev) {
					if (next is null) {
						prevChoice.choices = remove!((Node n) => n == this)(prevChoice.choices);
					} else {
						prevChoice.choices = replace(prevChoice.choices, [this], [next]);
					}
				} else if (auto prevOptional = cast(OptionalNode)prev) {
					if (prevOptional.content == this) prevOptional.content = next;
				}
				if (prev.next == this) {
					prev.next = next;
				}
			}
			if (next !is null) next.prev = prev;
		}
	}

	public final void transform() {
		next.transform();
		if (peek.length > 1) {
			// TODO: найти одинаковые и pop'нуть
		}
	}

	public abstract Node dup() const;

	public abstract string code() const;

	public abstract bool matches(Node other) const;

	protected T dupHelper(T)() const {
		T result = new T;
		result.fieldType = fieldType;
		result.fieldName = fieldName;
		result.prev = prev;
		result.next = next;
		return result;
	}
}

class StartKeywordNode : Node {
	public string keyword;

	public abstract Node dup() const {
		auto result = dupHelper!StartKeywordNode;
		result.keyword = keyword;
		return result;
	}

	public abstract string code() const {
		return "";
	}

	public abstract bool matches(Node otherNode) const {
		auto other = cast(StartKeywordNode)otherNode;
		if (other is null) return false;
		return keyword == other.keyword;
	}
}

class KeywordCharNode : Node {
	public string keyword;
	public char c;

	public abstract Node dup() const {
		auto result = dupHelper!KeywordCharNode;
		result.keyword = keyword;
		result.c = c;
		return result;
	}

	public abstract string code() const {
		return "";
	}

	public abstract bool matches(Node otherNode) const {
		auto other = cast(KeywordCharNode)otherNode;
		if (other is null) return false;
		return (keyword == other.keyword) && (c == other.c);
	}
}

class EndKeywordNode : Node {
	public string keyword;
}

class IdentifierNode : Node {
}

class ListNode : Node {
	public char separator;
	public Node content;

	override const(Node)[] peek() const {
		return content.peek();
	}

	override void pop(Node node) {
		Node copy = content.dup();
		copy.prev = prev;
		prev.next = copy;

		Node n = copy;
		while (n.next !is null) {
			n.next = n.next.dup();
			n = n.next;
		}
		n.next = this;
		prev = n;

		copy.pop(node);
	}
}

class OptionalNode : Node {
	public Node content;

	override const(Node)[] peek() const {
		return content.peek() ~ next.peek();
	}

	override void pop(Node node) {
		content.pop(node);
		next.pop(node);
	}
}

class ChoiceNode : Node {
	public Node[] choices;

	override const(Node)[] peek() const {
		return join(map!((const Node n) => n.peek)(choices));
	}

	override void pop(Node node) {
		foreach (choice; choices) choice.pop(choice);
	}
}

+/






















/+

/+
Нода - мапа  (путь -> (парсер, сохраняторВПоля, логика))

парсер и сохраняторВПоля могут перемещаться pop'ом

+/


class AbstractParser {
	string condition() const;
	string parsingCode() const;

	string code() const {
		return "if(" ~ condition ~ "){" ~ parsingCode ~ "}";
	}
}

class Node {
	AbstractParser parser;
	Node[] next;

	private Node[] getMatches() {
		foreach (i, a; next[0..$-1]) {
			Node[] result = [ a ];
			foreach (b; next[i+1..$]) {
				if (a.peek == b.peek) result ~= b;
			}
			if (result.length > 1) return result;
		}
		return [];
	}

	Node peek() const {
		if (parser !is null) return parser;
		return getMatches().length > 0 ? getMatches()[0].peek : null;
	}

	void pop() {
		if (parser !is null) {
			parser = null;
		} else {
			foreach (n; getMatches()) n.pop();
		}
	}

	string code() const {

	}
}





class NoOpNode : Node {
	Node peek() const {
		// search in choices
	}
	Node pop() {
		// search in choices
	}
	string condition() const {
		return "true";
	}
	string parsingCode() const {
		return "";
	}
}






class Node {
	Node[] choices;

	abstract Node peek() const;
	abstract Node pop();

	abstract string condition() const;
	abstract string parsingCode() const;

	string code() const {
		return "if(" ~ condition ~ "){" ~ parsingCode ~ "}";
	}
}

class NoOpNode : Node {
	Node peek() const {
		// search in choices
	}
	Node pop() {
		// search in choices
	}
	string condition() const {
		return "true";
	}
	string parsingCode() const {
		return "";
	}
}

class TokenStartNode : Node {
	Node peek() const {
		// return copy of this without choices
	}
}

class TokenEndNode : Node {
}

class ListStartNode : Node {
}

class ListEndNode : Node {
}

class OptionalStartNode : Node {
}

class OptionalEndNode : Node {
}

class IdentifierNode : Node {
}

class CharNode : Node {
}




// TODO: DAG!
struct Node {
	Node*[] choices;

	enum Type {
		START_OF_TOKEN,
		END_OF_TOKEN,

		IDENTIFIER,
		CHAR,

		START_OF_LIST,
		END_OF_LIST,
		START_OF_OPTIONAL,
		END_OF_OPTIONAL,
	}
}







struct DagNode {
	TokenParser*[] choices;

	enum Type {
		EMPTY,
		IDENTIFIER,
		CHAR,
		LIST,
	}

	Type type;
}





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
+/
