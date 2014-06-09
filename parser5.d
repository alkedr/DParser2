import std.stdio;
import std.typecons;
import std.algorithm;
import std.conv;
import std.string;
import std.array;



// TODO: НОДА MERGE!!!!!!

// TODO: pop() возвращает ноду, которую нужно вставить перед нодой, для которой вызвали pop


// FIXME: handle or at least detect choice of optionals
// FIXME: handle or at least detect sequence(list(contents, sep), sep)
// FIXME: detect choice(list(A, sep), list(A, sep))
// TODO: choice тоже может сходиться, как и optional








/*
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
		auto other = cast(const(CharParser))otherParser;
		return (other !is null) && (other.c == c);
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
	public string fieldType;
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


*/




T shallowCopyImpl(T)(T original) {
	auto result = new T;
	result.tupleof = original.tupleof;
	return result;
}

AbstractNode shallowCopy(AbstractNode original) {
	if (cast(AllocateASTNodeNode)original) return shallowCopyImpl(cast(AllocateASTNodeNode)original);
	if (cast(MergeNode)original) return shallowCopyImpl(cast(MergeNode)original);
	if (cast(EmptyNode)original) return shallowCopyImpl(cast(EmptyNode)original);
	if (cast(StartTokenNode)original) return shallowCopyImpl(cast(StartTokenNode)original);
	if (cast(CharNode)original) return shallowCopyImpl(cast(CharNode)original);
	if (cast(EndTokenNode)original) return shallowCopyImpl(cast(EndTokenNode)original);
	if (cast(IdentifierNode)original) return shallowCopyImpl(cast(IdentifierNode)original);
	if (cast(ListNode)original) return shallowCopyImpl(cast(ListNode)original);
	if (cast(OptionalNode)original) return shallowCopyImpl(cast(OptionalNode)original);
	if (cast(ChoiceNode)original) return shallowCopyImpl(cast(ChoiceNode)original);
	assert(0);
}



class AbstractNode {
	public string fieldType;
	public string fieldName;
	public int tmpVarIndex = -1;
	public AbstractNode prev;
	public AbstractNode next;

	protected abstract const(AbstractNode)[] peek() const;
	protected abstract void pop(const(AbstractNode) node, int tmpVarIndex);

	public abstract AbstractNode transform();

	protected bool sameAs(const(AbstractNode) other) const {
		return this.classinfo == other.classinfo;
	}

	public abstract string generateCode() const;
}


class AllocateASTNodeNode : SimpleNode {
	public override string generateCode() const {
		return format("%s = new %s;", fieldName, fieldType);
	}
}


class MergeNode : AbstractNode {
	public AbstractNode[] lastNodesOfBranches;

	protected override const(AbstractNode)[] peek() const {
		return next ? next.peek : [];
	}

	protected override void pop(const(AbstractNode) nodeToPop, int tmpVarIndex) {
		if (next /*&& next.peek contains nodeToPop*/) {
			next.pop(nodeToPop, tmpVarIndex);  // обновляет next
			// копируем next для каждой ветки
			foreach (lastNodeOfBranch; lastNodesOfBranches) {
				AbstractNode n = next.shallowCopy();
				n.prev = lastNodeOfBranch;
				n.next = this;
			}
			// удаляем next
			next = next.next;
			next.prev = this;
		}
	}

	public override AbstractNode transform() {
		if (next) next.transform();
		return this;
	}

	public override string generateCode() const {
		return "";
	}
}




class SimpleNode : AbstractNode {
	bool popped = false;

	protected override const(AbstractNode)[] peek() const {
		return popped ? (next ? next.peek : []) : [this];
	}

	protected override void pop(const(AbstractNode) node, int tmpVarIndex) {
		if (popped && next) {
			next.pop(node, tmpVarIndex);
		} else {
			popped = true;
			this.tmpVarIndex = tmpVarIndex;
		}
	}

	public override AbstractNode transform() {
		if (next) next.transform();
		return this;
	}
}

class EmptyNode : SimpleNode {
	public override string generateCode() const {
		return "";
	}
}

class StartTokenNode : SimpleNode {
	public override string generateCode() const {
		return "";
	}
}

class CharNode : SimpleNode {
	public dchar c;

	this() {}

	this(dchar c) {
		this.c = c;
	}

	protected override bool sameAs(const(AbstractNode) other) const {
		return super.sameAs(other) && ((cast(CharNode)other).c == c);
	}

	public override string generateCode() const {
		return "";
	}
}

class EndTokenNode : SimpleNode {
	public override string generateCode() const {
		return "";
	}
}

class IdentifierNode : SimpleNode {
	public override string generateCode() const {
		return "";
	}
}


class ComplexNode : AbstractNode {
	protected abstract inout(AbstractNode)[] choices() inout;

	public override const(AbstractNode)[] peek() const {
		return join(map!(choice => choice ? choice.peek : [])(choices));
	}
}

class ListNode : ComplexNode {
	public string separator;
	public AbstractNode contents;
	public bool isDoWhile = true;

	public override string generateCode() const {
		return "";
	}

	protected override inout(AbstractNode)[] choices() inout {
		return contents ? [ contents ] : [];
	}

	public override void pop(const(AbstractNode) node, int tmpVarIndex) {
		if (contents.sameAs(node)) {
			// TODO: copy content
			// listDoWhile(content, sep) => content, listWhile(content, sep)
			// listWhile(content, sep) => sep, content, listWhile(content, sep)
		}
	}

	public override AbstractNode transform() {
		if (next) next.transform();
		if (contents) contents.transform();
		return this;
	}
}

class OptionalNode : ComplexNode {
	public AbstractNode contents;

	public override string generateCode() const {
		return "";
	}

	public override inout(AbstractNode)[] choices() inout {
		return (contents ? [ contents ] : []) ~ (next ? [ next ] : []);
	}

	public override void pop(const(AbstractNode) node, int tmpVarIndex) {
		// TODO: opt(A), B => choice(AB, B)
	}

	public override AbstractNode transform() {
		if (next) next.transform();
		// TODO peek & pop choices
		return this;
	}
}

class ChoiceNode : ComplexNode {
	public AbstractNode[] _choices;

	public override string generateCode() const {
		return "";
	}

	public override inout(AbstractNode)[] choices() inout {
		return _choices;
	}

	public override void pop(const(AbstractNode) node, int tmpVarIndex) {
	}

	public override AbstractNode transform() {
		if (next) next.transform();
		// TODO peek & pop choices
		//
		return this;
	}
}


class ASTNode(alias node) {
}


struct Keyword { string keyword; }
struct identifier { string fieldName; }
struct field(T : ASTNode!(args), args...) { string fieldName; }
struct list(T : ASTNode!(args), args...) { string fieldName, separator; }
struct Sequence(args...) { args a; }
struct Choice(args...) { args a; }
struct Optional(args...) { args a; }

auto keyword(string s)() { return Keyword(s); }
auto dotList(T : ASTNode!(nodes), nodes...)(string fieldName) {
	return list!T(fieldName, ".");
}
auto commaList(T : ASTNode!(nodes), nodes...)(string fieldName) {
	return list!T(fieldName, ",");
}
auto sequence(A...)(A a) { return Sequence!A(a); }
auto choice(A...)(A a) { return Choice!A(a); }
auto optional(A...)(A a) { return Optional!A(a); }


//unittest {
//	class TestASTNode : ASTNode!(identifier("test")) {}
//	writeln(keyword!"qwe"());
//	writeln(identifier("qwe"));
//	writeln(field!TestASTNode("qwe"));
//	writeln(dotList!TestASTNode("qwe"));
//	writeln(commaList!TestASTNode("qwe"));
//	writeln(sequence(keyword!"a", keyword!"b"));
//	writeln(choice(keyword!"a", keyword!"b"));
//	writeln(optional(keyword!"a"));
//}


T createNode(T)(string fieldType = "", string fieldName = "") {
	auto result = new T;
	result.fieldType = fieldType;
	result.fieldName = fieldName;
	return result;
}

AbstractNode getLastNodeOfSequence(AbstractNode n) {
	while (n && n.next) n = n.next;
	return n;
}

AbstractNode getNodeBeforeLastNodeOfSequence(AbstractNode n) {
	while (n && n.next && n.next.next) n = n.next;
	return n;
}

AbstractNode sequenceOfParserNodes(AbstractNode[] nodes...) {
	if (nodes.length == 0) return new EmptyNode;
	for (int i = 1; i < nodes.length; i++) {
		AbstractNode end = getLastNodeOfSequence(nodes[i-1]);
		end.next = nodes[i];
		nodes[i].prev = end;
	}
	return nodes[0];
}

AbstractNode createParserNodes(T : ASTNode!(nodes), nodes...)() {
	return createParserNodes(sequence(nodes));  // TODO: кодогенератор для создания ноды
}

AbstractNode addStartEndTokenNodes(string fieldName, string fieldType, AbstractNode[] contents) {
	return sequenceOfParserNodes(
		new StartTokenNode ~
		contents ~
		createNode!EndTokenNode(fieldType, fieldName)
	);
}

AbstractNode createParserNodes(Keyword keyword) {
	return addStartEndTokenNodes("Token", keyword.keyword ~ "Keyword",
		map!(c => cast(AbstractNode)new CharNode(c))(keyword.keyword).array);
}

AbstractNode createParserNodes(identifier iden) {
	return createNode!IdentifierNode("Identifier", iden.fieldName);
}

AbstractNode createParserNodes(T : ASTNode!(args), args...)(field!T f) {
	return createParserNodes(sequence(args)/*, f.fieldName*/);
}

AbstractNode createParserNodes(T : ASTNode!(args), args...)(list!T l) {
	auto result = createNode!ListNode(T.stringof ~ "[]", l.fieldName);
	result.separator = l.separator;
	result.contents = createParserNodes(sequence(args));
	return result;
}

AbstractNode createParserNodes(Args...)(Sequence!Args seq) {
	static if (seq.a.length == 0) return null; else
	static if (seq.a.length == 1) return createParserNodes(seq.a[0]); else
	return sequenceOfParserNodes(
		createParserNodes(seq.a[0]),
		createParserNodes(sequence(seq.a[1..$]))
	);
}

AbstractNode createParserNodes(Args...)(Choice!Args ch) {
	auto result = new ChoiceNode;
	auto merge = new MergeNode;
	foreach (x; ch.a) {
		AbstractNode n = sequenceOfParserNodes(createParserNodes(x), merge);

		result._choices ~= sequenceOfParserNodes(createParserNodes(x), merge);
		AbstractNode end = result._choices[result._choices.length-1];
		while (end.next && end.next.next) end = end.next;
		merge.lastNodesOfBranches ~= end;
	}
	result.next = merge;
	merge.prev = result;
	return result;
}

AbstractNode createParserNodes(Args...)(Optional!Args opt) {
	auto result = new ChoiceNode;
	auto merge = new MergeNode;
	result._choices ~= merge;
	result._choices ~= sequenceOfParserNodes(createParserNodes(sequence(opt.a)), merge);
	AbstractNode end = result._choices[result._choices.length-1];
	while (end.next && end.next.next) end = end.next;
	result.next = merge;
	merge.prev = result;
	merge.lastNodesOfBranches ~= result;
	merge.lastNodesOfBranches ~= end;
	return result;
}



void writelnNode(AbstractNode n, string indent = "") {
	if (n) {
		write(indent);
		if (n.fieldName.length > 0) write(n.fieldType, " ", n.fieldName, " = ");
		if (n.tmpVarIndex >= 0) write("tmp", n.tmpVarIndex, " = ");
		write(n.classinfo.name, " ");
		if (auto cn = cast(CharNode)n) write("c = ", cn.c);
		if (auto cn = cast(ListNode)n) {
			writeln("sep = ", cn.separator);
			writelnNode(cn.contents, indent ~ "  ");
		}
		if (auto cn = cast(ChoiceNode)n) {
			writeln(indent, "_choices:");
			foreach (c; cn._choices) {
				writelnNode(c, indent ~ "  ");
				writeln();
			}
		}
		if (auto cn = cast(OptionalNode)n) {
			writeln(indent, "contents:");
			writelnNode(cn.contents, indent ~ "  ");
		}
		if (auto cn = cast(MergeNode)n) {
			writeln(cn.lastNodesOfBranches.length, " branches");
			foreach (c; cn.lastNodesOfBranches) {
				writeln(c.classinfo.name);
			}
		}

		writeln();
		if (n.next) writelnNode(n.next, indent);
	}
}


unittest {
	class TestASTNode : ASTNode!(identifier("test")) {}
	//writelnNode(createParserNodes(keyword!"qwe"()));
	//writeln();
	//writelnNode(createParserNodes(identifier("qwe")));
	//writeln();
	//writelnNode(createParserNodes(field!TestASTNode("qwe")));
	//writeln();
	//writelnNode(createParserNodes(dotList!TestASTNode("qwe")));
	//writeln();
	//writelnNode(createParserNodes(commaList!TestASTNode("qwe")));
	//writeln();
	//writelnNode(createParserNodes(sequence(keyword!"a", keyword!"b")));
	//writeln();
	writelnNode(createParserNodes(choice(keyword!"a", keyword!"b")));
	writeln();
	writelnNode(createParserNodes(optional(keyword!"a")));
}



unittest {
	//class TestASTNode : ASTNode!(identifier("test")) {}
	//writelnNode(createParserNodes(keyword!"qwe").transform);
	//writeln();
	//writelnNode(createParserNodes(identifier("qwe")).transform);
	//writeln();
	//writelnNode(createParserNodes(field!TestASTNode("qwe")).transform);
	//writeln();
	//writelnNode(createParserNodes(dotList!TestASTNode("qwe")).transform);
	//writeln();
	//writelnNode(createParserNodes(commaList!TestASTNode("qwe")).transform);
	//writeln();
	//writelnNode(createParserNodes(sequence(keyword!"a", keyword!"b")).transform);
	//writeln();
	//writelnNode(createParserNodes(choice(keyword!"a", keyword!"b")).transform);
	//writeln();
	//writelnNode(createParserNodes(optional(keyword!"a")).transform);
}





unittest {
	class Test { int a; }

	auto t = new Test;
	t.a = 1;

	auto t2 = new Test;
	t2.a = 2;
	t.tupleof = t2.tupleof;

	writeln(t.a);
}













/+

struct Node {
	enum Type {
		START_TOKEN,
		CHAR,
		END_TOKEN,
		IDENTIFIER,

		POPPED,
		LIST,
		OPTIONAL,
		CHOICE,
	}

	// common
	public Type type;
	public string fieldType;
	public string fieldName;
	public int tmpVarIndex = -1;
	public Node* prev;
	public Node* next;

	// for START_TOKEN, CHAR, END_TOKEN
	public string keyword;
	// for KEYWORD_CHAR
	public char c;

	// for LIST
	public char separator;

	// for LIST and OPTIONAL
	public Node* contents;

	// for CHOICE
	public Node*[] _choices;


	private inout(Node*)[] choices() inout {
		final switch (type) {
			case Type.POPPED  : return next ? next.choices() : [];
			case Type.LIST    : return [ contents ];
			case Type.OPTIONAL: return [ contents, next ];
			case Type.CHOICE  : return _choices;
			case Type.START_TOKEN:
			case Type.CHAR:
			case Type.END_TOKEN:
			case Type.IDENTIFIER:
				return [ &this ];
		}
	}

	public void pop(const(Node*) node, int tmpVarIndex) {
		final switch (type) {
			case Type.POPPED  :
			case Type.LIST    :
			case Type.OPTIONAL:
			case Type.CHOICE  :
				foreach (n; choices) n.pop(node, tmpVarIndex);
				break;
			case Type.START_TOKEN:
			case Type.CHAR:
			case Type.END_TOKEN:
			case Type.IDENTIFIER:
				type = Type.POPPED;
				this.tmpVarIndex = tmpVarIndex;
		}




		//if (type == Type.LIST    ) {

		//} else
		//if (type == Type.OPTIONAL) return [ contents, next ];
		//if (type == Type.CHOICE  ) return choices;
		//assert(node == this);
		//if (prev !is null) {
		//	prevChoice.choices = replace(prevChoice.choices, [this], [next]);
		//	prevChoice.choices = remove!((Node n) => n == null)(prevChoice.choices);
		//	if (prevOptional.contents == this) prevOptional.contents = next;
		//	if (prev.next == this) prev.next = next;
		//}
		//if (next !is null) next.prev = prev;
	}

	public void transform(int tmpVarIndex = 0) {
		if (next) next.transform(tmpVarIndex);
		//const(Node)[] peekResult = peek();
		//if (peekResult.length > 1) {
		//	// TODO: pop
		//}
	}
}


class ASTNode(alias node) {
}


Node* sequence(Node*[] nodes...) {
	nodes[0].prev = null;
	nodes[nodes.length-1].next = null;
	for (int i = 1; i < nodes.length; i++) {
		nodes[i-1].next = nodes[i];
		nodes[i].prev = nodes[i-1];
	}
	return nodes[0];
}

Node* field(T : ASTNode!(nodes), nodes...)(string fieldName) {
	auto result = sequence(nodes);
	result.fieldType = T.stringof;
	result.fieldName = fieldName;
	return result;
}

Node* list(T : ASTNode!(nodes), nodes...)(string fieldName, string separator) {
	auto result = Node();
	result.type = Node.Type.LIST;
	result.fieldType = T.stringof ~ "[]";
	result.fieldName = fieldName;
	result.separator = separator;
	result.contents = sequence(nodes);
	return result;
}

Node* dotList(T : ASTNode!(nodes), nodes...)(string fieldName) {
	return list!T(fieldName, ".");
}

Node* commaList(T : ASTNode!(nodes), nodes...)(string fieldName) {
	return list!T(fieldName, ",");
}

Node* optional(Node*[] nodes...) {
	auto result = new Node();
	result.type = Node.Type.OPTIONAL;
	result.contents = sequence(nodes);
	return result;
}

Node* choice(Node*[] nodes...) {
	auto result = new Node();
	result.type = Node.Type.CHOICE;
	result._choices = nodes;
	return result;
}

Node* startToken() {
	auto result = new Node();
	result.type = Node.Type.START_TOKEN;
	return result;
}

Node* endToken(string s) {
	auto result = new Node();
	result.type = Node.Type.END_TOKEN;
	result.fieldType = "Token";
	result.fieldName = s ~ "Keyword";
	return result;
}

Node*[] chars(string s) {
	Node*[] result;
	foreach (c; s) {
		auto n = new Node;
		n.type = Node.Type.CHAR;
		n.c = c;
		result ~= n;
	}
	return result;
}

Node* keyword(string keyword) {
	return sequence(startToken() ~ chars(keyword) ~ endToken(keyword));
}



void writelnNode(Node n) {
	writelnNode(&n);
}

void writelnNode(Node* n, string indent = "") {
	if (n) {
		if (n.fieldName.length > 0) write(indent, n.fieldType, " ", n.fieldName, " = ");
		if (n.tmpVarIndex >= 0) write(indent, "tmp", n.tmpVarIndex, " = ");
		write(indent, n.type, " ");
		if (n.keyword.length > 0) write(indent, "keyword = ", n.keyword, " ");
		if (n.c < 255) write(indent, "c = ", n.c, " ");
		if (n.separator < 255) write(indent, "separator = ", n.separator, " ");
		if (n.contents) {
			writeln(indent, "contents:");
			writelnNode(n.contents, indent ~ "  ");
		}
		if (n._choices.length > 0) {
			writeln(indent, "_choices:");
			foreach (c; n._choices) {
				writelnNode(c, indent ~ "  ");
				writeln();
			}
		}

		writeln();
		if (n.next) writelnNode(n.next, indent);
	}
}


Node f(Node*n) {return *n;}


//class TestNode : ASTNode!(*sequence(startToken(), endToken(""))) {

//}



unittest {
	//auto n = keyword("import");
	//writelnNode(n);

	//auto n2 = optional(choice(keyword("import"), keyword("immutable")));
	//writelnNode(n2);

	//auto n2 = commaList(choice(keyword("import"), keyword("immutable")));
	//writelnNode(n2);

	//n2 = dotList!TestNode("testField");
	//writelnNode(n2);

	//writelnNode(ASTNode!(Node()));
}



+/






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
