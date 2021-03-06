import std.stdio;
import std.typecons;
import std.algorithm;
import std.conv;
import std.string;
import std.array;


/*

Команды генератора:
	- keyword!"abc"("fieldName")         (сохраняет токен в поле abcKeyword или fieldName)
	- identifier("fieldName")
	- field!SomeNode("fieldName")
	- sequence(...)
	- choice(...)
	- optional(...)
	- list!(SomeNode, sep...)("fieldName")

Ноды генератора:
	- StartToken
	- EndToken
	- Char
	- Identifier

	- Choice
	- List

transform:
	- Choice - peek & pop choices, transform choices
	- List - transform contents and next
	- others - transform next

peek:
	- Choice - return join(peeks of choices)
	- List - return isDoWhile ? [contents.peek] :
	- others - transform next


*/




class AbstractParserNode {
	protected AbstractParserNode[] prevs;
	protected AbstractParserNode next;

	protected abstract AbstractParserNode[][string] peek(AbstractParserNode comingFrom);
	protected abstract AbstractParserNode pop(AbstractParserNode comingFrom,
		AbstractParserNode nodeToPop, int tmpVarIndex);

	public abstract AbstractParserNode transform();
	public abstract string generateCode() const;
}


class AbstractSimpleParserNode : AbstractParserNode {
	public override AbstractSimpleParserNode transform() {
		if (nexts.length > 1) {
			while (true) {
				auto choices = getChoices();
				// TODO: get all choices
				// TODO: find
			}
			// TODO: choice, peek & pop
		}
		foreach (ref next; nexts) {
			next = next.transform();
		}
		return this;
	}

	public override string generateCode() const {
		if (condition.empty) {
			return parsingCode;
		} else {
			return format("if(%s){%s}else{error();}", condition, parsingCode);
		}
	}

	protected override AbstractParserNode[][string] peek(AbstractParserNode comingFrom) {
		return [ condition: [this] ];
	}

	protected override AbstractParserNode pop(
		AbstractParserNode comingFrom,
		AbstractParserNode nodeToPop,
		int tmpVarIndex
	) {
		if (nodeToPop is this) {
			if (prevs.length > 1) {
				// TODO: copy this to each prev, return pop() on node that is after comingFrom
			} else {
				// TODO: return this with tmpVarIndex set and nexts and prevs unset
				// TODO: add moving from tmp var to result somewhere
			}
		}
		return null;
	}

	protected abstract string condition() const;
	protected abstract string parsingCode() const;

}


class StartParserNode {
}

class EndParserNode {
}

class StartTokenParserNode {
}

class EndTokenParserNode {
}

class CharParserNode {
}

class IdentifierParserNode {
}


class ListParserNode {
}












/*

class AbstractSimpleParserNode : AbstractParserNode {
	private AbstractParserNode[][string] getChoices() {
		AbstractParserNode[][string] result;
		foreach (next; nexts) {
			if (next.condition in result) {
				result[next.condition] ~= next;
			} else {
				result[next.condition] = [ next ];
			}
		}
		return result;
	}

	public override AbstractSimpleParserNode transform() {
		if (nexts.length > 1) {
			while (true) {
				auto choices = getChoices();
				// TODO: get all choices
				// TODO: find
			}
			// TODO: choice, peek & pop
		}
		foreach (ref next; nexts) {
			next = next.transform();
		}
		return this;
	}

	public override string generateCode() const {
		if (condition.empty) {
			return parsingCode;
		} else {
			return format("if(%s){%s}else{error();}", condition, parsingCode);
		}
	}

	protected override AbstractParserNode[][string] peek(AbstractParserNode comingFrom) {
		return [ condition: [this] ];
	}

	protected override AbstractParserNode pop(
		AbstractParserNode comingFrom,
		AbstractParserNode nodeToPop,
		int tmpVarIndex
	) {
		if (nodeToPop is this) {
			if (prevs.length > 1) {
				// TODO: copy this to each prev, return pop() on node that is after comingFrom
			} else {
				// TODO: return this with tmpVarIndex set and nexts and prevs unset
				// TODO: add moving from tmp var to result somewhere
			}
		}
		return null;
	}

}


class StartParserNode {
}

class EndParserNode {
}

class StartTokenParserNode {
}

class EndTokenParserNode {
}

class CharParserNode {
}

class IdentifierParserNode {
}


class ListParserNode {
}



*/





/+


class AbstractParserNode {
	protected AbstractParserNode next;
	public string fieldName;
	public string astNodeName;
	protected int tmpVarIndex = -1;

	protected this() {}

	protected this(typeof(this) that) {
		this.next = that.next;
		this.fieldName = that.fieldName;
		this.astNodeName = that.astNodeName;
		this.tmpVarIndex = that.tmpVarIndex;
	}

	protected abstract AbstractParserNode dup();
	protected abstract AbstractSimpleParserNode[] choices();
	protected abstract void pop(AbstractSimpleParserNode nodeToPop, ref int tmpVarIndex);

	public abstract AbstractParserNode transform();
	public abstract string generateCode() const;
}


class AbstractSimpleParserNode : AbstractParserNode {
	protected this() {}

	protected this(AbstractSimpleParserNode that) {
		super(that);
	}

	public override AbstractSimpleParserNode transform() {
		if (next) next = next.transform();
		return this;
	}

	public override string generateCode() const {
		return condition.length > 0 ?
			format("if(%s){%s}else{error();}", condition, parsingCode)
			: parsingCode;
	}

	protected override AbstractSimpleParserNode[] choices() {
		return [this];
	}

	protected override void pop(AbstractSimpleParserNode nodeToPop, ref int tmpVarIndex) {
		if (nodeToPop is this) {
			this.tmpVarIndex = tmpVarIndex++;
		}
	}

	protected abstract string condition() const;
	protected abstract string parsingCode() const;

	protected abstract bool sameParserAs(AbstractSimpleParserNode that);
}


class SimpleParserNode(T) : AbstractSimpleParserNode {
	protected this() {}

	protected this(T that) {
		super(that);
	}

	protected override bool sameParserAs(AbstractSimpleParserNode that) {
		return cast(T)that !is null;
	}

	protected override T dup() {
		return new T(cast(T)this);
	}
}


// optional => choice(contents, empty)
// прозрачен для peek/pop
final class EmptyParserNode : SimpleParserNode!EmptyParserNode {
	protected this() {}
	protected this(typeof(this) that) { super(that); }

	protected override string condition() const { return ""; }
	protected override string parsingCode() const { return ""; }
}

// keyword => sequence(start, chars, end)
// identifier => sequence(start, identifier, end)
final class StartTokenParserNode : SimpleParserNode!StartTokenParserNode {
	protected this() {}
	protected this(typeof(this) that) { super(that); }

	protected override string condition() const { return ""; }
	protected override string parsingCode() const { return "startToken;"; }
}

final class EndTokenParserNode : SimpleParserNode!EndTokenParserNode {
	protected this() {}
	protected this(typeof(this) that) { super(that); }

	protected override string condition() const { return ""; }
	protected override string parsingCode() const { return "endToken;"; }
}

final class CharParserNode : AbstractSimpleParserNode {
	protected dchar c;

	protected this(dchar c) { this.c = c; }
	protected this(typeof(this) that) { super(that); this.c = that.c; }

	protected override bool sameParserAs(AbstractSimpleParserNode that) {
		return (cast(CharParserNode)that !is null) && ((cast(CharParserNode)that).c == this.c);
	}

	protected override CharParserNode dup() {
		return new CharParserNode(this);
	}

	protected override string condition() const { return format("currentChar=='%c'", c); }
	protected override string parsingCode() const { return "advance;"; }
}

final class IdentifierParserNode : SimpleParserNode!IdentifierParserNode {
	protected this() {}
	protected this(typeof(this) that) { super(that); }

	protected override string condition() const { return "isIdentifierFirstChar(currentChar)"; }
	protected override string parsingCode() const { return "do{advance}while(isIdentifierChar(currentChar);"; }
}



class AbstractComplexParserNode : AbstractParserNode {
	protected this() {}

	protected this(typeof(this) that) {
		super(that);
	}

}

// генерирует код списков (циклы)
// pop: listDoWhile(contents, separator) => contents - listWhile(contents, separator)
// pop: listWhile(contents, separator) => separator - contents - listWhile(contents, separator)
final class ListParserNode : AbstractComplexParserNode {
	public string separator;
	public AbstractParserNode contents;
	public bool isDoWhile = true;

	protected this() {}

	protected this(typeof(this) that) {
		super(that);
		this.separator = that.separator;
		this.contents = that.contents;
		this.isDoWhile = that.isDoWhile;
	}

	public override AbstractParserNode transform() {
		if (next) next = next.transform();
		// TODO
		return this;
	}

	public override string generateCode() const {
		// TODO
		return "";
	}

	protected override typeof(this) dup() {
		auto result = new typeof(this);
		result.separator = this.separator;
		result.contents = this.contents;
		result.isDoWhile = this.isDoWhile;
		return result;
	}

	protected override AbstractSimpleParserNode[] choices() {
		return contents.choices;
	}

	protected override void pop(AbstractSimpleParserNode nodeToPop, ref int tmpVarIndex) {
		//TODO
	}
}

// pop: choice(A, BC, BD) => choice(A, B - choice(C, D))
final class ChoiceParserNode : AbstractComplexParserNode {
	public AbstractParserNode[] _choices;

	protected this() {}

	protected this(typeof(this) that) {
		super(that);
		this._choices = that._choices;
	}

	public override AbstractParserNode transform() {
		if (next) next = next.transform();
		// TODO
		return this;
	}

	public override string generateCode() const {
		// TODO
		return "";
	}

	protected override typeof(this) dup() {
		auto result = new typeof(this);
		result._choices = this._choices;
		return result;
	}

	protected override AbstractSimpleParserNode[] choices() {
		return join(map!(node => node.choices)(_choices).array);
	}

	protected override void pop(AbstractSimpleParserNode nodeToPop, ref int tmpVarIndex) {
		//TODO
	}
}

// pop: copies next node into every merged branch
final class MergeBranchesParserNode : AbstractComplexParserNode {
	public AbstractParserNode[] lastNodesOfBranches;

	protected this() {}

	protected this(typeof(this) that) {
		super(that);
		this.lastNodesOfBranches = that.lastNodesOfBranches;
	}

	public override AbstractParserNode transform() {
		if (next) next = next.transform();
		// TODO
		return this;
	}

	public override string generateCode() const {
		// TODO
		return "";
	}

	protected override MergeBranchesParserNode dup() {
		auto result = new typeof(this);
		result.lastNodesOfBranches = this.lastNodesOfBranches;
		return result;
	}

	protected override AbstractSimpleParserNode[] choices() {
		return next.choices;
	}

	protected override void pop(AbstractSimpleParserNode nodeToPop, ref int tmpVarIndex) {
		//TODO
	}
}






class ASTNode(alias node) {
}


struct Empty {}
struct StartToken {}
struct Char(char c) {}
struct EndToken {}
struct Identifier { string fieldName; }
struct field(T : ASTNode!(args), args...) { string fieldName; }
struct list(T : ASTNode!(args), args...) { string fieldName, separator; }
struct Sequence(args...) { args a; }
struct Choice(args...) { args a; }

auto charSequence(string s, A...)(A a) {
	static if (s.length > 0) {
		return charSequence!(s[1..$], A, Char!(s[0]))(a, Char!(s[0])());
	} else {
		return sequence(a);
	}
}
auto keyword(string s)() { return sequence(StartToken(), charSequence!(s)(), EndToken()); }
auto identifier(string s) { return sequence(StartToken(), Identifier(s), EndToken()); }
auto dotList(T : ASTNode!(nodes), nodes...)(string fieldName) {
	return list!T(fieldName, ".");
}
auto commaList(T : ASTNode!(nodes), nodes...)(string fieldName) {
	return list!T(fieldName, ",");
}
auto sequence(A...)(A a) { return Sequence!A(a); }
auto choice(A...)(A a) { return Choice!A(a); }
auto optional(A...)(A a) { return choice(a, Empty()); }


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


T createNode(T)(string fieldName) {
	auto result = new T;
	result.fieldName = fieldName;
	return result;
}

AbstractParserNode lastNodeOfSequence(AbstractParserNode n) {
	while (n && n.next) n = n.next;
	return n;
}

AbstractParserNode nodeBeforeLastNodeOfSequence(AbstractParserNode n) {
	while (n && n.next && n.next.next) n = n.next;
	return n;
}

AbstractParserNode sequenceOfParserNodes(AbstractParserNode[] nodes...) {
	if (nodes.length == 0) return new EmptyParserNode;
	for (int i = 1; i < nodes.length; i++) {
		lastNodeOfSequence(nodes[i-1]).next = nodes[i];
	}
	return nodes[0];
}

AbstractParserNode createParserNodes(T : ASTNode!(nodes), nodes...)() {
	return createParserNodes(sequence(nodes));  // TODO: кодогенератор для создания ноды
}

AbstractParserNode createParserNodes(Empty empty, string fieldNamePrefix = "") {
	return new EmptyParserNode;
}

AbstractParserNode createParserNodes(StartToken s, string fieldNamePrefix = "") {
	return new StartTokenParserNode;
}

AbstractParserNode createParserNodes(char c)(Char!c s, string fieldNamePrefix = "") {
	return new CharParserNode(c);
}

AbstractParserNode createParserNodes(EndToken s, string fieldNamePrefix = "") {
	return new EndTokenParserNode;
}

AbstractParserNode createParserNodes(Identifier identifier, string fieldNamePrefix = "") {
	return createNode!IdentifierParserNode(fieldNamePrefix ~ identifier.fieldName);
}

AbstractParserNode createParserNodes(T : ASTNode!(args), args...)(field!T f, string fieldNamePrefix = "") {
	AbstractParserNode result = createParserNodes(sequence(args), fieldNamePrefix ~ f.fieldName ~ ".");
	result.astNodeName = fieldNamePrefix ~ f.fieldName;
	return result;
}

AbstractParserNode createParserNodes(T : ASTNode!(args), args...)(list!T l, string fieldNamePrefix = "") {
	auto result = createNode!ListParserNode(fieldNamePrefix ~ l.fieldName);
	result.separator = l.separator;
	result.contents = createParserNodes(sequence(args));
	return result;
}

AbstractParserNode createParserNodes(Args...)(Sequence!Args seq, string fieldNamePrefix = "") {
	static if (seq.a.length == 0) return null; else
	static if (seq.a.length == 1) return createParserNodes(seq.a[0]); else
	return sequenceOfParserNodes(
		createParserNodes(seq.a[0]),
		createParserNodes(sequence(seq.a[1..$]))
	);
}

AbstractParserNode createParserNodes(Args...)(Choice!Args ch, string fieldNamePrefix = "") {
	auto result = new ChoiceParserNode;
	auto merge = new MergeBranchesParserNode;
	foreach (x; ch.a) {
		AbstractParserNode n = sequenceOfParserNodes(createParserNodes(x), merge);

		result._choices ~= sequenceOfParserNodes(createParserNodes(x), merge);
		AbstractParserNode end = result._choices[result._choices.length-1];
		while (end.next && end.next.next) end = end.next;
		merge.lastNodesOfBranches ~= end;
	}
	result.next = merge;
	return result;
}



void writelnNodeImpl(AbstractParserNode n, string indent = "", bool stopOnMerge = false) {
	if (n) {
		if (cast(MergeBranchesParserNode)n !is null) return;
		write(indent);
		if (n.fieldName.length > 0) write(n.fieldName, " = ");
		if (n.tmpVarIndex >= 0) write("tmp", n.tmpVarIndex, " = ");
		write(n.classinfo.name, " ");
		//if (auto cn = cast(CharParserNode)n) write("c = ", cn.c);
		if (auto cn = cast(ListParserNode)n) {
			writeln("sep = ", cn.separator);
			writelnNodeImpl(cn.contents, indent ~ "  ");
		}
		if (auto cn = cast(ChoiceParserNode)n) {
			writeln(indent, "_choices:");
			foreach (c; cn._choices) {
				writelnNodeImpl(c, indent ~ "  ", true);
				writeln();
			}
		}
		//if (auto cn = cast(OptionalParserNode)n) {
		//	writeln(indent, "contents:");
		//	writelnNodeImpl(cn.contents, indent ~ "  ");
		//}
		if (auto cn = cast(MergeBranchesParserNode)n) {
			writeln(cn.lastNodesOfBranches.length, " branches");
			foreach (c; cn.lastNodesOfBranches) {
				writeln(c.classinfo.name);
			}
		}

		writeln();
		if (n.next) writelnNodeImpl(n.next, indent);
	}
}

void writelnNode(AbstractParserNode n) {
	writeln("==============================");
	writelnNodeImpl(n);
	writeln("==============================");
	writeln();
}


unittest {
	class TestASTNode : ASTNode!(identifier("test")) {}
	writelnNode(createParserNodes(keyword!"qwe").transform);
	writelnNode(createParserNodes(identifier("qwe")).transform);
	writelnNode(createParserNodes(field!TestASTNode("qwe")).transform);
	writelnNode(createParserNodes(dotList!TestASTNode("qwe")).transform);
	writelnNode(createParserNodes(commaList!TestASTNode("qwe")).transform);
	writelnNode(createParserNodes(sequence(keyword!"a", keyword!"b")).transform);
	writelnNode(createParserNodes(choice(keyword!"a", keyword!"b")).transform);
	writelnNode(createParserNodes(optional(keyword!"a")).transform);
}
+/
