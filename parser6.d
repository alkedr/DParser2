import std.stdio;
import std.typecons;
import std.algorithm;
import std.conv;
import std.string;
import std.array;



// Данные в структурах

class AbstractParserNode {
	public abstract void transform();
	public abstract string generateCode() const;

	public T void dup(T = AbstractParserNode)() {
		auto
	}
}

class AbstractParserNodeWithData(FieldData, ParserData) {
	protected FieldData fd;
	protected ParserData pd;
}



class ParserNode(ParserStruct) : AbstractParserNode {
	public AbstractParserNode next;
	private FieldStruct fs;
	private ParserStruct ps;

	this(ParserStruct ps) { this.ps = ps; }

	public override void transform() { ps.transform(); }
	public override string generateCode() const { return ps.generateCode(); }
}


struct FieldStruct {
	public string fieldType;
	public string fieldName;
}

struct StartTokenStruct {
	public void transform() {
	}

	public string generateCode() const {
		return "";
	}
}

const auto x = new ParserNode!StartTokenStruct(StartTokenStruct());



class AbstractNode {
	public string fieldType;
	public string fieldName;
	public int tmpVarIndex = -1;  //??
	public AbstractNode next;

	protected abstract const(AbstractNode)[] peek() const;
	protected abstract void pop(const(AbstractNode) node, int tmpVarIndex);

	public abstract AbstractNode transform();

	protected bool sameAs(const(AbstractNode) other) const {
		return this.classinfo == other.classinfo;
	}

	public abstract string generateCode() const;
}
