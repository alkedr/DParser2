import std.stdio;
import std.typecons;
import std.algorithm;
import std.conv;
import std.string;
import std.array;


/*


class AbstractParserNode {
	public dstring fieldType;
	public dstring[] fieldName;   // ["nodeA", "nodeB", "nodeC"] for nodeA.nodeB.nodeC
	public AbstractParserNode[] prev;
	public AbstractParserNode next;

	public abstract AbstractParserNode transform();
	protected abstract dstring condition() const;
	protected abstract dstring parserCode() const;

	public final dstring generateCode(
		dstring beforeIf, dstring beforeParser,
		dstring afterParser, dstring afterIf
	) {
		return beforeIf ~
			((condition.length > 0) ? "if(" ~ condition ~ ")" : "") ~
			"{" ~ beforeParser ~ parserCode ~ afterParser ~ "}" ~
			afterIf;
	}

	public final AbstractParserNode followedBy(AbstractParserNode node) {
		next = node;
		return node;
	}
}

class AbstractSimpleNode : AbstractParserNode {
	public final override AbstractParserNode transform() {
		next.transform;
		return this;
	}
}

class DumbPieceOfCodeNode(dstring code) : AbstractSimpleNode {
	public final override dstring condition() { return ""; }
	public final override dstring parserCode() { return code; }
}

class StartNode : DumbPieceOfCodeNode!"start;" {}

class EndNode : DumbPieceOfCodeNode!"end;" {}

class StartTokenNode : DumbPieceOfCodeNode!"startToken;" {}

class EndTokenNode : AbstractSimpleNode {
	public final override dstring condition() { return "current"; }
	public final override dstring parserCode() { return code; }

	public override dstring generateCode() {
		return join(fieldName) ~ " = endToken;"d;
	}
}

class CharNode(dchar c) : DumbPieceOfCodeNode!"" {
}

class IdentifierNode : AbstractSimpleNode {
	public override dstring generateCode() {
		return "parseIdentifier;";
	}
}


*/
