package pp.block5.cc.simple;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;

/** Class holding the results of the Simple Pascal checker. */
public class Result {

	/** Mapping from statements and expressions to the atomic
	 * subtree that is their entry in the control flow graph. */
	private final ParseTreeProperty<ParserRuleContext> entries = new ParseTreeProperty<>();
	/** Mapping from expressions to types. */
	private final ParseTreeProperty<Type> types = new ParseTreeProperty<>();
	/** Mapping from variables to coordinates. */
	private final ParseTreeProperty<Integer> offsets = new ParseTreeProperty<>();

	/** Adds an association from parse tree node to the flow graph entry. */
	public void setEntry(ParseTree node, ParserRuleContext entry) {
		this.entries.put(node, entry);
	}

	/** Returns the flow graph entry associated 
	 * with a given parse tree node. */
	public ParserRuleContext getEntry(ParseTree node) {
		return this.entries.get(node);
	}

	/** Adds an association from a parse tree node containing a 
	 * variable reference to the offset
	 * of that variable. */
	public void setOffset(ParseTree node, int offset) {
		this.offsets.put(node, offset);
	}

	/** Returns the declaration offset of the variable 
	 * accessed in a given parse tree node. */
	public int getOffset(ParseTree node) {
		return this.offsets.get(node);
	}

	/** Adds an association from a parse tree expression, type,
	 * or assignment target node to the corresponding (inferred) type. */
	public void setType(ParseTree node, Type type) {
		this.types.put(node, type);
	}

	/** Returns the type associated with a given parse tree node. */
	public Type getType(ParseTree node) {
		return this.types.get(node);
	}
}
