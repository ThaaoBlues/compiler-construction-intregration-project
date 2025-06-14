package pp.block5.cc.simple;

import java.util.ArrayList;
import java.util.List;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import pp.block5.cc.ParseException;
import pp.block5.cc.pascal.SimplePascalBaseListener;
import pp.block5.cc.pascal.SimplePascalParser;
import pp.block5.cc.pascal.SimplePascalParser.BoolExprContext;
import pp.block5.cc.pascal.SimplePascalParser.CompExprContext;
import pp.block5.cc.pascal.SimplePascalParser.FalseExprContext;
import pp.block5.cc.pascal.SimplePascalParser.IdExprContext;
import pp.block5.cc.pascal.SimplePascalParser.MultExprContext;
import pp.block5.cc.pascal.SimplePascalParser.NumExprContext;
import pp.block5.cc.pascal.SimplePascalParser.ParExprContext;
import pp.block5.cc.pascal.SimplePascalParser.PlusExprContext;
import pp.block5.cc.pascal.SimplePascalParser.PrfExprContext;
import pp.block5.cc.pascal.SimplePascalParser.TrueExprContext;

/** Class to type check and calculate flow entries and variable offsets. */
public class Checker extends SimplePascalBaseListener {

	/** Result of the latest call of {@link #check}. */
	private Result result;
	/** Variable scope for the latest call of {@link #check}. */
	private Scope scope;
	/** List of errors collected in the latest call of {@link #check}. */
	private List<String> errors;

	/** Runs this checker on a given parse tree,
	 * and returns the checker result.
	 * @throws ParseException if an error was found during checking.
	 */
	public Result check(ParseTree tree) throws ParseException {
		this.scope = new Scope();
		this.result = new Result();
		this.errors = new ArrayList<>();
		new ParseTreeWalker().walk(this, tree);
		if (hasErrors()) {
			throw new ParseException(getErrors());
		}
		return this.result;
	}

	@Override
	public void exitBody(SimplePascalParser.BodyContext ctx){
		setEntry(ctx,entry(ctx.block()));
	}

	@Override
	public void enterBlock(SimplePascalParser.BlockContext ctx){
		setEntry(ctx,ctx);
	}

	@Override
	public void exitBlock(SimplePascalParser.BlockContext ctx){
			setEntry(ctx,entry(ctx.stat(0)));
	}

	@Override
	public void exitVar(SimplePascalParser.VarContext ctx){

		// variables offsets and types
		for(int i = 0;i<ctx.ID().size();i++){
			result.setOffset(ctx.ID(i),i*4);
			switch (ctx.type().getText()){
				case "Integer":
					result.setType(ctx.ID(i),Type.INT);
					scope.put(ctx.ID(i).getText(),Type.INT);
				break;
				case "Boolean":
					result.setType(ctx.ID(i),Type.BOOL);
					scope.put(ctx.ID(i).getText(),Type.BOOL);
					break;
			}
		}

		//setEntry(ctx,entry(ctx.getParent().getChild(1)));
	}

	@Override
	public void exitAssStat(SimplePascalParser.AssStatContext ctx){
		String id = ctx.target().getChild(0).getText();
		Type type = this.scope.type(id);
		if (type == null) {
			addError(ctx, "Variable '%s' not declared", id);
		} else {
			setType(ctx.target(), type);
			setOffset(ctx.target(), this.scope.offset(id));
			//setEntry(ctx.target(), ctx);
			//System.out.println("Setting statement entry to :"+entry(ctx.expr()));

			setEntry(ctx,entry(ctx.expr()));
			//setEntry(ctx, ctx);
		}

	}

	@Override
	public void exitIfStat(SimplePascalParser.IfStatContext ctx){

		checkType(ctx.expr(),Type.BOOL);
		setEntry(ctx,entry(ctx.stat(0)));
		setEntry(ctx,entry(ctx.stat(1)));

	}

	@Override
	public void exitWhileStat(SimplePascalParser.WhileStatContext ctx){

		// while body
		setEntry(ctx,entry(ctx.stat()));
		// looping itself
		setEntry(ctx,ctx);
		// while end
		setEntry(ctx,entry(ctx.getParent()));
	}

	@Override
	public void exitBlockStat(SimplePascalParser.BlockStatContext ctx){
		setEntry(ctx,entry(ctx.block()));

	}

	@Override
	public void exitInStat(SimplePascalParser.InStatContext ctx){
		String id = ctx.target().getChild(0).getText();
		Type type = this.scope.type(id);
		if (type == null) {
			addError(ctx, "Variable '%s' not declared", id);
		} else {
			setType(ctx, type);
			setOffset(ctx, this.scope.offset(id));
			setEntry(ctx, ctx);
		}
	}

	@Override
	public void exitOutStat(SimplePascalParser.OutStatContext ctx){
		setEntry(ctx, entry(ctx.expr()));
	}

	// Override the listener methods for the statement nodes
	@Override
	public void exitBoolExpr(BoolExprContext ctx) {
		checkType(ctx.expr(0), Type.BOOL);
		checkType(ctx.expr(1), Type.BOOL);
		setType(ctx, Type.BOOL);
		setEntry(ctx, entry(ctx.expr(0)));
	}

	@Override
	public void exitCompExpr(CompExprContext ctx) {
		checkType(ctx.expr(0), Type.INT);
		checkType(ctx.expr(1), Type.INT);
		setType(ctx, Type.BOOL);
		setEntry(ctx, entry(ctx.expr(0)));
	}

	@Override
	public void exitFalseExpr(FalseExprContext ctx) {
		setType(ctx, Type.BOOL);
		setEntry(ctx, ctx);
	}

	@Override
	public void exitIdExpr(IdExprContext ctx) {
		String id = ctx.ID().getText();
		Type type = this.scope.type(id);
		if (type == null) {
			addError(ctx, "Variable '%s' not declared", id);
		} else {
			setType(ctx, type);
			setOffset(ctx, this.scope.offset(id));
			setEntry(ctx, ctx);
		}
	}

	@Override
	public void exitMultExpr(MultExprContext ctx) {
		checkType(ctx.expr(0), Type.INT);
		checkType(ctx.expr(1), Type.INT);
		setType(ctx, Type.INT);
		setEntry(ctx, entry(ctx.expr(0)));
	}

	@Override
	public void exitNumExpr(NumExprContext ctx) {
		setType(ctx, Type.INT);
		setEntry(ctx, ctx);
	}

	@Override
	public void exitParExpr(ParExprContext ctx) {
		setType(ctx, getType(ctx.expr()));
		setEntry(ctx, entry(ctx.expr()));
	}

	@Override
	public void exitPlusExpr(PlusExprContext ctx) {
		checkType(ctx.expr(0), Type.INT);
		checkType(ctx.expr(1), Type.INT);
		setType(ctx, Type.INT);
		setEntry(ctx, entry(ctx.expr(0)));
	}

	@Override
	public void exitPrfExpr(PrfExprContext ctx) {
		Type type;
		if (ctx.prfOp().MINUS() != null) {
			type = Type.INT;
		} else {
			assert ctx.prfOp().NOT() != null;
			type = Type.BOOL;
		}
		checkType(ctx.expr(), type);
		setType(ctx, type);
		setEntry(ctx, entry(ctx.expr()));
	}

	@Override
	public void exitTrueExpr(TrueExprContext ctx) {
		setType(ctx, Type.BOOL);
		setEntry(ctx, ctx);
	}

	/** Indicates if any errors were encountered in this tree listener. */
	public boolean hasErrors() {
		return !getErrors().isEmpty();
	}

	/** Returns the list of errors collected in this tree listener. */
	public List<String> getErrors() {
		return this.errors;
	}

	/** Checks the inferred type of a given parse tree,
	 * and adds an error if it does not correspond to the expected type.
	 */
	private void checkType(ParserRuleContext node, Type expected) {
		Type actual = getType(node);
		if (actual == null) {
			throw new IllegalArgumentException("Missing inferred type of " + node.getText());
		}
		if (!actual.equals(expected)) {
			addError(node, "Expected type '%s' but found '%s'", expected, actual);
		}
	}

	/** Records an error at a given parse tree node.
	 * @param node the parse tree node at which the error occurred
	 * @param message the error message
	 * @param args arguments for the message, see {@link String#format}
	 */
	private void addError(ParserRuleContext node, String message, Object... args) {
		addError(node.getStart(), message, args);
	}

	/** Records an error at a given token.
	 * @param token the token at which the error occurred
	 * @param message the error message
	 * @param args arguments for the message, see {@link String#format}
	 */
	private void addError(Token token, String message, Object... args) {
		int line = token.getLine();
		int column = token.getCharPositionInLine();
		message = String.format(message, args);
		message = String.format("Line %d:%d - %s", line, column, message);
		this.errors.add(message);
	}

	/** Convenience method to add an offset to the result. */
	private void setOffset(ParseTree node, Integer offset) {
		this.result.setOffset(node, offset);
	}

	/** Convenience method to add a type to the result. */
	private void setType(ParseTree node, Type type) {
		this.result.setType(node, type);
	}

	/** Returns the type of a given expression or type node. */
	private Type getType(ParseTree node) {
		return this.result.getType(node);
	}

	/** Convenience method to add a flow graph entry to the result. */
	private void setEntry(ParseTree node, ParserRuleContext entry) {
		if (entry == null) {
			throw new IllegalArgumentException("Null flow graph entry");
		}
		this.result.setEntry(node, entry);
	}

	/** Returns the flow graph entry of a given expression or statement. */
	private ParserRuleContext entry(ParseTree node) {
		return this.result.getEntry(node);
	}
}
