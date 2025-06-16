package pp.block5.cc.simple;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;

import pp.block5.cc.pascal.ProcPascalParser;
import pp.block5.cc.pascal.SimplePascalBaseVisitor;
import pp.block5.cc.pascal.SimplePascalParser;
import pp.iloc.Simulator;
import pp.iloc.model.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Stack;

/** Class to generate ILOC code for Simple Pascal. */
public class Generator extends SimplePascalBaseVisitor<Op> {

	/** The representation of the boolean value <code>false</code>. */
	public final static Num FALSE_VALUE = new Num(Simulator.FALSE);
	/** The representation of the boolean value <code>true</code>. */
	public final static Num TRUE_VALUE = new Num(Simulator.TRUE);

	/** The base register. */
	private Reg arp = new Reg("r_arp");
	/** The outcome of the checker phase. */
	private Result checkResult;
	/** Association of statement nodes to labels. */
	private ParseTreeProperty<Label> labels;
	/** The program being built. */
	private Program prog;
	/** Register count, used to generate fresh registers. */
	private int regCount;
	/** Association of expression and target nodes to registers. */
	private ParseTreeProperty<Reg> regs;

	private Map<String,Reg> vars = new HashMap<>();

	private Stack<String> labelStack = new Stack<>();

	/** Generates ILOC code for a given parse tree,
	 * given a pre-computed checker result.
	 */
	public Program generate(ParseTree tree, Result checkResult) {
		this.prog = new Program();
		this.checkResult = checkResult;
		this.regs = new ParseTreeProperty<>();
		this.labels = new ParseTreeProperty<>();
		this.regCount = 0;
		tree.accept(this);
		return this.prog;
	}



	// Override the visitor methods


	public Op visitProgram(SimplePascalParser.ProgramContext ctx) {
		return visit(ctx.body());
	}

	public Op visitBody(SimplePascalParser.BodyContext ctx) {

		for(int i = 0;i<ctx.decl().size();i++){
			visit(ctx.decl(i));
		}

		Op ret = visit(ctx.block());

		// flush label stack if end of program right after a while or an if (or a combination of n of them ).
		while (!labelStack.isEmpty()){
			Op o = emit(OpCode.nop);
			o.setLabel(new Label(labelStack.pop()));
		}
		return ret;
	}

	public Op visitVarDecl(SimplePascalParser.VarDeclContext ctx) {
		for(int i = 0;i<ctx.var().size();i++){
			visit(ctx.var(i));
		}
		return null;
	}

	public Op visitVar(SimplePascalParser.VarContext ctx) {
		// on déclarera quand on assignera une valeur par un load
		for(int i = 0;i<ctx.ID().size();i++){
			Reg r = reg(ctx.ID(i));
			vars.put(ctx.ID(i).getText(),r);
			emit(OpCode.loadI,new Num(0),r);
		}
		return null;
	}

	public Op visitBlock(SimplePascalParser.BlockContext ctx) {
		Op ret = null;
		for(int i =0;i<ctx.stat().size();i++){
			Op tmp = visit(ctx.stat(i));

			if(i == 0) ret = tmp;

			// STAT WAS A WHILE !!!
			// Create end of while block if there is a thing after
			if(tmp.getOpCode() == OpCode.jumpI){


				/*if(i != ctx.stat().size()-1){
					i++;
					tmp = visit(ctx.stat(i));
					tmp.setLabel(new Label("EOW"));
				}else{
					// while was the last statement
					// artificial supplementary block that just sends control to end of program
					//tmp = emit(OpCode.jumpI,new Label("END"));
					//tmp.setLabel(new Label("EOW"));
				}*/

			}else if(Objects.equals(tmp.getComment(), "IF BLOCK")){
				//labelStack.push("AFTER_IF");
				if(i != ctx.stat().size()-1){
					//i++;
					//tmp = visit(ctx.stat(i));
					//tmp.setLabel(new Label("AFTER_IF"));
				}else{


					// IF was the last statement
					// artificial supplementary block that just sends control to end of program
					//labelStack.push("AFTER_IF");
					//tmp = emit(OpCode.jumpI,new Label("END"));
					//tmp.setLabel(new Label("AFTER_IF"));
				}
			}
		}

		return ret;
	}

	public Op visitAssStat(SimplePascalParser.AssStatContext ctx) {


		Op rs = visit(ctx.expr());

		if(!labelStack.isEmpty()){
			rs.setLabel(new Label(labelStack.pop()));
		}


		Reg val_reg =reg(ctx.expr());

		// will associate the reg with the variable name also
		visit(ctx.target());

		Reg r3 = reg(ctx.target());

		return emit(OpCode.i2i,val_reg,r3);


	}

	public Op visitIfStat(SimplePascalParser.IfStatContext ctx) {
		Op cmp = visit(ctx.expr());

		if(!labelStack.isEmpty()){
			if(cmp != null){
				cmp.setLabel(new Label(labelStack.pop()));
			}
		}

		//cmp.setLabel(new Label("IF"));

		Op ret = emit(OpCode.cbr,reg(ctx.expr()),new Label("THEN"), new Label("ELSE"));
		if(!labelStack.isEmpty()){
			ret.setLabel(new Label(labelStack.pop()));
		}

		ret.setComment("IF BLOCK");

		// the label stack enables us to give the then label to the first instruction
		// that will be in the if body
		labelStack.push("THEN");
		Op then = visit(ctx.stat(0));

		//then.setLabel(new Label("THEN"));

		emit(OpCode.jumpI,new Label("AFTER_IF"));


		if(ctx.stat().size() > 1){
			labelStack.push("ELSE");
			Op else_block = visit(ctx.stat(1));
		}

		labelStack.push("AFTER_IF");

		//else_block.setLabel(new Label("ELSE"));



		return ret;

	}

	public Op visitWhileStat(SimplePascalParser.WhileStatContext ctx) {

		labelStack.push("WHILE");
		Op cmp = visit(ctx.expr());

		//cmp.setLabel(new Label("WHILE"));
		emit(OpCode.cbr,reg(ctx.expr()),new Label("BODY"), new Label("EOW"));

		labelStack.push("BODY");
		Op body = visit(ctx.stat());

		Op ret = emit(OpCode.jumpI,new Label("WHILE"));
		if(!labelStack.isEmpty()){
			ret.setLabel(new Label(labelStack.pop()));
		}

		labelStack.push("EOW");

		return ret;
	}

	public Op visitBlockStat(SimplePascalParser.BlockStatContext ctx) {
		Op ret = visit(ctx.block());

		return ret;

	}

	public Op visitInStat(SimplePascalParser.InStatContext ctx) {
		visit(ctx.target());
		Op ret = emit(OpCode.in,new Str(ctx.STR().getText()),reg(ctx.target()));
		if(!labelStack.isEmpty()){
			ret.setLabel(new Label(labelStack.pop()));
		}
		return ret;
	}

	public Op visitOutStat(SimplePascalParser.OutStatContext ctx) {
		Op exp = visit(ctx.expr());
		Op ret = emit(OpCode.out,new Str(ctx.STR().getText()),reg(ctx.expr()));
		if(!labelStack.isEmpty()){
			ret.setLabel(new Label(labelStack.pop()));
		}
		return ret;
	}

	public Op visitIdTarget(SimplePascalParser.IdTargetContext ctx) {

		// associate the name with the future value
		regs.put(ctx,vars.get(ctx.ID().getText()));
		return null;
	}

	public Op visitParExpr(SimplePascalParser.ParExprContext ctx) {
		Op ret= visit(ctx.expr());
		this.regs.put(ctx,reg(ctx.expr()));
		return ret;
	}

	public Op visitTrueExpr(SimplePascalParser.TrueExprContext ctx) {
		Op ret = emit(OpCode.loadI,TRUE_VALUE,reg(ctx));
		if(!labelStack.isEmpty()){
			ret.setLabel(new Label(labelStack.pop()));
		}
		return ret;
	}

	public Op visitCompExpr(SimplePascalParser.CompExprContext ctx) {
		visit(ctx.expr(0));
		Reg r1 = reg(ctx.expr(0));
		visit(ctx.expr(1));
		Reg r2 = reg(ctx.expr(1));

		Reg r3 = reg(ctx);

		Op ret = null;
		if(ctx.compOp().EQ() != null){
			ret = emit(OpCode.cmp_EQ,r1,r2,r3);
		} else if (ctx.compOp().LE() != null) {
			ret = emit(OpCode.cmp_LE,r1,r2,r3);
		} else if (ctx.compOp().GT() != null) {
			ret = emit(OpCode.cmp_GT,r1,r2,r3);
		} else if (ctx.compOp().GE() != null) {
			ret = emit(OpCode.cmp_GE,r1,r2,r3);
		} else if (ctx.compOp().LT() != null) {
			ret = emit(OpCode.cmp_LT,r1,r2,r3);
		} else if (ctx.compOp().NE() != null) {
			ret = emit(OpCode.cmp_NE,r1,r2,r3);
		}
		if(!labelStack.isEmpty()){
			ret.setLabel(new Label(labelStack.pop()));
		}

		return ret;

	}

	public Op visitPrfExpr(SimplePascalParser.PrfExprContext ctx) {

		visit(ctx.expr());

		Op ret = null;
		if(ctx.prfOp().NOT() != null){

			ret = emit(OpCode.xorI,reg(ctx.expr()),TRUE_VALUE,reg(ctx));
		}else if (ctx.prfOp().MINUS() != null){
			ret =  emit(OpCode.rsubI,reg(ctx.expr()),new Num(0),reg(ctx));
		}

		if(!labelStack.isEmpty()){
			ret.setLabel(new Label(labelStack.pop()));
		}

		return ret;
	}

 
	public Op visitFalseExpr(SimplePascalParser.FalseExprContext ctx) {
		return emit(OpCode.loadI,FALSE_VALUE,reg(ctx));
	}

 
	public Op visitBoolExpr(SimplePascalParser.BoolExprContext ctx) {

		visit(ctx.expr(0));
		Reg r1 = reg(ctx.expr(0));
		visit(ctx.expr(1));
		Reg r2 = reg(ctx.expr(1));

		Reg r3 = reg(ctx);

		Op ret = null;
		if(ctx.boolOp().OR() != null){
			ret = emit(OpCode.or,r1,r2,r3);
		} else if (ctx.boolOp().AND() != null) {
			ret = emit(OpCode.and,r1,r2,r3);
		}

		if(!labelStack.isEmpty()){
			ret.setLabel(new Label(labelStack.pop()));
		}

		return ret;
	}

 
	public Op visitMultExpr(SimplePascalParser.MultExprContext ctx) {

		visit(ctx.expr(0));
		Reg r1 = reg(ctx.expr(0));
		visit(ctx.expr(1));
		Reg r2 = reg(ctx.expr(1));

		Reg r3 = reg(ctx);

		Op ret = null;
		if(ctx.multOp().SLASH() != null){
			ret = emit(OpCode.div,r1,r2,r3);

		}else if (ctx.multOp().STAR() != null){
			ret = emit(OpCode.mult,r1,r2,r3);

		}
		if(!labelStack.isEmpty()){
			ret.setLabel(new Label(labelStack.pop()));
		}

		return ret;
	}

 
	public Op visitNumExpr(SimplePascalParser.NumExprContext ctx) {

		Op ret = emit(OpCode.loadI,new Num(Integer.parseInt(ctx.NUM().getText())),reg(ctx));

		if(!labelStack.isEmpty()){
			ret.setLabel(new Label(labelStack.pop()));
		}
		return ret;
	}

 
	public Op visitPlusExpr(SimplePascalParser.PlusExprContext ctx) {
		visit(ctx.expr(0));
		Reg r1 = reg(ctx.expr(0));
		visit(ctx.expr(1));
		Reg r2 = reg(ctx.expr(1));

		Reg r3 = reg(ctx);

		Op ret = null;
		if(ctx.plusOp().PLUS() != null){
			ret = emit(OpCode.add,r1,r2,r3);
		}else if (ctx.plusOp().MINUS() != null){
			ret = emit(OpCode.sub,r1,r2,r3);
		}

		if(!labelStack.isEmpty()){
			ret.setLabel(new Label(labelStack.pop()));
		}

		return ret;
	}

 
	public Op visitIdExpr(SimplePascalParser.IdExprContext ctx) {

		this.regs.put(ctx,vars.get(ctx.ID().getText()));
		return null;

	}

 
	public Op visitPrfOp(SimplePascalParser.PrfOpContext ctx) {


		return null;

	}

 
	public Op visitMultOp(SimplePascalParser.MultOpContext ctx) { return visitChildren(ctx); }

	public Op visitPlusOp(SimplePascalParser.PlusOpContext ctx) { return visitChildren(ctx); }

	public Op visitBoolOp(SimplePascalParser.BoolOpContext ctx) { return visitChildren(ctx); }

	public Op visitCompOp(SimplePascalParser.CompOpContext ctx) {
		return null;
	}

	public Op visitIntType(SimplePascalParser.IntTypeContext ctx) {
		// balec du type ici c'est que des int
		return null;
	}

	public Op visitBoolType(SimplePascalParser.BoolTypeContext ctx) {
		// balec du type ici c'est que des int
		return null;
	}



	/** Constructs an operation from the parameters 
	 * and adds it to the program under construction. */
	private Op emit(Label label, OpCode opCode, Operand... args) {
		Op result = new Op(label, opCode, args);
		this.prog.addInstr(result);
		return result;
	}

	/** Constructs an operation from the parameters 
	 * and adds it to the program under construction. */
	private Op emit(OpCode opCode, Operand... args) {
		return emit((Label) null, opCode, args);
	}

	/** 
	 * Looks up the label for a given parse tree node,
	 * creating it if none has been created before.
	 * The label is actually constructed from the entry node
	 * in the flow graph, as stored in the checker result.
	 */
	private Label label(ParserRuleContext node) {
		Label result = this.labels.get(node);
		if (result == null) {
			ParserRuleContext entry = this.checkResult.getEntry(node);
			result = createLabel(entry, "n");
			this.labels.put(node, result);
		}
		return result;
	}

	/** Creates a label for a given parse tree node and prefix. */
	private Label createLabel(ParserRuleContext node, String prefix) {
		Token token = node.getStart();
		int line = token.getLine();
		int column = token.getCharPositionInLine();
		String result = prefix + "_" + line + "_" + column;
		return new Label(result);
	}

	/** Retrieves the offset of a variable node from the checker result,
	 * wrapped in a {@link Num} operand. */
	private Num offset(ParseTree node) {
		return new Num(this.checkResult.getOffset(node));
	}

	/** Returns a register for a given parse tree node,
	 * creating a fresh register if there is none for that node. */
	private Reg reg(ParseTree node) {
		Reg result = this.regs.get(node);
		if (result == null) {
			result = new Reg("r_" + this.regCount);
			this.regs.put(node, result);
			this.regCount++;
		}
		return result;
	}

	/** Assigns a register to a given parse tree node. */
	private void setReg(ParseTree node, Reg reg) {
		this.regs.put(node, reg);
	}
}
