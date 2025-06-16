// Generated from /media/thaao/60E80E40E80E14C6/INSA/3MIC/S2/SEMESTRE_ETRANGER/PROGRAMMING_PARADIGMS/LAB/BLOCK_5-CC/pp-student-block5/pp/block5/cc/pascal/ProcPascal.g4 by ANTLR 4.13.2
package pp.block5.cc.pascal;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link ProcPascalParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface ProcPascalVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by the {@code funcDecl}
	 * labeled alternative in {@link ProcPascalParser#decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFuncDecl(ProcPascalParser.FuncDeclContext ctx);
	/**
	 * Visit a parse tree produced by the {@code procDecl}
	 * labeled alternative in {@link ProcPascalParser#decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcDecl(ProcPascalParser.ProcDeclContext ctx);
	/**
	 * Visit a parse tree produced by the {@code varDecl}
	 * labeled alternative in {@link ProcPascalParser#decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVarDecl(ProcPascalParser.VarDeclContext ctx);
	/**
	 * Visit a parse tree produced by {@link ProcPascalParser#pars}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPars(ProcPascalParser.ParsContext ctx);
	/**
	 * Visit a parse tree produced by the {@code assStat}
	 * labeled alternative in {@link ProcPascalParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssStat(ProcPascalParser.AssStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ifStat}
	 * labeled alternative in {@link ProcPascalParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfStat(ProcPascalParser.IfStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code whileStat}
	 * labeled alternative in {@link ProcPascalParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWhileStat(ProcPascalParser.WhileStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code blockStat}
	 * labeled alternative in {@link ProcPascalParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlockStat(ProcPascalParser.BlockStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code inStat}
	 * labeled alternative in {@link ProcPascalParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInStat(ProcPascalParser.InStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code outStat}
	 * labeled alternative in {@link ProcPascalParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOutStat(ProcPascalParser.OutStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code callStat}
	 * labeled alternative in {@link ProcPascalParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCallStat(ProcPascalParser.CallStatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code exitStat}
	 * labeled alternative in {@link ProcPascalParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExitStat(ProcPascalParser.ExitStatContext ctx);
	/**
	 * Visit a parse tree produced by {@link ProcPascalParser#args}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArgs(ProcPascalParser.ArgsContext ctx);
	/**
	 * Visit a parse tree produced by the {@code parExpr}
	 * labeled alternative in {@link ProcPascalParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParExpr(ProcPascalParser.ParExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code trueExpr}
	 * labeled alternative in {@link ProcPascalParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTrueExpr(ProcPascalParser.TrueExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code compExpr}
	 * labeled alternative in {@link ProcPascalParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCompExpr(ProcPascalParser.CompExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code prfExpr}
	 * labeled alternative in {@link ProcPascalParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrfExpr(ProcPascalParser.PrfExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code callExpr}
	 * labeled alternative in {@link ProcPascalParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCallExpr(ProcPascalParser.CallExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code falseExpr}
	 * labeled alternative in {@link ProcPascalParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFalseExpr(ProcPascalParser.FalseExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code boolExpr}
	 * labeled alternative in {@link ProcPascalParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBoolExpr(ProcPascalParser.BoolExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code numExpr}
	 * labeled alternative in {@link ProcPascalParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNumExpr(ProcPascalParser.NumExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code plusExpr}
	 * labeled alternative in {@link ProcPascalParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPlusExpr(ProcPascalParser.PlusExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code idExpr}
	 * labeled alternative in {@link ProcPascalParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdExpr(ProcPascalParser.IdExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link ProcPascalParser#program}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgram(ProcPascalParser.ProgramContext ctx);
	/**
	 * Visit a parse tree produced by {@link ProcPascalParser#body}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBody(ProcPascalParser.BodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link ProcPascalParser#var}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVar(ProcPascalParser.VarContext ctx);
	/**
	 * Visit a parse tree produced by {@link ProcPascalParser#block}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlock(ProcPascalParser.BlockContext ctx);
	/**
	 * Visit a parse tree produced by the {@code idTarget}
	 * labeled alternative in {@link ProcPascalParser#target}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdTarget(ProcPascalParser.IdTargetContext ctx);
	/**
	 * Visit a parse tree produced by {@link ProcPascalParser#prfOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrfOp(ProcPascalParser.PrfOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link ProcPascalParser#multOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMultOp(ProcPascalParser.MultOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link ProcPascalParser#plusOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPlusOp(ProcPascalParser.PlusOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link ProcPascalParser#boolOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBoolOp(ProcPascalParser.BoolOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link ProcPascalParser#compOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCompOp(ProcPascalParser.CompOpContext ctx);
	/**
	 * Visit a parse tree produced by the {@code intType}
	 * labeled alternative in {@link ProcPascalParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIntType(ProcPascalParser.IntTypeContext ctx);
	/**
	 * Visit a parse tree produced by the {@code boolType}
	 * labeled alternative in {@link ProcPascalParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBoolType(ProcPascalParser.BoolTypeContext ctx);
}