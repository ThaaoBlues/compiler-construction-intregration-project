// Generated from /media/thaao/60E80E40E80E14C6/INSA/3MIC/S2/SEMESTRE_ETRANGER/PROGRAMMING_PARADIGMS/LAB/BLOCK_5-CC/pp-student-block5/pp/block5/cc/antlr/Building.g4 by ANTLR 4.13.2
package pp.block5.cc.antlr;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link BuildingParser}.
 */
public interface BuildingListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link BuildingParser#building}.
	 * @param ctx the parse tree
	 */
	void enterBuilding(BuildingParser.BuildingContext ctx);
	/**
	 * Exit a parse tree produced by {@link BuildingParser#building}.
	 * @param ctx the parse tree
	 */
	void exitBuilding(BuildingParser.BuildingContext ctx);
	/**
	 * Enter a parse tree produced by {@link BuildingParser#floor}.
	 * @param ctx the parse tree
	 */
	void enterFloor(BuildingParser.FloorContext ctx);
	/**
	 * Exit a parse tree produced by {@link BuildingParser#floor}.
	 * @param ctx the parse tree
	 */
	void exitFloor(BuildingParser.FloorContext ctx);
	/**
	 * Enter a parse tree produced by {@link BuildingParser#room}.
	 * @param ctx the parse tree
	 */
	void enterRoom(BuildingParser.RoomContext ctx);
	/**
	 * Exit a parse tree produced by {@link BuildingParser#room}.
	 * @param ctx the parse tree
	 */
	void exitRoom(BuildingParser.RoomContext ctx);
}