package pp.block5.cc.antlr;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import pp.block5.cc.ErrorListener;
import pp.block5.cc.ParseException;
import pp.block5.cc.antlr.NumWordBaseVisitor;
import pp.block5.cc.antlr.NumWordLexer;
import pp.block5.cc.antlr.NumWordParser;

/** Prettyprints a (number, word)-sentence and sums up the numbers. */
public class NumWordProcessor extends NumWordBaseVisitor<Integer> {

	public static void main(String[] args) {
		NumWordProcessor grouper = new NumWordProcessor();
		if (args.length == 0) {
			process(grouper, "1sock2shoes 3 holes");
			process(grouper, "3 strands 10 blocks 11 weeks 15 credits");
			process(grouper, "1 2 3");
		} else {
			for (String text : args) {
				process(grouper, text);
			}
		}
	}

	private static void process(NumWordProcessor grouper, String text) {
		try {
			System.out.printf("Processing '%s':%n", text);
			int result = grouper.group(text);
			System.out.println("Total: " + result);
		} catch (ParseException exc) {
			exc.print();
		}
	}

	/** Groups a given sentence and prints it to stdout.
	 * Returns the sum of the numbers in the sentence.
	 */
	public int group(String text) throws ParseException {
		CharStream chars = CharStreams.fromString(text);
		ErrorListener listener = new ErrorListener();
		Lexer lexer = new NumWordLexer(chars);
		lexer.removeErrorListeners();
		lexer.addErrorListener(listener);
		TokenStream tokens = new CommonTokenStream(lexer);
		NumWordParser parser = new NumWordParser(tokens);
		parser.removeErrorListeners();
		parser.addErrorListener(listener);
		ParseTree tree = parser.sentence();
		listener.throwException();
		return visit(tree);
	}

	// Override the visitor methods.
	// Each visitor method should call visit(child)
	// if and when it wants to visit that child node.

	@Override
	public Integer visitSentence(NumWordParser.SentenceContext ctx){
		StringBuilder stb = new StringBuilder();

		var words = ctx.word();
		int res = 0;
		var numbers = ctx.number();
		int current_number = 0;
		for(int i = 0;i<words.size();i++){

			current_number = visit(numbers.get(i));
			stb.append(current_number);
			stb.append(" ");

			res += current_number;

			stb.append(words.get(i).WORD().getText());

			if(i ==  words.size()-2 ){
				stb.append(" ");
				stb.append("and");
				stb.append(" ");
			}else if(i == words.size() -1){
				continue;
			}else{
				stb.append(",");
				stb.append(" ");
			}
		}

		System.out.println("Processed sentence : "+stb.toString());
		return res;
	}
	@Override
	public Integer visitNumber(NumWordParser.NumberContext ctx){

        return Integer.parseInt(ctx.NUMBER().getText());

	}



}
