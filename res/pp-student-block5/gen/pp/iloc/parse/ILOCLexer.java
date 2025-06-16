// Generated from /media/thaao/60E80E40E80E14C6/INSA/3MIC/S2/SEMESTRE_ETRANGER/PROGRAMMING_PARADIGMS/LAB/BLOCK_5-CC/pp-student-block5/pp/iloc/parse/ILOC.g4 by ANTLR 4.13.2
package pp.iloc.parse;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue", "this-escape"})
public class ILOCLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.13.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, MINUS=2, COMMA=3, SEMI=4, LSQ=5, RSQ=6, DARROW=7, ARROW=8, ASS=9, 
		ID=10, SYMB=11, LAB=12, NUM=13, STR=14, COMMENT=15, WS=16, EOL=17;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "LETTER", "DIGIT", "MINUS", "COMMA", "SEMI", "LSQ", "RSQ", "DARROW", 
			"ARROW", "ASS", "ID", "SYMB", "LAB", "NUM", "STR", "COMMENT", "WS", "EOL"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "':'", "'-'", "','", "';'", "'['", "']'", "'=>'", "'->'", "'<-'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, "MINUS", "COMMA", "SEMI", "LSQ", "RSQ", "DARROW", "ARROW", 
			"ASS", "ID", "SYMB", "LAB", "NUM", "STR", "COMMENT", "WS", "EOL"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	public ILOCLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "ILOC.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\u0004\u0000\u0011w\u0006\uffff\uffff\u0002\u0000\u0007\u0000\u0002\u0001"+
		"\u0007\u0001\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004"+
		"\u0007\u0004\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007"+
		"\u0007\u0007\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b"+
		"\u0007\u000b\u0002\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002"+
		"\u000f\u0007\u000f\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011\u0002"+
		"\u0012\u0007\u0012\u0001\u0000\u0001\u0000\u0001\u0001\u0001\u0001\u0001"+
		"\u0002\u0001\u0002\u0001\u0003\u0001\u0003\u0001\u0004\u0001\u0004\u0001"+
		"\u0005\u0001\u0005\u0001\u0006\u0001\u0006\u0001\u0007\u0001\u0007\u0001"+
		"\b\u0001\b\u0001\b\u0001\t\u0001\t\u0001\t\u0001\n\u0001\n\u0001\n\u0001"+
		"\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0005\u000bE\b\u000b\n\u000b"+
		"\f\u000bH\t\u000b\u0001\f\u0001\f\u0001\f\u0001\r\u0001\r\u0001\r\u0001"+
		"\u000e\u0003\u000eQ\b\u000e\u0001\u000e\u0004\u000eT\b\u000e\u000b\u000e"+
		"\f\u000eU\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0005\u000f"+
		"\\\b\u000f\n\u000f\f\u000f_\t\u000f\u0001\u000f\u0001\u000f\u0001\u0010"+
		"\u0001\u0010\u0001\u0010\u0001\u0010\u0005\u0010g\b\u0010\n\u0010\f\u0010"+
		"j\t\u0010\u0001\u0011\u0004\u0011m\b\u0011\u000b\u0011\f\u0011n\u0001"+
		"\u0011\u0001\u0011\u0001\u0012\u0004\u0012t\b\u0012\u000b\u0012\f\u0012"+
		"u\u0000\u0000\u0013\u0001\u0001\u0003\u0000\u0005\u0000\u0007\u0002\t"+
		"\u0003\u000b\u0004\r\u0005\u000f\u0006\u0011\u0007\u0013\b\u0015\t\u0017"+
		"\n\u0019\u000b\u001b\f\u001d\r\u001f\u000e!\u000f#\u0010%\u0011\u0001"+
		"\u0000\u0006\u0002\u0000AZaz\u0001\u000009\u0002\u0000--__\u0003\u0000"+
		"\n\n\r\r\"\"\u0002\u0000\n\n\r\r\u0002\u0000\t\t  ~\u0000\u0001\u0001"+
		"\u0000\u0000\u0000\u0000\u0007\u0001\u0000\u0000\u0000\u0000\t\u0001\u0000"+
		"\u0000\u0000\u0000\u000b\u0001\u0000\u0000\u0000\u0000\r\u0001\u0000\u0000"+
		"\u0000\u0000\u000f\u0001\u0000\u0000\u0000\u0000\u0011\u0001\u0000\u0000"+
		"\u0000\u0000\u0013\u0001\u0000\u0000\u0000\u0000\u0015\u0001\u0000\u0000"+
		"\u0000\u0000\u0017\u0001\u0000\u0000\u0000\u0000\u0019\u0001\u0000\u0000"+
		"\u0000\u0000\u001b\u0001\u0000\u0000\u0000\u0000\u001d\u0001\u0000\u0000"+
		"\u0000\u0000\u001f\u0001\u0000\u0000\u0000\u0000!\u0001\u0000\u0000\u0000"+
		"\u0000#\u0001\u0000\u0000\u0000\u0000%\u0001\u0000\u0000\u0000\u0001\'"+
		"\u0001\u0000\u0000\u0000\u0003)\u0001\u0000\u0000\u0000\u0005+\u0001\u0000"+
		"\u0000\u0000\u0007-\u0001\u0000\u0000\u0000\t/\u0001\u0000\u0000\u0000"+
		"\u000b1\u0001\u0000\u0000\u0000\r3\u0001\u0000\u0000\u0000\u000f5\u0001"+
		"\u0000\u0000\u0000\u00117\u0001\u0000\u0000\u0000\u0013:\u0001\u0000\u0000"+
		"\u0000\u0015=\u0001\u0000\u0000\u0000\u0017@\u0001\u0000\u0000\u0000\u0019"+
		"I\u0001\u0000\u0000\u0000\u001bL\u0001\u0000\u0000\u0000\u001dP\u0001"+
		"\u0000\u0000\u0000\u001fW\u0001\u0000\u0000\u0000!b\u0001\u0000\u0000"+
		"\u0000#l\u0001\u0000\u0000\u0000%s\u0001\u0000\u0000\u0000\'(\u0005:\u0000"+
		"\u0000(\u0002\u0001\u0000\u0000\u0000)*\u0007\u0000\u0000\u0000*\u0004"+
		"\u0001\u0000\u0000\u0000+,\u0007\u0001\u0000\u0000,\u0006\u0001\u0000"+
		"\u0000\u0000-.\u0005-\u0000\u0000.\b\u0001\u0000\u0000\u0000/0\u0005,"+
		"\u0000\u00000\n\u0001\u0000\u0000\u000012\u0005;\u0000\u00002\f\u0001"+
		"\u0000\u0000\u000034\u0005[\u0000\u00004\u000e\u0001\u0000\u0000\u0000"+
		"56\u0005]\u0000\u00006\u0010\u0001\u0000\u0000\u000078\u0005=\u0000\u0000"+
		"89\u0005>\u0000\u00009\u0012\u0001\u0000\u0000\u0000:;\u0005-\u0000\u0000"+
		";<\u0005>\u0000\u0000<\u0014\u0001\u0000\u0000\u0000=>\u0005<\u0000\u0000"+
		">?\u0005-\u0000\u0000?\u0016\u0001\u0000\u0000\u0000@F\u0003\u0003\u0001"+
		"\u0000AE\u0003\u0003\u0001\u0000BE\u0003\u0005\u0002\u0000CE\u0007\u0002"+
		"\u0000\u0000DA\u0001\u0000\u0000\u0000DB\u0001\u0000\u0000\u0000DC\u0001"+
		"\u0000\u0000\u0000EH\u0001\u0000\u0000\u0000FD\u0001\u0000\u0000\u0000"+
		"FG\u0001\u0000\u0000\u0000G\u0018\u0001\u0000\u0000\u0000HF\u0001\u0000"+
		"\u0000\u0000IJ\u0005@\u0000\u0000JK\u0003\u0017\u000b\u0000K\u001a\u0001"+
		"\u0000\u0000\u0000LM\u0005#\u0000\u0000MN\u0003\u0017\u000b\u0000N\u001c"+
		"\u0001\u0000\u0000\u0000OQ\u0003\u0007\u0003\u0000PO\u0001\u0000\u0000"+
		"\u0000PQ\u0001\u0000\u0000\u0000QS\u0001\u0000\u0000\u0000RT\u0003\u0005"+
		"\u0002\u0000SR\u0001\u0000\u0000\u0000TU\u0001\u0000\u0000\u0000US\u0001"+
		"\u0000\u0000\u0000UV\u0001\u0000\u0000\u0000V\u001e\u0001\u0000\u0000"+
		"\u0000W]\u0005\"\u0000\u0000X\\\b\u0003\u0000\u0000YZ\u0005\\\u0000\u0000"+
		"Z\\\u0005\"\u0000\u0000[X\u0001\u0000\u0000\u0000[Y\u0001\u0000\u0000"+
		"\u0000\\_\u0001\u0000\u0000\u0000][\u0001\u0000\u0000\u0000]^\u0001\u0000"+
		"\u0000\u0000^`\u0001\u0000\u0000\u0000_]\u0001\u0000\u0000\u0000`a\u0005"+
		"\"\u0000\u0000a \u0001\u0000\u0000\u0000bc\u0005/\u0000\u0000cd\u0005"+
		"/\u0000\u0000dh\u0001\u0000\u0000\u0000eg\b\u0004\u0000\u0000fe\u0001"+
		"\u0000\u0000\u0000gj\u0001\u0000\u0000\u0000hf\u0001\u0000\u0000\u0000"+
		"hi\u0001\u0000\u0000\u0000i\"\u0001\u0000\u0000\u0000jh\u0001\u0000\u0000"+
		"\u0000km\u0007\u0005\u0000\u0000lk\u0001\u0000\u0000\u0000mn\u0001\u0000"+
		"\u0000\u0000nl\u0001\u0000\u0000\u0000no\u0001\u0000\u0000\u0000op\u0001"+
		"\u0000\u0000\u0000pq\u0006\u0011\u0000\u0000q$\u0001\u0000\u0000\u0000"+
		"rt\u0007\u0004\u0000\u0000sr\u0001\u0000\u0000\u0000tu\u0001\u0000\u0000"+
		"\u0000us\u0001\u0000\u0000\u0000uv\u0001\u0000\u0000\u0000v&\u0001\u0000"+
		"\u0000\u0000\n\u0000DFPU[]hnu\u0001\u0006\u0000\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}