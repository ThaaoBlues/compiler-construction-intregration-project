// Generated from /media/thaao/60E80E40E80E14C6/INSA/3MIC/S2/SEMESTRE_ETRANGER/PROGRAMMING_PARADIGMS/LAB/BLOCK_5-CC/pp-student-block5/pp/block5/cc/antlr/NumWordGroup.g4 by ANTLR 4.13.2
package pp.block5.cc.antlr;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue", "this-escape"})
public class NumWordGroupParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		WORD=1, NUMBER=2, WS=3;
	public static final int
		RULE_sentence = 0, RULE_group = 1, RULE_penultimateGroup = 2, RULE_lastGroup = 3, 
		RULE_number = 4, RULE_word = 5;
	private static String[] makeRuleNames() {
		return new String[] {
			"sentence", "group", "penultimateGroup", "lastGroup", "number", "word"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "WORD", "NUMBER", "WS"
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

	@Override
	public String getGrammarFileName() { return "NumWordGroup.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public NumWordGroupParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class SentenceContext extends ParserRuleContext {
		public LastGroupContext lastGroup() {
			return getRuleContext(LastGroupContext.class,0);
		}
		public TerminalNode EOF() { return getToken(NumWordGroupParser.EOF, 0); }
		public PenultimateGroupContext penultimateGroup() {
			return getRuleContext(PenultimateGroupContext.class,0);
		}
		public List<GroupContext> group() {
			return getRuleContexts(GroupContext.class);
		}
		public GroupContext group(int i) {
			return getRuleContext(GroupContext.class,i);
		}
		public SentenceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_sentence; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NumWordGroupListener ) ((NumWordGroupListener)listener).enterSentence(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NumWordGroupListener ) ((NumWordGroupListener)listener).exitSentence(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NumWordGroupVisitor ) return ((NumWordGroupVisitor<? extends T>)visitor).visitSentence(this);
			else return visitor.visitChildren(this);
		}
	}

	public final SentenceContext sentence() throws RecognitionException {
		SentenceContext _localctx = new SentenceContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_sentence);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(19);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,1,_ctx) ) {
			case 1:
				{
				setState(15);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,0,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(12);
						group();
						}
						} 
					}
					setState(17);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,0,_ctx);
				}
				setState(18);
				penultimateGroup();
				}
				break;
			}
			setState(21);
			lastGroup();
			setState(22);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class GroupContext extends ParserRuleContext {
		public NumberContext number() {
			return getRuleContext(NumberContext.class,0);
		}
		public WordContext word() {
			return getRuleContext(WordContext.class,0);
		}
		public GroupContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_group; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NumWordGroupListener ) ((NumWordGroupListener)listener).enterGroup(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NumWordGroupListener ) ((NumWordGroupListener)listener).exitGroup(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NumWordGroupVisitor ) return ((NumWordGroupVisitor<? extends T>)visitor).visitGroup(this);
			else return visitor.visitChildren(this);
		}
	}

	public final GroupContext group() throws RecognitionException {
		GroupContext _localctx = new GroupContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_group);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(24);
			number();
			setState(25);
			word();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PenultimateGroupContext extends ParserRuleContext {
		public NumberContext number() {
			return getRuleContext(NumberContext.class,0);
		}
		public WordContext word() {
			return getRuleContext(WordContext.class,0);
		}
		public PenultimateGroupContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_penultimateGroup; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NumWordGroupListener ) ((NumWordGroupListener)listener).enterPenultimateGroup(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NumWordGroupListener ) ((NumWordGroupListener)listener).exitPenultimateGroup(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NumWordGroupVisitor ) return ((NumWordGroupVisitor<? extends T>)visitor).visitPenultimateGroup(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PenultimateGroupContext penultimateGroup() throws RecognitionException {
		PenultimateGroupContext _localctx = new PenultimateGroupContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_penultimateGroup);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(27);
			number();
			setState(28);
			word();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LastGroupContext extends ParserRuleContext {
		public NumberContext number() {
			return getRuleContext(NumberContext.class,0);
		}
		public WordContext word() {
			return getRuleContext(WordContext.class,0);
		}
		public LastGroupContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_lastGroup; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NumWordGroupListener ) ((NumWordGroupListener)listener).enterLastGroup(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NumWordGroupListener ) ((NumWordGroupListener)listener).exitLastGroup(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NumWordGroupVisitor ) return ((NumWordGroupVisitor<? extends T>)visitor).visitLastGroup(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LastGroupContext lastGroup() throws RecognitionException {
		LastGroupContext _localctx = new LastGroupContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_lastGroup);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(30);
			number();
			setState(31);
			word();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class NumberContext extends ParserRuleContext {
		public TerminalNode NUMBER() { return getToken(NumWordGroupParser.NUMBER, 0); }
		public NumberContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_number; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NumWordGroupListener ) ((NumWordGroupListener)listener).enterNumber(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NumWordGroupListener ) ((NumWordGroupListener)listener).exitNumber(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NumWordGroupVisitor ) return ((NumWordGroupVisitor<? extends T>)visitor).visitNumber(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NumberContext number() throws RecognitionException {
		NumberContext _localctx = new NumberContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_number);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(33);
			match(NUMBER);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class WordContext extends ParserRuleContext {
		public TerminalNode WORD() { return getToken(NumWordGroupParser.WORD, 0); }
		public WordContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_word; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof NumWordGroupListener ) ((NumWordGroupListener)listener).enterWord(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof NumWordGroupListener ) ((NumWordGroupListener)listener).exitWord(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof NumWordGroupVisitor ) return ((NumWordGroupVisitor<? extends T>)visitor).visitWord(this);
			else return visitor.visitChildren(this);
		}
	}

	public final WordContext word() throws RecognitionException {
		WordContext _localctx = new WordContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_word);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(35);
			match(WORD);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\u0004\u0001\u0003&\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0001\u0000\u0005\u0000\u000e\b\u0000\n\u0000\f\u0000"+
		"\u0011\t\u0000\u0001\u0000\u0003\u0000\u0014\b\u0000\u0001\u0000\u0001"+
		"\u0000\u0001\u0000\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0004\u0001"+
		"\u0004\u0001\u0005\u0001\u0005\u0001\u0005\u0000\u0000\u0006\u0000\u0002"+
		"\u0004\u0006\b\n\u0000\u0000!\u0000\u0013\u0001\u0000\u0000\u0000\u0002"+
		"\u0018\u0001\u0000\u0000\u0000\u0004\u001b\u0001\u0000\u0000\u0000\u0006"+
		"\u001e\u0001\u0000\u0000\u0000\b!\u0001\u0000\u0000\u0000\n#\u0001\u0000"+
		"\u0000\u0000\f\u000e\u0003\u0002\u0001\u0000\r\f\u0001\u0000\u0000\u0000"+
		"\u000e\u0011\u0001\u0000\u0000\u0000\u000f\r\u0001\u0000\u0000\u0000\u000f"+
		"\u0010\u0001\u0000\u0000\u0000\u0010\u0012\u0001\u0000\u0000\u0000\u0011"+
		"\u000f\u0001\u0000\u0000\u0000\u0012\u0014\u0003\u0004\u0002\u0000\u0013"+
		"\u000f\u0001\u0000\u0000\u0000\u0013\u0014\u0001\u0000\u0000\u0000\u0014"+
		"\u0015\u0001\u0000\u0000\u0000\u0015\u0016\u0003\u0006\u0003\u0000\u0016"+
		"\u0017\u0005\u0000\u0000\u0001\u0017\u0001\u0001\u0000\u0000\u0000\u0018"+
		"\u0019\u0003\b\u0004\u0000\u0019\u001a\u0003\n\u0005\u0000\u001a\u0003"+
		"\u0001\u0000\u0000\u0000\u001b\u001c\u0003\b\u0004\u0000\u001c\u001d\u0003"+
		"\n\u0005\u0000\u001d\u0005\u0001\u0000\u0000\u0000\u001e\u001f\u0003\b"+
		"\u0004\u0000\u001f \u0003\n\u0005\u0000 \u0007\u0001\u0000\u0000\u0000"+
		"!\"\u0005\u0002\u0000\u0000\"\t\u0001\u0000\u0000\u0000#$\u0005\u0001"+
		"\u0000\u0000$\u000b\u0001\u0000\u0000\u0000\u0002\u000f\u0013";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}