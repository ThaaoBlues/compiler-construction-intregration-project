// Generated from /media/thaao/60E80E40E80E14C6/INSA/3MIC/S2/SEMESTRE_ETRANGER/PROGRAMMING_PARADIGMS/LAB/BLOCK_5-CC/pp-student-block5/pp/iloc/parse/ILOC.g4 by ANTLR 4.13.2
package pp.iloc.parse;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue", "this-escape"})
public class ILOCParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, MINUS=2, COMMA=3, SEMI=4, LSQ=5, RSQ=6, DARROW=7, ARROW=8, ASS=9, 
		ID=10, SYMB=11, LAB=12, NUM=13, STR=14, COMMENT=15, WS=16, EOL=17;
	public static final int
		RULE_program = 0, RULE_decl = 1, RULE_instr = 2, RULE_op = 3, RULE_sources = 4, 
		RULE_targets = 5, RULE_label = 6, RULE_opCode = 7, RULE_operand = 8;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "decl", "instr", "op", "sources", "targets", "label", "opCode", 
			"operand"
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

	@Override
	public String getGrammarFileName() { return "ILOC.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public ILOCParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProgramContext extends ParserRuleContext {
		public List<InstrContext> instr() {
			return getRuleContexts(InstrContext.class);
		}
		public InstrContext instr(int i) {
			return getRuleContext(InstrContext.class,i);
		}
		public TerminalNode EOF() { return getToken(ILOCParser.EOF, 0); }
		public List<DeclContext> decl() {
			return getRuleContexts(DeclContext.class);
		}
		public DeclContext decl(int i) {
			return getRuleContext(DeclContext.class,i);
		}
		public List<TerminalNode> EOL() { return getTokens(ILOCParser.EOL); }
		public TerminalNode EOL(int i) {
			return getToken(ILOCParser.EOL, i);
		}
		public ProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_program; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).enterProgram(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).exitProgram(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ILOCVisitor ) return ((ILOCVisitor<? extends T>)visitor).visitProgram(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProgramContext program() throws RecognitionException {
		ProgramContext _localctx = new ProgramContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_program);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(21);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,0,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(18);
					decl();
					}
					} 
				}
				setState(23);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,0,_ctx);
			}
			setState(24);
			instr();
			setState(33);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(26); 
					_errHandler.sync(this);
					_la = _input.LA(1);
					do {
						{
						{
						setState(25);
						match(EOL);
						}
						}
						setState(28); 
						_errHandler.sync(this);
						_la = _input.LA(1);
					} while ( _la==EOL );
					setState(30);
					instr();
					}
					} 
				}
				setState(35);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			}
			setState(39);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==EOL) {
				{
				{
				setState(36);
				match(EOL);
				}
				}
				setState(41);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(42);
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
	public static class DeclContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(ILOCParser.ID, 0); }
		public TerminalNode ASS() { return getToken(ILOCParser.ASS, 0); }
		public TerminalNode NUM() { return getToken(ILOCParser.NUM, 0); }
		public TerminalNode COMMENT() { return getToken(ILOCParser.COMMENT, 0); }
		public List<TerminalNode> EOL() { return getTokens(ILOCParser.EOL); }
		public TerminalNode EOL(int i) {
			return getToken(ILOCParser.EOL, i);
		}
		public DeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).enterDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).exitDecl(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ILOCVisitor ) return ((ILOCVisitor<? extends T>)visitor).visitDecl(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DeclContext decl() throws RecognitionException {
		DeclContext _localctx = new DeclContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_decl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(44);
			match(ID);
			setState(45);
			match(ASS);
			setState(46);
			match(NUM);
			setState(48);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMENT) {
				{
				setState(47);
				match(COMMENT);
				}
			}

			setState(51); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(50);
				match(EOL);
				}
				}
				setState(53); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==EOL );
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
	public static class InstrContext extends ParserRuleContext {
		public InstrContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_instr; }
	 
		public InstrContext() { }
		public void copyFrom(InstrContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class InstrListContext extends InstrContext {
		public TerminalNode LSQ() { return getToken(ILOCParser.LSQ, 0); }
		public List<OpContext> op() {
			return getRuleContexts(OpContext.class);
		}
		public OpContext op(int i) {
			return getRuleContext(OpContext.class,i);
		}
		public TerminalNode RSQ() { return getToken(ILOCParser.RSQ, 0); }
		public LabelContext label() {
			return getRuleContext(LabelContext.class,0);
		}
		public List<TerminalNode> EOL() { return getTokens(ILOCParser.EOL); }
		public TerminalNode EOL(int i) {
			return getToken(ILOCParser.EOL, i);
		}
		public InstrListContext(InstrContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).enterInstrList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).exitInstrList(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ILOCVisitor ) return ((ILOCVisitor<? extends T>)visitor).visitInstrList(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class SingleInstrContext extends InstrContext {
		public OpContext op() {
			return getRuleContext(OpContext.class,0);
		}
		public LabelContext label() {
			return getRuleContext(LabelContext.class,0);
		}
		public SingleInstrContext(InstrContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).enterSingleInstr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).exitSingleInstr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ILOCVisitor ) return ((ILOCVisitor<? extends T>)visitor).visitSingleInstr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final InstrContext instr() throws RecognitionException {
		InstrContext _localctx = new InstrContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_instr);
		int _la;
		try {
			int _alt;
			setState(93);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,12,_ctx) ) {
			case 1:
				_localctx = new SingleInstrContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(58);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
				case 1:
					{
					setState(55);
					label();
					setState(56);
					match(T__0);
					}
					break;
				}
				setState(60);
				op();
				}
				break;
			case 2:
				_localctx = new InstrListContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(64);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==ID) {
					{
					setState(61);
					label();
					setState(62);
					match(T__0);
					}
				}

				setState(66);
				match(LSQ);
				setState(70);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==EOL) {
					{
					{
					setState(67);
					match(EOL);
					}
					}
					setState(72);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(73);
				op();
				setState(82);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,10,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(75); 
						_errHandler.sync(this);
						_la = _input.LA(1);
						do {
							{
							{
							setState(74);
							match(EOL);
							}
							}
							setState(77); 
							_errHandler.sync(this);
							_la = _input.LA(1);
						} while ( _la==EOL );
						setState(79);
						op();
						}
						} 
					}
					setState(84);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,10,_ctx);
				}
				setState(88);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==EOL) {
					{
					{
					setState(85);
					match(EOL);
					}
					}
					setState(90);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(91);
				match(RSQ);
				}
				break;
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
	public static class OpContext extends ParserRuleContext {
		public OpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_op; }
	 
		public OpContext() { }
		public void copyFrom(OpContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class CommentContext extends OpContext {
		public TerminalNode COMMENT() { return getToken(ILOCParser.COMMENT, 0); }
		public CommentContext(OpContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).enterComment(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).exitComment(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ILOCVisitor ) return ((ILOCVisitor<? extends T>)visitor).visitComment(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class RealOpContext extends OpContext {
		public OpCodeContext opCode() {
			return getRuleContext(OpCodeContext.class,0);
		}
		public SourcesContext sources() {
			return getRuleContext(SourcesContext.class,0);
		}
		public TargetsContext targets() {
			return getRuleContext(TargetsContext.class,0);
		}
		public TerminalNode SEMI() { return getToken(ILOCParser.SEMI, 0); }
		public TerminalNode COMMENT() { return getToken(ILOCParser.COMMENT, 0); }
		public TerminalNode ARROW() { return getToken(ILOCParser.ARROW, 0); }
		public TerminalNode DARROW() { return getToken(ILOCParser.DARROW, 0); }
		public RealOpContext(OpContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).enterRealOp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).exitRealOp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ILOCVisitor ) return ((ILOCVisitor<? extends T>)visitor).visitRealOp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OpContext op() throws RecognitionException {
		OpContext _localctx = new OpContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_op);
		int _la;
		try {
			setState(108);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case COMMENT:
				_localctx = new CommentContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(95);
				match(COMMENT);
				}
				break;
			case ID:
				_localctx = new RealOpContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(96);
				opCode();
				setState(97);
				sources();
				setState(100);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==DARROW || _la==ARROW) {
					{
					setState(98);
					_la = _input.LA(1);
					if ( !(_la==DARROW || _la==ARROW) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(99);
					targets();
					}
				}

				setState(103);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==SEMI) {
					{
					setState(102);
					match(SEMI);
					}
				}

				setState(106);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==COMMENT) {
					{
					setState(105);
					match(COMMENT);
					}
				}

				}
				break;
			default:
				throw new NoViableAltException(this);
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
	public static class SourcesContext extends ParserRuleContext {
		public List<OperandContext> operand() {
			return getRuleContexts(OperandContext.class);
		}
		public OperandContext operand(int i) {
			return getRuleContext(OperandContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(ILOCParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(ILOCParser.COMMA, i);
		}
		public SourcesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_sources; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).enterSources(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).exitSources(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ILOCVisitor ) return ((ILOCVisitor<? extends T>)visitor).visitSources(this);
			else return visitor.visitChildren(this);
		}
	}

	public final SourcesContext sources() throws RecognitionException {
		SourcesContext _localctx = new SourcesContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_sources);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(118);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 31744L) != 0)) {
				{
				setState(110);
				operand();
				setState(115);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(111);
					match(COMMA);
					setState(112);
					operand();
					}
					}
					setState(117);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
			}

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
	public static class TargetsContext extends ParserRuleContext {
		public List<OperandContext> operand() {
			return getRuleContexts(OperandContext.class);
		}
		public OperandContext operand(int i) {
			return getRuleContext(OperandContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(ILOCParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(ILOCParser.COMMA, i);
		}
		public TargetsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_targets; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).enterTargets(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).exitTargets(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ILOCVisitor ) return ((ILOCVisitor<? extends T>)visitor).visitTargets(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TargetsContext targets() throws RecognitionException {
		TargetsContext _localctx = new TargetsContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_targets);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(120);
			operand();
			setState(125);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(121);
				match(COMMA);
				setState(122);
				operand();
				}
				}
				setState(127);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
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
	public static class LabelContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(ILOCParser.ID, 0); }
		public LabelContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_label; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).enterLabel(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).exitLabel(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ILOCVisitor ) return ((ILOCVisitor<? extends T>)visitor).visitLabel(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LabelContext label() throws RecognitionException {
		LabelContext _localctx = new LabelContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_label);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(128);
			match(ID);
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
	public static class OpCodeContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(ILOCParser.ID, 0); }
		public OpCodeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_opCode; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).enterOpCode(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).exitOpCode(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ILOCVisitor ) return ((ILOCVisitor<? extends T>)visitor).visitOpCode(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OpCodeContext opCode() throws RecognitionException {
		OpCodeContext _localctx = new OpCodeContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_opCode);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(130);
			match(ID);
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
	public static class OperandContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(ILOCParser.ID, 0); }
		public TerminalNode NUM() { return getToken(ILOCParser.NUM, 0); }
		public TerminalNode SYMB() { return getToken(ILOCParser.SYMB, 0); }
		public TerminalNode LAB() { return getToken(ILOCParser.LAB, 0); }
		public TerminalNode STR() { return getToken(ILOCParser.STR, 0); }
		public OperandContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_operand; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).enterOperand(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ILOCListener ) ((ILOCListener)listener).exitOperand(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ILOCVisitor ) return ((ILOCVisitor<? extends T>)visitor).visitOperand(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OperandContext operand() throws RecognitionException {
		OperandContext _localctx = new OperandContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_operand);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(132);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 31744L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
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
		"\u0004\u0001\u0011\u0087\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001"+
		"\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004"+
		"\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007"+
		"\u0002\b\u0007\b\u0001\u0000\u0005\u0000\u0014\b\u0000\n\u0000\f\u0000"+
		"\u0017\t\u0000\u0001\u0000\u0001\u0000\u0004\u0000\u001b\b\u0000\u000b"+
		"\u0000\f\u0000\u001c\u0001\u0000\u0005\u0000 \b\u0000\n\u0000\f\u0000"+
		"#\t\u0000\u0001\u0000\u0005\u0000&\b\u0000\n\u0000\f\u0000)\t\u0000\u0001"+
		"\u0000\u0001\u0000\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0003"+
		"\u00011\b\u0001\u0001\u0001\u0004\u00014\b\u0001\u000b\u0001\f\u00015"+
		"\u0001\u0002\u0001\u0002\u0001\u0002\u0003\u0002;\b\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0002\u0001\u0002\u0003\u0002A\b\u0002\u0001\u0002"+
		"\u0001\u0002\u0005\u0002E\b\u0002\n\u0002\f\u0002H\t\u0002\u0001\u0002"+
		"\u0001\u0002\u0004\u0002L\b\u0002\u000b\u0002\f\u0002M\u0001\u0002\u0005"+
		"\u0002Q\b\u0002\n\u0002\f\u0002T\t\u0002\u0001\u0002\u0005\u0002W\b\u0002"+
		"\n\u0002\f\u0002Z\t\u0002\u0001\u0002\u0001\u0002\u0003\u0002^\b\u0002"+
		"\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0003\u0003"+
		"e\b\u0003\u0001\u0003\u0003\u0003h\b\u0003\u0001\u0003\u0003\u0003k\b"+
		"\u0003\u0003\u0003m\b\u0003\u0001\u0004\u0001\u0004\u0001\u0004\u0005"+
		"\u0004r\b\u0004\n\u0004\f\u0004u\t\u0004\u0003\u0004w\b\u0004\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0005\u0005|\b\u0005\n\u0005\f\u0005\u007f\t"+
		"\u0005\u0001\u0006\u0001\u0006\u0001\u0007\u0001\u0007\u0001\b\u0001\b"+
		"\u0001\b\u0000\u0000\t\u0000\u0002\u0004\u0006\b\n\f\u000e\u0010\u0000"+
		"\u0002\u0001\u0000\u0007\b\u0001\u0000\n\u000e\u0091\u0000\u0015\u0001"+
		"\u0000\u0000\u0000\u0002,\u0001\u0000\u0000\u0000\u0004]\u0001\u0000\u0000"+
		"\u0000\u0006l\u0001\u0000\u0000\u0000\bv\u0001\u0000\u0000\u0000\nx\u0001"+
		"\u0000\u0000\u0000\f\u0080\u0001\u0000\u0000\u0000\u000e\u0082\u0001\u0000"+
		"\u0000\u0000\u0010\u0084\u0001\u0000\u0000\u0000\u0012\u0014\u0003\u0002"+
		"\u0001\u0000\u0013\u0012\u0001\u0000\u0000\u0000\u0014\u0017\u0001\u0000"+
		"\u0000\u0000\u0015\u0013\u0001\u0000\u0000\u0000\u0015\u0016\u0001\u0000"+
		"\u0000\u0000\u0016\u0018\u0001\u0000\u0000\u0000\u0017\u0015\u0001\u0000"+
		"\u0000\u0000\u0018!\u0003\u0004\u0002\u0000\u0019\u001b\u0005\u0011\u0000"+
		"\u0000\u001a\u0019\u0001\u0000\u0000\u0000\u001b\u001c\u0001\u0000\u0000"+
		"\u0000\u001c\u001a\u0001\u0000\u0000\u0000\u001c\u001d\u0001\u0000\u0000"+
		"\u0000\u001d\u001e\u0001\u0000\u0000\u0000\u001e \u0003\u0004\u0002\u0000"+
		"\u001f\u001a\u0001\u0000\u0000\u0000 #\u0001\u0000\u0000\u0000!\u001f"+
		"\u0001\u0000\u0000\u0000!\"\u0001\u0000\u0000\u0000\"\'\u0001\u0000\u0000"+
		"\u0000#!\u0001\u0000\u0000\u0000$&\u0005\u0011\u0000\u0000%$\u0001\u0000"+
		"\u0000\u0000&)\u0001\u0000\u0000\u0000\'%\u0001\u0000\u0000\u0000\'(\u0001"+
		"\u0000\u0000\u0000(*\u0001\u0000\u0000\u0000)\'\u0001\u0000\u0000\u0000"+
		"*+\u0005\u0000\u0000\u0001+\u0001\u0001\u0000\u0000\u0000,-\u0005\n\u0000"+
		"\u0000-.\u0005\t\u0000\u0000.0\u0005\r\u0000\u0000/1\u0005\u000f\u0000"+
		"\u00000/\u0001\u0000\u0000\u000001\u0001\u0000\u0000\u000013\u0001\u0000"+
		"\u0000\u000024\u0005\u0011\u0000\u000032\u0001\u0000\u0000\u000045\u0001"+
		"\u0000\u0000\u000053\u0001\u0000\u0000\u000056\u0001\u0000\u0000\u0000"+
		"6\u0003\u0001\u0000\u0000\u000078\u0003\f\u0006\u000089\u0005\u0001\u0000"+
		"\u00009;\u0001\u0000\u0000\u0000:7\u0001\u0000\u0000\u0000:;\u0001\u0000"+
		"\u0000\u0000;<\u0001\u0000\u0000\u0000<^\u0003\u0006\u0003\u0000=>\u0003"+
		"\f\u0006\u0000>?\u0005\u0001\u0000\u0000?A\u0001\u0000\u0000\u0000@=\u0001"+
		"\u0000\u0000\u0000@A\u0001\u0000\u0000\u0000AB\u0001\u0000\u0000\u0000"+
		"BF\u0005\u0005\u0000\u0000CE\u0005\u0011\u0000\u0000DC\u0001\u0000\u0000"+
		"\u0000EH\u0001\u0000\u0000\u0000FD\u0001\u0000\u0000\u0000FG\u0001\u0000"+
		"\u0000\u0000GI\u0001\u0000\u0000\u0000HF\u0001\u0000\u0000\u0000IR\u0003"+
		"\u0006\u0003\u0000JL\u0005\u0011\u0000\u0000KJ\u0001\u0000\u0000\u0000"+
		"LM\u0001\u0000\u0000\u0000MK\u0001\u0000\u0000\u0000MN\u0001\u0000\u0000"+
		"\u0000NO\u0001\u0000\u0000\u0000OQ\u0003\u0006\u0003\u0000PK\u0001\u0000"+
		"\u0000\u0000QT\u0001\u0000\u0000\u0000RP\u0001\u0000\u0000\u0000RS\u0001"+
		"\u0000\u0000\u0000SX\u0001\u0000\u0000\u0000TR\u0001\u0000\u0000\u0000"+
		"UW\u0005\u0011\u0000\u0000VU\u0001\u0000\u0000\u0000WZ\u0001\u0000\u0000"+
		"\u0000XV\u0001\u0000\u0000\u0000XY\u0001\u0000\u0000\u0000Y[\u0001\u0000"+
		"\u0000\u0000ZX\u0001\u0000\u0000\u0000[\\\u0005\u0006\u0000\u0000\\^\u0001"+
		"\u0000\u0000\u0000]:\u0001\u0000\u0000\u0000]@\u0001\u0000\u0000\u0000"+
		"^\u0005\u0001\u0000\u0000\u0000_m\u0005\u000f\u0000\u0000`a\u0003\u000e"+
		"\u0007\u0000ad\u0003\b\u0004\u0000bc\u0007\u0000\u0000\u0000ce\u0003\n"+
		"\u0005\u0000db\u0001\u0000\u0000\u0000de\u0001\u0000\u0000\u0000eg\u0001"+
		"\u0000\u0000\u0000fh\u0005\u0004\u0000\u0000gf\u0001\u0000\u0000\u0000"+
		"gh\u0001\u0000\u0000\u0000hj\u0001\u0000\u0000\u0000ik\u0005\u000f\u0000"+
		"\u0000ji\u0001\u0000\u0000\u0000jk\u0001\u0000\u0000\u0000km\u0001\u0000"+
		"\u0000\u0000l_\u0001\u0000\u0000\u0000l`\u0001\u0000\u0000\u0000m\u0007"+
		"\u0001\u0000\u0000\u0000ns\u0003\u0010\b\u0000op\u0005\u0003\u0000\u0000"+
		"pr\u0003\u0010\b\u0000qo\u0001\u0000\u0000\u0000ru\u0001\u0000\u0000\u0000"+
		"sq\u0001\u0000\u0000\u0000st\u0001\u0000\u0000\u0000tw\u0001\u0000\u0000"+
		"\u0000us\u0001\u0000\u0000\u0000vn\u0001\u0000\u0000\u0000vw\u0001\u0000"+
		"\u0000\u0000w\t\u0001\u0000\u0000\u0000x}\u0003\u0010\b\u0000yz\u0005"+
		"\u0003\u0000\u0000z|\u0003\u0010\b\u0000{y\u0001\u0000\u0000\u0000|\u007f"+
		"\u0001\u0000\u0000\u0000}{\u0001\u0000\u0000\u0000}~\u0001\u0000\u0000"+
		"\u0000~\u000b\u0001\u0000\u0000\u0000\u007f}\u0001\u0000\u0000\u0000\u0080"+
		"\u0081\u0005\n\u0000\u0000\u0081\r\u0001\u0000\u0000\u0000\u0082\u0083"+
		"\u0005\n\u0000\u0000\u0083\u000f\u0001\u0000\u0000\u0000\u0084\u0085\u0007"+
		"\u0001\u0000\u0000\u0085\u0011\u0001\u0000\u0000\u0000\u0014\u0015\u001c"+
		"!\'05:@FMRX]dgjlsv}";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}