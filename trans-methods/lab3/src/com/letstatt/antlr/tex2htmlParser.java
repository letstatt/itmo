// Generated from java-escape by ANTLR 4.11.1

package com.letstatt.antlr;

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class tex2htmlParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.11.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		DOLLAR=1, UNDERSCORE=2, CARET=3, LEFT_PARENTHESES=4, RIGHT_PARENTHESES=5, 
		LEFT_SQAURE_BRACKET=6, RIGHT_SQAURE_BRACKET=7, LEFT_BRACE=8, RIGHT_BRACE=9, 
		PLUS=10, MINUS=11, MULTIPLICATION=12, DIVISION=13, EQ=14, SQRT=15, FRAC=16, 
		DOCUMENTCLASS=17, ARTICLE=18, USEPACKAGE=19, DOCUMENT=20, BEGIN=21, END=22, 
		SPACE=23, WORD=24, NUMBER=25;
	public static final int
		RULE_tex2html = 0, RULE_header = 1, RULE_packages = 2, RULE_body = 3, 
		RULE_text = 4, RULE_math = 5, RULE_expr = 6, RULE_expr1 = 7, RULE_term = 8, 
		RULE_term1 = 9, RULE_factor = 10, RULE_group = 11, RULE_subsup = 12, RULE_plus = 13, 
		RULE_minus = 14, RULE_eq = 15, RULE_mul = 16, RULE_div = 17, RULE_sup = 18, 
		RULE_sub = 19, RULE_number = 20, RULE_word = 21, RULE_sqrt = 22, RULE_sqrt_base = 23, 
		RULE_frac = 24;
	private static String[] makeRuleNames() {
		return new String[] {
			"tex2html", "header", "packages", "body", "text", "math", "expr", "expr1", 
			"term", "term1", "factor", "group", "subsup", "plus", "minus", "eq", 
			"mul", "div", "sup", "sub", "number", "word", "sqrt", "sqrt_base", "frac"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'$'", "'_'", "'^'", "'('", "')'", "'['", "']'", "'{'", "'}'", 
			"'+'", "'-'", "'*'", "'/'", "'='", "'\\sqrt'", "'\\frac'", "'\\documentclass'", 
			null, "'\\usepackage'", null, "'\\begin'", "'\\end'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "DOLLAR", "UNDERSCORE", "CARET", "LEFT_PARENTHESES", "RIGHT_PARENTHESES", 
			"LEFT_SQAURE_BRACKET", "RIGHT_SQAURE_BRACKET", "LEFT_BRACE", "RIGHT_BRACE", 
			"PLUS", "MINUS", "MULTIPLICATION", "DIVISION", "EQ", "SQRT", "FRAC", 
			"DOCUMENTCLASS", "ARTICLE", "USEPACKAGE", "DOCUMENT", "BEGIN", "END", 
			"SPACE", "WORD", "NUMBER"
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
	public String getGrammarFileName() { return "java-escape"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public tex2htmlParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Tex2htmlContext extends ParserRuleContext {
		public StringBuilder res;
		public HeaderContext header() {
			return getRuleContext(HeaderContext.class,0);
		}
		public PackagesContext packages() {
			return getRuleContext(PackagesContext.class,0);
		}
		public BodyContext body() {
			return getRuleContext(BodyContext.class,0);
		}
		public TerminalNode EOF() { return getToken(tex2htmlParser.EOF, 0); }
		public Tex2htmlContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tex2html; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterTex2html(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitTex2html(this);
		}
	}

	public final Tex2htmlContext tex2html() throws RecognitionException {
		Tex2htmlContext _localctx = new Tex2htmlContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_tex2html);

		StringBuilder res = new StringBuilder();
		((Tex2htmlContext)_localctx).res =  res;

		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(50);
			header();
			setState(51);
			packages();
			setState(52);
			body(res);
			setState(53);
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
	public static class HeaderContext extends ParserRuleContext {
		public TerminalNode DOCUMENTCLASS() { return getToken(tex2htmlParser.DOCUMENTCLASS, 0); }
		public TerminalNode ARTICLE() { return getToken(tex2htmlParser.ARTICLE, 0); }
		public HeaderContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_header; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterHeader(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitHeader(this);
		}
	}

	public final HeaderContext header() throws RecognitionException {
		HeaderContext _localctx = new HeaderContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_header);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(55);
			match(DOCUMENTCLASS);
			setState(56);
			match(ARTICLE);
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
	public static class PackagesContext extends ParserRuleContext {
		public List<TerminalNode> USEPACKAGE() { return getTokens(tex2htmlParser.USEPACKAGE); }
		public TerminalNode USEPACKAGE(int i) {
			return getToken(tex2htmlParser.USEPACKAGE, i);
		}
		public List<TerminalNode> LEFT_BRACE() { return getTokens(tex2htmlParser.LEFT_BRACE); }
		public TerminalNode LEFT_BRACE(int i) {
			return getToken(tex2htmlParser.LEFT_BRACE, i);
		}
		public List<TerminalNode> WORD() { return getTokens(tex2htmlParser.WORD); }
		public TerminalNode WORD(int i) {
			return getToken(tex2htmlParser.WORD, i);
		}
		public List<TerminalNode> RIGHT_BRACE() { return getTokens(tex2htmlParser.RIGHT_BRACE); }
		public TerminalNode RIGHT_BRACE(int i) {
			return getToken(tex2htmlParser.RIGHT_BRACE, i);
		}
		public PackagesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_packages; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterPackages(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitPackages(this);
		}
	}

	public final PackagesContext packages() throws RecognitionException {
		PackagesContext _localctx = new PackagesContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_packages);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(64);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==USEPACKAGE) {
				{
				{
				setState(58);
				match(USEPACKAGE);
				setState(59);
				match(LEFT_BRACE);
				setState(60);
				match(WORD);
				setState(61);
				match(RIGHT_BRACE);
				}
				}
				setState(66);
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
	public static class BodyContext extends ParserRuleContext {
		public StringBuilder res;
		public TerminalNode BEGIN() { return getToken(tex2htmlParser.BEGIN, 0); }
		public List<TerminalNode> DOCUMENT() { return getTokens(tex2htmlParser.DOCUMENT); }
		public TerminalNode DOCUMENT(int i) {
			return getToken(tex2htmlParser.DOCUMENT, i);
		}
		public TerminalNode END() { return getToken(tex2htmlParser.END, 0); }
		public List<TextContext> text() {
			return getRuleContexts(TextContext.class);
		}
		public TextContext text(int i) {
			return getRuleContext(TextContext.class,i);
		}
		public List<MathContext> math() {
			return getRuleContexts(MathContext.class);
		}
		public MathContext math(int i) {
			return getRuleContext(MathContext.class,i);
		}
		public BodyContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public BodyContext(ParserRuleContext parent, int invokingState, StringBuilder res) {
			super(parent, invokingState);
			this.res = res;
		}
		@Override public int getRuleIndex() { return RULE_body; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterBody(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitBody(this);
		}
	}

	public final BodyContext body(StringBuilder res) throws RecognitionException {
		BodyContext _localctx = new BodyContext(_ctx, getState(), res);
		enterRule(_localctx, 6, RULE_body);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(67);
			match(BEGIN);
			setState(68);
			match(DOCUMENT);
			_localctx.res.append("<!DOCTYPE html>\n<html lang=\"ru\"><head><title>ANTLR4 example</title></head><body>\n");
			setState(72); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				setState(72);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case WORD:
				case NUMBER:
					{
					setState(70);
					text(res);
					}
					break;
				case DOLLAR:
					{
					setState(71);
					math(res);
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				}
				setState(74); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( ((_la) & ~0x3f) == 0 && ((1L << _la) & 50331650L) != 0 );
			_localctx.res.append("</body></html>\n");
			setState(77);
			match(END);
			setState(78);
			match(DOCUMENT);
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
	public static class TextContext extends ParserRuleContext {
		public StringBuilder res;
		public StringBuilder t;
		public WordContext word;
		public NumberContext number;
		public List<WordContext> word() {
			return getRuleContexts(WordContext.class);
		}
		public WordContext word(int i) {
			return getRuleContext(WordContext.class,i);
		}
		public List<NumberContext> number() {
			return getRuleContexts(NumberContext.class);
		}
		public NumberContext number(int i) {
			return getRuleContext(NumberContext.class,i);
		}
		public TextContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public TextContext(ParserRuleContext parent, int invokingState, StringBuilder res) {
			super(parent, invokingState);
			this.res = res;
		}
		@Override public int getRuleIndex() { return RULE_text; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterText(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitText(this);
		}
	}

	public final TextContext text(StringBuilder res) throws RecognitionException {
		TextContext _localctx = new TextContext(_ctx, getState(), res);
		enterRule(_localctx, 8, RULE_text);

		((TextContext)_localctx).t =  new StringBuilder();

		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			_localctx.res.append("<div><p>");
			setState(87); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					setState(87);
					_errHandler.sync(this);
					switch (_input.LA(1)) {
					case WORD:
						{
						setState(81);
						((TextContext)_localctx).word = word();
						_localctx.t.append((_localctx.t.length() > 0 ? " " : "") + (((TextContext)_localctx).word!=null?_input.getText(((TextContext)_localctx).word.start,((TextContext)_localctx).word.stop):null));
						}
						break;
					case NUMBER:
						{
						setState(84);
						((TextContext)_localctx).number = number();
						_localctx.t.append((_localctx.t.length() > 0 ? " " : "") + (((TextContext)_localctx).number!=null?_input.getText(((TextContext)_localctx).number.start,((TextContext)_localctx).number.stop):null));
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(89); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,4,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			_localctx.res.append(_localctx.t);
			    _localctx.res.append("</p></div>\n");
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
	public static class MathContext extends ParserRuleContext {
		public StringBuilder res;
		public List<TerminalNode> DOLLAR() { return getTokens(tex2htmlParser.DOLLAR); }
		public TerminalNode DOLLAR(int i) {
			return getToken(tex2htmlParser.DOLLAR, i);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public MathContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public MathContext(ParserRuleContext parent, int invokingState, StringBuilder res) {
			super(parent, invokingState);
			this.res = res;
		}
		@Override public int getRuleIndex() { return RULE_math; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterMath(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitMath(this);
		}
	}

	public final MathContext math(StringBuilder res) throws RecognitionException {
		MathContext _localctx = new MathContext(_ctx, getState(), res);
		enterRule(_localctx, 10, RULE_math);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(93);
			match(DOLLAR);
			_localctx.res.append("<math display=\"block\">");
			setState(95);
			expr(res);
			setState(96);
			match(DOLLAR);
			_localctx.res.append("</math>\n");
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
	public static class ExprContext extends ParserRuleContext {
		public StringBuilder res;
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public Expr1Context expr1() {
			return getRuleContext(Expr1Context.class,0);
		}
		public ExprContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public ExprContext(ParserRuleContext parent, int invokingState, StringBuilder res) {
			super(parent, invokingState);
			this.res = res;
		}
		@Override public int getRuleIndex() { return RULE_expr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitExpr(this);
		}
	}

	public final ExprContext expr(StringBuilder res) throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState(), res);
		enterRule(_localctx, 12, RULE_expr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(99);
			term(res);
			setState(100);
			expr1(res);
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
	public static class Expr1Context extends ParserRuleContext {
		public StringBuilder res;
		public PlusContext plus() {
			return getRuleContext(PlusContext.class,0);
		}
		public TermContext term() {
			return getRuleContext(TermContext.class,0);
		}
		public Expr1Context expr1() {
			return getRuleContext(Expr1Context.class,0);
		}
		public EqContext eq() {
			return getRuleContext(EqContext.class,0);
		}
		public MinusContext minus() {
			return getRuleContext(MinusContext.class,0);
		}
		public Expr1Context(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public Expr1Context(ParserRuleContext parent, int invokingState, StringBuilder res) {
			super(parent, invokingState);
			this.res = res;
		}
		@Override public int getRuleIndex() { return RULE_expr1; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterExpr1(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitExpr1(this);
		}
	}

	public final Expr1Context expr1(StringBuilder res) throws RecognitionException {
		Expr1Context _localctx = new Expr1Context(_ctx, getState(), res);
		enterRule(_localctx, 14, RULE_expr1);
		try {
			setState(118);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case PLUS:
				enterOuterAlt(_localctx, 1);
				{
				setState(102);
				plus();
				_localctx.res.append("<mo>+</mo>");
				setState(104);
				term(res);
				setState(105);
				expr1(res);
				}
				break;
			case EQ:
				enterOuterAlt(_localctx, 2);
				{
				setState(107);
				eq();
				_localctx.res.append("<mo>=</mo>");
				setState(109);
				term(res);
				setState(110);
				expr1(res);
				}
				break;
			case MINUS:
				enterOuterAlt(_localctx, 3);
				{
				setState(112);
				minus();
				_localctx.res.append("<mo>-</mo>");
				setState(114);
				term(res);
				setState(115);
				expr1(res);
				}
				break;
			case DOLLAR:
			case RIGHT_PARENTHESES:
			case RIGHT_SQAURE_BRACKET:
			case RIGHT_BRACE:
				enterOuterAlt(_localctx, 4);
				{
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
	public static class TermContext extends ParserRuleContext {
		public StringBuilder res;
		public FactorContext factor() {
			return getRuleContext(FactorContext.class,0);
		}
		public Term1Context term1() {
			return getRuleContext(Term1Context.class,0);
		}
		public TermContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public TermContext(ParserRuleContext parent, int invokingState, StringBuilder res) {
			super(parent, invokingState);
			this.res = res;
		}
		@Override public int getRuleIndex() { return RULE_term; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterTerm(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitTerm(this);
		}
	}

	public final TermContext term(StringBuilder res) throws RecognitionException {
		TermContext _localctx = new TermContext(_ctx, getState(), res);
		enterRule(_localctx, 16, RULE_term);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(120);
			factor(res);
			setState(121);
			term1(res);
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
	public static class Term1Context extends ParserRuleContext {
		public StringBuilder res;
		public MulContext mul() {
			return getRuleContext(MulContext.class,0);
		}
		public FactorContext factor() {
			return getRuleContext(FactorContext.class,0);
		}
		public Term1Context term1() {
			return getRuleContext(Term1Context.class,0);
		}
		public DivContext div() {
			return getRuleContext(DivContext.class,0);
		}
		public Term1Context(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public Term1Context(ParserRuleContext parent, int invokingState, StringBuilder res) {
			super(parent, invokingState);
			this.res = res;
		}
		@Override public int getRuleIndex() { return RULE_term1; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterTerm1(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitTerm1(this);
		}
	}

	public final Term1Context term1(StringBuilder res) throws RecognitionException {
		Term1Context _localctx = new Term1Context(_ctx, getState(), res);
		enterRule(_localctx, 18, RULE_term1);
		try {
			setState(134);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case MULTIPLICATION:
				enterOuterAlt(_localctx, 1);
				{
				setState(123);
				mul();
				_localctx.res.append("<mo>*</mo>");
				setState(125);
				factor(res);
				setState(126);
				term1(res);
				}
				break;
			case DIVISION:
				enterOuterAlt(_localctx, 2);
				{
				setState(128);
				div();
				_localctx.res.append("<mo>/</mo>");
				setState(130);
				factor(res);
				setState(131);
				term1(res);
				}
				break;
			case DOLLAR:
			case RIGHT_PARENTHESES:
			case RIGHT_SQAURE_BRACKET:
			case RIGHT_BRACE:
			case PLUS:
			case MINUS:
			case EQ:
				enterOuterAlt(_localctx, 3);
				{
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
	public static class FactorContext extends ParserRuleContext {
		public StringBuilder res;
		public GroupContext group() {
			return getRuleContext(GroupContext.class,0);
		}
		public SubsupContext subsup() {
			return getRuleContext(SubsupContext.class,0);
		}
		public SupContext sup() {
			return getRuleContext(SupContext.class,0);
		}
		public FactorContext factor() {
			return getRuleContext(FactorContext.class,0);
		}
		public SubContext sub() {
			return getRuleContext(SubContext.class,0);
		}
		public FactorContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public FactorContext(ParserRuleContext parent, int invokingState, StringBuilder res) {
			super(parent, invokingState);
			this.res = res;
		}
		@Override public int getRuleIndex() { return RULE_factor; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterFactor(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitFactor(this);
		}
	}

	public final FactorContext factor(StringBuilder res) throws RecognitionException {
		FactorContext _localctx = new FactorContext(_ctx, getState(), res);
		enterRule(_localctx, 20, RULE_factor);
		try {
			setState(155);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,7,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				_localctx.res.append("<mrow>");
				setState(137);
				group(res);
				_localctx.res.append("</mrow>");
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(140);
				subsup(res);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				_localctx.res.append("<msup><mrow>");
				setState(142);
				group(res);
				_localctx.res.append("</mrow>");
				setState(144);
				sup();
				setState(145);
				factor(res);
				_localctx.res.append("</msup>");
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				_localctx.res.append("<msub><mrow>");
				setState(149);
				group(res);
				_localctx.res.append("</mrow>");
				setState(151);
				sub();
				setState(152);
				factor(res);
				_localctx.res.append("</msub>");
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
	public static class GroupContext extends ParserRuleContext {
		public StringBuilder res;
		public NumberContext number;
		public WordContext word;
		public NumberContext number() {
			return getRuleContext(NumberContext.class,0);
		}
		public WordContext word() {
			return getRuleContext(WordContext.class,0);
		}
		public SqrtContext sqrt() {
			return getRuleContext(SqrtContext.class,0);
		}
		public FracContext frac() {
			return getRuleContext(FracContext.class,0);
		}
		public TerminalNode LEFT_PARENTHESES() { return getToken(tex2htmlParser.LEFT_PARENTHESES, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RIGHT_PARENTHESES() { return getToken(tex2htmlParser.RIGHT_PARENTHESES, 0); }
		public TerminalNode LEFT_BRACE() { return getToken(tex2htmlParser.LEFT_BRACE, 0); }
		public TerminalNode RIGHT_BRACE() { return getToken(tex2htmlParser.RIGHT_BRACE, 0); }
		public GroupContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public GroupContext(ParserRuleContext parent, int invokingState, StringBuilder res) {
			super(parent, invokingState);
			this.res = res;
		}
		@Override public int getRuleIndex() { return RULE_group; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterGroup(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitGroup(this);
		}
	}

	public final GroupContext group(StringBuilder res) throws RecognitionException {
		GroupContext _localctx = new GroupContext(_ctx, getState(), res);
		enterRule(_localctx, 22, RULE_group);
		try {
			setState(179);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,8,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(157);
				((GroupContext)_localctx).number = number();
				_localctx.res.append("<mn>" + (((GroupContext)_localctx).number!=null?_input.getText(((GroupContext)_localctx).number.start,((GroupContext)_localctx).number.stop):null) + "</mn>");
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(160);
				((GroupContext)_localctx).word = word();
				_localctx.res.append("<mi>" + (((GroupContext)_localctx).word!=null?_input.getText(((GroupContext)_localctx).word.start,((GroupContext)_localctx).word.stop):null) + "</mi>");
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(163);
				((GroupContext)_localctx).number = number();
				setState(164);
				((GroupContext)_localctx).word = word();
				_localctx.res.append("<mn>" + (((GroupContext)_localctx).number!=null?_input.getText(((GroupContext)_localctx).number.start,((GroupContext)_localctx).number.stop):null) + "</mn><mi>" + (((GroupContext)_localctx).word!=null?_input.getText(((GroupContext)_localctx).word.start,((GroupContext)_localctx).word.stop):null) + "<mi>");
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(167);
				sqrt(res);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(168);
				frac(res);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(169);
				match(LEFT_PARENTHESES);
				_localctx.res.append("<mo>(</mo>");
				setState(171);
				expr(res);
				setState(172);
				match(RIGHT_PARENTHESES);
				_localctx.res.append("<mo>)</mo>");
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(175);
				match(LEFT_BRACE);
				setState(176);
				expr(res);
				setState(177);
				match(RIGHT_BRACE);
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
	public static class SubsupContext extends ParserRuleContext {
		public StringBuilder res;
		public StringBuilder left;
		public StringBuilder right;
		public GroupContext group() {
			return getRuleContext(GroupContext.class,0);
		}
		public SubContext sub() {
			return getRuleContext(SubContext.class,0);
		}
		public List<FactorContext> factor() {
			return getRuleContexts(FactorContext.class);
		}
		public FactorContext factor(int i) {
			return getRuleContext(FactorContext.class,i);
		}
		public SupContext sup() {
			return getRuleContext(SupContext.class,0);
		}
		public SubsupContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public SubsupContext(ParserRuleContext parent, int invokingState, StringBuilder res) {
			super(parent, invokingState);
			this.res = res;
		}
		@Override public int getRuleIndex() { return RULE_subsup; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterSubsup(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitSubsup(this);
		}
	}

	public final SubsupContext subsup(StringBuilder res) throws RecognitionException {
		SubsupContext _localctx = new SubsupContext(_ctx, getState(), res);
		enterRule(_localctx, 24, RULE_subsup);

		//StringBuilder left = new StringBuilder();
		//StringBuilder right = new StringBuilder();
		((SubsupContext)_localctx).left =  new StringBuilder();
		((SubsupContext)_localctx).right =  new StringBuilder();

		try {
			setState(199);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,9,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				_localctx.res.append("<msubsup><mrow>");
				setState(182);
				group(res);
				_localctx.res.append("</mrow>");
				setState(184);
				sub();
				setState(185);
				factor(res);
				setState(186);
				sup();
				setState(187);
				factor(res);
				_localctx.res.append("</msubsup>");
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				_localctx.res.append("<msubsup><mrow>");
				setState(191);
				group(res);
				_localctx.res.append("</mrow>");
				setState(193);
				sup();
				setState(194);
				factor(_localctx.right);
				setState(195);
				sub();
				setState(196);
				factor(_localctx.left);
				_localctx.res.append(_localctx.left);
				      _localctx.res.append(_localctx.right);
				      _localctx.res.append("</msubsup>");
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
	public static class PlusContext extends ParserRuleContext {
		public TerminalNode PLUS() { return getToken(tex2htmlParser.PLUS, 0); }
		public PlusContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_plus; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterPlus(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitPlus(this);
		}
	}

	public final PlusContext plus() throws RecognitionException {
		PlusContext _localctx = new PlusContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_plus);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(201);
			match(PLUS);
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
	public static class MinusContext extends ParserRuleContext {
		public TerminalNode MINUS() { return getToken(tex2htmlParser.MINUS, 0); }
		public MinusContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_minus; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterMinus(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitMinus(this);
		}
	}

	public final MinusContext minus() throws RecognitionException {
		MinusContext _localctx = new MinusContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_minus);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(203);
			match(MINUS);
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
	public static class EqContext extends ParserRuleContext {
		public TerminalNode EQ() { return getToken(tex2htmlParser.EQ, 0); }
		public EqContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_eq; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterEq(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitEq(this);
		}
	}

	public final EqContext eq() throws RecognitionException {
		EqContext _localctx = new EqContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_eq);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(205);
			match(EQ);
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
	public static class MulContext extends ParserRuleContext {
		public TerminalNode MULTIPLICATION() { return getToken(tex2htmlParser.MULTIPLICATION, 0); }
		public MulContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_mul; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterMul(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitMul(this);
		}
	}

	public final MulContext mul() throws RecognitionException {
		MulContext _localctx = new MulContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_mul);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(207);
			match(MULTIPLICATION);
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
	public static class DivContext extends ParserRuleContext {
		public TerminalNode DIVISION() { return getToken(tex2htmlParser.DIVISION, 0); }
		public DivContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_div; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterDiv(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitDiv(this);
		}
	}

	public final DivContext div() throws RecognitionException {
		DivContext _localctx = new DivContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_div);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(209);
			match(DIVISION);
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
	public static class SupContext extends ParserRuleContext {
		public TerminalNode CARET() { return getToken(tex2htmlParser.CARET, 0); }
		public SupContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_sup; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterSup(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitSup(this);
		}
	}

	public final SupContext sup() throws RecognitionException {
		SupContext _localctx = new SupContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_sup);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(211);
			match(CARET);
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
	public static class SubContext extends ParserRuleContext {
		public TerminalNode UNDERSCORE() { return getToken(tex2htmlParser.UNDERSCORE, 0); }
		public SubContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_sub; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterSub(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitSub(this);
		}
	}

	public final SubContext sub() throws RecognitionException {
		SubContext _localctx = new SubContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_sub);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(213);
			match(UNDERSCORE);
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
		public TerminalNode NUMBER() { return getToken(tex2htmlParser.NUMBER, 0); }
		public NumberContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_number; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterNumber(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitNumber(this);
		}
	}

	public final NumberContext number() throws RecognitionException {
		NumberContext _localctx = new NumberContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_number);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(215);
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
		public TerminalNode WORD() { return getToken(tex2htmlParser.WORD, 0); }
		public WordContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_word; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterWord(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitWord(this);
		}
	}

	public final WordContext word() throws RecognitionException {
		WordContext _localctx = new WordContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_word);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(217);
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

	@SuppressWarnings("CheckReturnValue")
	public static class SqrtContext extends ParserRuleContext {
		public StringBuilder res;
		public Sqrt_baseContext sqrt_base;
		public TerminalNode SQRT() { return getToken(tex2htmlParser.SQRT, 0); }
		public Sqrt_baseContext sqrt_base() {
			return getRuleContext(Sqrt_baseContext.class,0);
		}
		public TerminalNode LEFT_BRACE() { return getToken(tex2htmlParser.LEFT_BRACE, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RIGHT_BRACE() { return getToken(tex2htmlParser.RIGHT_BRACE, 0); }
		public SqrtContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public SqrtContext(ParserRuleContext parent, int invokingState, StringBuilder res) {
			super(parent, invokingState);
			this.res = res;
		}
		@Override public int getRuleIndex() { return RULE_sqrt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterSqrt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitSqrt(this);
		}
	}

	public final SqrtContext sqrt(StringBuilder res) throws RecognitionException {
		SqrtContext _localctx = new SqrtContext(_ctx, getState(), res);
		enterRule(_localctx, 44, RULE_sqrt);
		try {
			setState(234);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,10,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(219);
				match(SQRT);
				setState(220);
				((SqrtContext)_localctx).sqrt_base = sqrt_base();
				_localctx.res.append("<mroot><mrow>");
				setState(222);
				match(LEFT_BRACE);
				setState(223);
				expr(res);
				setState(224);
				match(RIGHT_BRACE);
				_localctx.res.append("</mrow><mrow>");
				      _localctx.res.append(((SqrtContext)_localctx).sqrt_base.e);
				      _localctx.res.append("</mrow></mroot>");
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(227);
				match(SQRT);
				_localctx.res.append("<msqrt><mrow>");
				setState(229);
				match(LEFT_BRACE);
				setState(230);
				expr(res);
				setState(231);
				match(RIGHT_BRACE);
				_localctx.res.append("</mrow></msqrt>");
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
	public static class Sqrt_baseContext extends ParserRuleContext {
		public StringBuilder e;
		public TerminalNode LEFT_SQAURE_BRACKET() { return getToken(tex2htmlParser.LEFT_SQAURE_BRACKET, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RIGHT_SQAURE_BRACKET() { return getToken(tex2htmlParser.RIGHT_SQAURE_BRACKET, 0); }
		public Sqrt_baseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_sqrt_base; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterSqrt_base(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitSqrt_base(this);
		}
	}

	public final Sqrt_baseContext sqrt_base() throws RecognitionException {
		Sqrt_baseContext _localctx = new Sqrt_baseContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_sqrt_base);

		StringBuilder e = new StringBuilder();
		((Sqrt_baseContext)_localctx).e =  e;

		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(236);
			match(LEFT_SQAURE_BRACKET);
			setState(237);
			expr(e);
			setState(238);
			match(RIGHT_SQAURE_BRACKET);
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
	public static class FracContext extends ParserRuleContext {
		public StringBuilder res;
		public TerminalNode FRAC() { return getToken(tex2htmlParser.FRAC, 0); }
		public List<TerminalNode> LEFT_BRACE() { return getTokens(tex2htmlParser.LEFT_BRACE); }
		public TerminalNode LEFT_BRACE(int i) {
			return getToken(tex2htmlParser.LEFT_BRACE, i);
		}
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public List<TerminalNode> RIGHT_BRACE() { return getTokens(tex2htmlParser.RIGHT_BRACE); }
		public TerminalNode RIGHT_BRACE(int i) {
			return getToken(tex2htmlParser.RIGHT_BRACE, i);
		}
		public FracContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public FracContext(ParserRuleContext parent, int invokingState, StringBuilder res) {
			super(parent, invokingState);
			this.res = res;
		}
		@Override public int getRuleIndex() { return RULE_frac; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).enterFrac(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof tex2htmlListener ) ((tex2htmlListener)listener).exitFrac(this);
		}
	}

	public final FracContext frac(StringBuilder res) throws RecognitionException {
		FracContext _localctx = new FracContext(_ctx, getState(), res);
		enterRule(_localctx, 48, RULE_frac);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(240);
			match(FRAC);
			_localctx.res.append("<mfrac><mrow>");
			setState(242);
			match(LEFT_BRACE);
			setState(243);
			expr(res);
			setState(244);
			match(RIGHT_BRACE);
			_localctx.res.append("</mrow><mrow>");
			setState(246);
			match(LEFT_BRACE);
			setState(247);
			expr(res);
			setState(248);
			match(RIGHT_BRACE);
			_localctx.res.append("</mrow></mfrac>");
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
		"\u0004\u0001\u0019\u00fc\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001"+
		"\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004"+
		"\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007"+
		"\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b"+
		"\u0002\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007"+
		"\u000f\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011\u0002\u0012\u0007"+
		"\u0012\u0002\u0013\u0007\u0013\u0002\u0014\u0007\u0014\u0002\u0015\u0007"+
		"\u0015\u0002\u0016\u0007\u0016\u0002\u0017\u0007\u0017\u0002\u0018\u0007"+
		"\u0018\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0005\u0002?\b\u0002\n\u0002\f\u0002B\t\u0002\u0001\u0003\u0001"+
		"\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0004\u0003I\b\u0003\u000b"+
		"\u0003\f\u0003J\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001"+
		"\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001"+
		"\u0004\u0004\u0004X\b\u0004\u000b\u0004\f\u0004Y\u0001\u0004\u0001\u0004"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0003\u0007w\b\u0007\u0001\b\u0001\b\u0001\b\u0001\t\u0001"+
		"\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001"+
		"\t\u0003\t\u0087\b\t\u0001\n\u0001\n\u0001\n\u0001\n\u0001\n\u0001\n\u0001"+
		"\n\u0001\n\u0001\n\u0001\n\u0001\n\u0001\n\u0001\n\u0001\n\u0001\n\u0001"+
		"\n\u0001\n\u0001\n\u0001\n\u0003\n\u009c\b\n\u0001\u000b\u0001\u000b\u0001"+
		"\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001"+
		"\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001"+
		"\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001"+
		"\u000b\u0001\u000b\u0003\u000b\u00b4\b\u000b\u0001\f\u0001\f\u0001\f\u0001"+
		"\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001"+
		"\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0003\f\u00c8\b\f\u0001\r\u0001"+
		"\r\u0001\u000e\u0001\u000e\u0001\u000f\u0001\u000f\u0001\u0010\u0001\u0010"+
		"\u0001\u0011\u0001\u0011\u0001\u0012\u0001\u0012\u0001\u0013\u0001\u0013"+
		"\u0001\u0014\u0001\u0014\u0001\u0015\u0001\u0015\u0001\u0016\u0001\u0016"+
		"\u0001\u0016\u0001\u0016\u0001\u0016\u0001\u0016\u0001\u0016\u0001\u0016"+
		"\u0001\u0016\u0001\u0016\u0001\u0016\u0001\u0016\u0001\u0016\u0001\u0016"+
		"\u0001\u0016\u0003\u0016\u00eb\b\u0016\u0001\u0017\u0001\u0017\u0001\u0017"+
		"\u0001\u0017\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0018"+
		"\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0018"+
		"\u0001\u0018\u0000\u0000\u0019\u0000\u0002\u0004\u0006\b\n\f\u000e\u0010"+
		"\u0012\u0014\u0016\u0018\u001a\u001c\u001e \"$&(*,.0\u0000\u0000\u00f7"+
		"\u00002\u0001\u0000\u0000\u0000\u00027\u0001\u0000\u0000\u0000\u0004@"+
		"\u0001\u0000\u0000\u0000\u0006C\u0001\u0000\u0000\u0000\bP\u0001\u0000"+
		"\u0000\u0000\n]\u0001\u0000\u0000\u0000\fc\u0001\u0000\u0000\u0000\u000e"+
		"v\u0001\u0000\u0000\u0000\u0010x\u0001\u0000\u0000\u0000\u0012\u0086\u0001"+
		"\u0000\u0000\u0000\u0014\u009b\u0001\u0000\u0000\u0000\u0016\u00b3\u0001"+
		"\u0000\u0000\u0000\u0018\u00c7\u0001\u0000\u0000\u0000\u001a\u00c9\u0001"+
		"\u0000\u0000\u0000\u001c\u00cb\u0001\u0000\u0000\u0000\u001e\u00cd\u0001"+
		"\u0000\u0000\u0000 \u00cf\u0001\u0000\u0000\u0000\"\u00d1\u0001\u0000"+
		"\u0000\u0000$\u00d3\u0001\u0000\u0000\u0000&\u00d5\u0001\u0000\u0000\u0000"+
		"(\u00d7\u0001\u0000\u0000\u0000*\u00d9\u0001\u0000\u0000\u0000,\u00ea"+
		"\u0001\u0000\u0000\u0000.\u00ec\u0001\u0000\u0000\u00000\u00f0\u0001\u0000"+
		"\u0000\u000023\u0003\u0002\u0001\u000034\u0003\u0004\u0002\u000045\u0003"+
		"\u0006\u0003\u000056\u0005\u0000\u0000\u00016\u0001\u0001\u0000\u0000"+
		"\u000078\u0005\u0011\u0000\u000089\u0005\u0012\u0000\u00009\u0003\u0001"+
		"\u0000\u0000\u0000:;\u0005\u0013\u0000\u0000;<\u0005\b\u0000\u0000<=\u0005"+
		"\u0018\u0000\u0000=?\u0005\t\u0000\u0000>:\u0001\u0000\u0000\u0000?B\u0001"+
		"\u0000\u0000\u0000@>\u0001\u0000\u0000\u0000@A\u0001\u0000\u0000\u0000"+
		"A\u0005\u0001\u0000\u0000\u0000B@\u0001\u0000\u0000\u0000CD\u0005\u0015"+
		"\u0000\u0000DE\u0005\u0014\u0000\u0000EH\u0006\u0003\uffff\uffff\u0000"+
		"FI\u0003\b\u0004\u0000GI\u0003\n\u0005\u0000HF\u0001\u0000\u0000\u0000"+
		"HG\u0001\u0000\u0000\u0000IJ\u0001\u0000\u0000\u0000JH\u0001\u0000\u0000"+
		"\u0000JK\u0001\u0000\u0000\u0000KL\u0001\u0000\u0000\u0000LM\u0006\u0003"+
		"\uffff\uffff\u0000MN\u0005\u0016\u0000\u0000NO\u0005\u0014\u0000\u0000"+
		"O\u0007\u0001\u0000\u0000\u0000PW\u0006\u0004\uffff\uffff\u0000QR\u0003"+
		"*\u0015\u0000RS\u0006\u0004\uffff\uffff\u0000SX\u0001\u0000\u0000\u0000"+
		"TU\u0003(\u0014\u0000UV\u0006\u0004\uffff\uffff\u0000VX\u0001\u0000\u0000"+
		"\u0000WQ\u0001\u0000\u0000\u0000WT\u0001\u0000\u0000\u0000XY\u0001\u0000"+
		"\u0000\u0000YW\u0001\u0000\u0000\u0000YZ\u0001\u0000\u0000\u0000Z[\u0001"+
		"\u0000\u0000\u0000[\\\u0006\u0004\uffff\uffff\u0000\\\t\u0001\u0000\u0000"+
		"\u0000]^\u0005\u0001\u0000\u0000^_\u0006\u0005\uffff\uffff\u0000_`\u0003"+
		"\f\u0006\u0000`a\u0005\u0001\u0000\u0000ab\u0006\u0005\uffff\uffff\u0000"+
		"b\u000b\u0001\u0000\u0000\u0000cd\u0003\u0010\b\u0000de\u0003\u000e\u0007"+
		"\u0000e\r\u0001\u0000\u0000\u0000fg\u0003\u001a\r\u0000gh\u0006\u0007"+
		"\uffff\uffff\u0000hi\u0003\u0010\b\u0000ij\u0003\u000e\u0007\u0000jw\u0001"+
		"\u0000\u0000\u0000kl\u0003\u001e\u000f\u0000lm\u0006\u0007\uffff\uffff"+
		"\u0000mn\u0003\u0010\b\u0000no\u0003\u000e\u0007\u0000ow\u0001\u0000\u0000"+
		"\u0000pq\u0003\u001c\u000e\u0000qr\u0006\u0007\uffff\uffff\u0000rs\u0003"+
		"\u0010\b\u0000st\u0003\u000e\u0007\u0000tw\u0001\u0000\u0000\u0000uw\u0001"+
		"\u0000\u0000\u0000vf\u0001\u0000\u0000\u0000vk\u0001\u0000\u0000\u0000"+
		"vp\u0001\u0000\u0000\u0000vu\u0001\u0000\u0000\u0000w\u000f\u0001\u0000"+
		"\u0000\u0000xy\u0003\u0014\n\u0000yz\u0003\u0012\t\u0000z\u0011\u0001"+
		"\u0000\u0000\u0000{|\u0003 \u0010\u0000|}\u0006\t\uffff\uffff\u0000}~"+
		"\u0003\u0014\n\u0000~\u007f\u0003\u0012\t\u0000\u007f\u0087\u0001\u0000"+
		"\u0000\u0000\u0080\u0081\u0003\"\u0011\u0000\u0081\u0082\u0006\t\uffff"+
		"\uffff\u0000\u0082\u0083\u0003\u0014\n\u0000\u0083\u0084\u0003\u0012\t"+
		"\u0000\u0084\u0087\u0001\u0000\u0000\u0000\u0085\u0087\u0001\u0000\u0000"+
		"\u0000\u0086{\u0001\u0000\u0000\u0000\u0086\u0080\u0001\u0000\u0000\u0000"+
		"\u0086\u0085\u0001\u0000\u0000\u0000\u0087\u0013\u0001\u0000\u0000\u0000"+
		"\u0088\u0089\u0006\n\uffff\uffff\u0000\u0089\u008a\u0003\u0016\u000b\u0000"+
		"\u008a\u008b\u0006\n\uffff\uffff\u0000\u008b\u009c\u0001\u0000\u0000\u0000"+
		"\u008c\u009c\u0003\u0018\f\u0000\u008d\u008e\u0006\n\uffff\uffff\u0000"+
		"\u008e\u008f\u0003\u0016\u000b\u0000\u008f\u0090\u0006\n\uffff\uffff\u0000"+
		"\u0090\u0091\u0003$\u0012\u0000\u0091\u0092\u0003\u0014\n\u0000\u0092"+
		"\u0093\u0006\n\uffff\uffff\u0000\u0093\u009c\u0001\u0000\u0000\u0000\u0094"+
		"\u0095\u0006\n\uffff\uffff\u0000\u0095\u0096\u0003\u0016\u000b\u0000\u0096"+
		"\u0097\u0006\n\uffff\uffff\u0000\u0097\u0098\u0003&\u0013\u0000\u0098"+
		"\u0099\u0003\u0014\n\u0000\u0099\u009a\u0006\n\uffff\uffff\u0000\u009a"+
		"\u009c\u0001\u0000\u0000\u0000\u009b\u0088\u0001\u0000\u0000\u0000\u009b"+
		"\u008c\u0001\u0000\u0000\u0000\u009b\u008d\u0001\u0000\u0000\u0000\u009b"+
		"\u0094\u0001\u0000\u0000\u0000\u009c\u0015\u0001\u0000\u0000\u0000\u009d"+
		"\u009e\u0003(\u0014\u0000\u009e\u009f\u0006\u000b\uffff\uffff\u0000\u009f"+
		"\u00b4\u0001\u0000\u0000\u0000\u00a0\u00a1\u0003*\u0015\u0000\u00a1\u00a2"+
		"\u0006\u000b\uffff\uffff\u0000\u00a2\u00b4\u0001\u0000\u0000\u0000\u00a3"+
		"\u00a4\u0003(\u0014\u0000\u00a4\u00a5\u0003*\u0015\u0000\u00a5\u00a6\u0006"+
		"\u000b\uffff\uffff\u0000\u00a6\u00b4\u0001\u0000\u0000\u0000\u00a7\u00b4"+
		"\u0003,\u0016\u0000\u00a8\u00b4\u00030\u0018\u0000\u00a9\u00aa\u0005\u0004"+
		"\u0000\u0000\u00aa\u00ab\u0006\u000b\uffff\uffff\u0000\u00ab\u00ac\u0003"+
		"\f\u0006\u0000\u00ac\u00ad\u0005\u0005\u0000\u0000\u00ad\u00ae\u0006\u000b"+
		"\uffff\uffff\u0000\u00ae\u00b4\u0001\u0000\u0000\u0000\u00af\u00b0\u0005"+
		"\b\u0000\u0000\u00b0\u00b1\u0003\f\u0006\u0000\u00b1\u00b2\u0005\t\u0000"+
		"\u0000\u00b2\u00b4\u0001\u0000\u0000\u0000\u00b3\u009d\u0001\u0000\u0000"+
		"\u0000\u00b3\u00a0\u0001\u0000\u0000\u0000\u00b3\u00a3\u0001\u0000\u0000"+
		"\u0000\u00b3\u00a7\u0001\u0000\u0000\u0000\u00b3\u00a8\u0001\u0000\u0000"+
		"\u0000\u00b3\u00a9\u0001\u0000\u0000\u0000\u00b3\u00af\u0001\u0000\u0000"+
		"\u0000\u00b4\u0017\u0001\u0000\u0000\u0000\u00b5\u00b6\u0006\f\uffff\uffff"+
		"\u0000\u00b6\u00b7\u0003\u0016\u000b\u0000\u00b7\u00b8\u0006\f\uffff\uffff"+
		"\u0000\u00b8\u00b9\u0003&\u0013\u0000\u00b9\u00ba\u0003\u0014\n\u0000"+
		"\u00ba\u00bb\u0003$\u0012\u0000\u00bb\u00bc\u0003\u0014\n\u0000\u00bc"+
		"\u00bd\u0006\f\uffff\uffff\u0000\u00bd\u00c8\u0001\u0000\u0000\u0000\u00be"+
		"\u00bf\u0006\f\uffff\uffff\u0000\u00bf\u00c0\u0003\u0016\u000b\u0000\u00c0"+
		"\u00c1\u0006\f\uffff\uffff\u0000\u00c1\u00c2\u0003$\u0012\u0000\u00c2"+
		"\u00c3\u0003\u0014\n\u0000\u00c3\u00c4\u0003&\u0013\u0000\u00c4\u00c5"+
		"\u0003\u0014\n\u0000\u00c5\u00c6\u0006\f\uffff\uffff\u0000\u00c6\u00c8"+
		"\u0001\u0000\u0000\u0000\u00c7\u00b5\u0001\u0000\u0000\u0000\u00c7\u00be"+
		"\u0001\u0000\u0000\u0000\u00c8\u0019\u0001\u0000\u0000\u0000\u00c9\u00ca"+
		"\u0005\n\u0000\u0000\u00ca\u001b\u0001\u0000\u0000\u0000\u00cb\u00cc\u0005"+
		"\u000b\u0000\u0000\u00cc\u001d\u0001\u0000\u0000\u0000\u00cd\u00ce\u0005"+
		"\u000e\u0000\u0000\u00ce\u001f\u0001\u0000\u0000\u0000\u00cf\u00d0\u0005"+
		"\f\u0000\u0000\u00d0!\u0001\u0000\u0000\u0000\u00d1\u00d2\u0005\r\u0000"+
		"\u0000\u00d2#\u0001\u0000\u0000\u0000\u00d3\u00d4\u0005\u0003\u0000\u0000"+
		"\u00d4%\u0001\u0000\u0000\u0000\u00d5\u00d6\u0005\u0002\u0000\u0000\u00d6"+
		"\'\u0001\u0000\u0000\u0000\u00d7\u00d8\u0005\u0019\u0000\u0000\u00d8)"+
		"\u0001\u0000\u0000\u0000\u00d9\u00da\u0005\u0018\u0000\u0000\u00da+\u0001"+
		"\u0000\u0000\u0000\u00db\u00dc\u0005\u000f\u0000\u0000\u00dc\u00dd\u0003"+
		".\u0017\u0000\u00dd\u00de\u0006\u0016\uffff\uffff\u0000\u00de\u00df\u0005"+
		"\b\u0000\u0000\u00df\u00e0\u0003\f\u0006\u0000\u00e0\u00e1\u0005\t\u0000"+
		"\u0000\u00e1\u00e2\u0006\u0016\uffff\uffff\u0000\u00e2\u00eb\u0001\u0000"+
		"\u0000\u0000\u00e3\u00e4\u0005\u000f\u0000\u0000\u00e4\u00e5\u0006\u0016"+
		"\uffff\uffff\u0000\u00e5\u00e6\u0005\b\u0000\u0000\u00e6\u00e7\u0003\f"+
		"\u0006\u0000\u00e7\u00e8\u0005\t\u0000\u0000\u00e8\u00e9\u0006\u0016\uffff"+
		"\uffff\u0000\u00e9\u00eb\u0001\u0000\u0000\u0000\u00ea\u00db\u0001\u0000"+
		"\u0000\u0000\u00ea\u00e3\u0001\u0000\u0000\u0000\u00eb-\u0001\u0000\u0000"+
		"\u0000\u00ec\u00ed\u0005\u0006\u0000\u0000\u00ed\u00ee\u0003\f\u0006\u0000"+
		"\u00ee\u00ef\u0005\u0007\u0000\u0000\u00ef/\u0001\u0000\u0000\u0000\u00f0"+
		"\u00f1\u0005\u0010\u0000\u0000\u00f1\u00f2\u0006\u0018\uffff\uffff\u0000"+
		"\u00f2\u00f3\u0005\b\u0000\u0000\u00f3\u00f4\u0003\f\u0006\u0000\u00f4"+
		"\u00f5\u0005\t\u0000\u0000\u00f5\u00f6\u0006\u0018\uffff\uffff\u0000\u00f6"+
		"\u00f7\u0005\b\u0000\u0000\u00f7\u00f8\u0003\f\u0006\u0000\u00f8\u00f9"+
		"\u0005\t\u0000\u0000\u00f9\u00fa\u0006\u0018\uffff\uffff\u0000\u00fa1"+
		"\u0001\u0000\u0000\u0000\u000b@HJWYv\u0086\u009b\u00b3\u00c7\u00ea";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}