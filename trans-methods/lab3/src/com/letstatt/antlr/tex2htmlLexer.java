// Generated from java-escape by ANTLR 4.11.1

package com.letstatt.antlr;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class tex2htmlLexer extends Lexer {
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
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"DOLLAR", "UNDERSCORE", "CARET", "LEFT_PARENTHESES", "RIGHT_PARENTHESES", 
			"LEFT_SQAURE_BRACKET", "RIGHT_SQAURE_BRACKET", "LEFT_BRACE", "RIGHT_BRACE", 
			"PLUS", "MINUS", "MULTIPLICATION", "DIVISION", "EQ", "SQRT", "FRAC", 
			"DOCUMENTCLASS", "ARTICLE", "USEPACKAGE", "DOCUMENT", "BEGIN", "END", 
			"SPACE", "WORD", "NUMBER"
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


	public tex2htmlLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "tex2html.g4"; }

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
		"\u0004\u0000\u0019\u00b4\u0006\uffff\uffff\u0002\u0000\u0007\u0000\u0002"+
		"\u0001\u0007\u0001\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002"+
		"\u0004\u0007\u0004\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002"+
		"\u0007\u0007\u0007\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002"+
		"\u000b\u0007\u000b\u0002\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e"+
		"\u0002\u000f\u0007\u000f\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011"+
		"\u0002\u0012\u0007\u0012\u0002\u0013\u0007\u0013\u0002\u0014\u0007\u0014"+
		"\u0002\u0015\u0007\u0015\u0002\u0016\u0007\u0016\u0002\u0017\u0007\u0017"+
		"\u0002\u0018\u0007\u0018\u0001\u0000\u0001\u0000\u0001\u0001\u0001\u0001"+
		"\u0001\u0002\u0001\u0002\u0001\u0003\u0001\u0003\u0001\u0004\u0001\u0004"+
		"\u0001\u0005\u0001\u0005\u0001\u0006\u0001\u0006\u0001\u0007\u0001\u0007"+
		"\u0001\b\u0001\b\u0001\t\u0001\t\u0001\n\u0001\n\u0001\u000b\u0001\u000b"+
		"\u0001\f\u0001\f\u0001\r\u0001\r\u0001\u000e\u0001\u000e\u0001\u000e\u0001"+
		"\u000e\u0001\u000e\u0001\u000e\u0001\u000f\u0001\u000f\u0001\u000f\u0001"+
		"\u000f\u0001\u000f\u0001\u000f\u0001\u0010\u0001\u0010\u0001\u0010\u0001"+
		"\u0010\u0001\u0010\u0001\u0010\u0001\u0010\u0001\u0010\u0001\u0010\u0001"+
		"\u0010\u0001\u0010\u0001\u0010\u0001\u0010\u0001\u0010\u0001\u0010\u0001"+
		"\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0001"+
		"\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0012\u0001"+
		"\u0012\u0001\u0012\u0001\u0012\u0001\u0012\u0001\u0012\u0001\u0012\u0001"+
		"\u0012\u0001\u0012\u0001\u0012\u0001\u0012\u0001\u0012\u0001\u0013\u0001"+
		"\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001"+
		"\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0014\u0001"+
		"\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001"+
		"\u0015\u0001\u0015\u0001\u0015\u0001\u0015\u0001\u0015\u0001\u0016\u0004"+
		"\u0016\u009b\b\u0016\u000b\u0016\f\u0016\u009c\u0001\u0016\u0001\u0016"+
		"\u0001\u0017\u0001\u0017\u0005\u0017\u00a3\b\u0017\n\u0017\f\u0017\u00a6"+
		"\t\u0017\u0001\u0018\u0003\u0018\u00a9\b\u0018\u0001\u0018\u0001\u0018"+
		"\u0001\u0018\u0005\u0018\u00ae\b\u0018\n\u0018\f\u0018\u00b1\t\u0018\u0003"+
		"\u0018\u00b3\b\u0018\u0000\u0000\u0019\u0001\u0001\u0003\u0002\u0005\u0003"+
		"\u0007\u0004\t\u0005\u000b\u0006\r\u0007\u000f\b\u0011\t\u0013\n\u0015"+
		"\u000b\u0017\f\u0019\r\u001b\u000e\u001d\u000f\u001f\u0010!\u0011#\u0012"+
		"%\u0013\'\u0014)\u0015+\u0016-\u0017/\u00181\u0019\u0001\u0000\u0005\u0003"+
		"\u0000\t\n\r\r  \u0002\u0000AZaz\u0003\u000009AZaz\u0001\u000019\u0001"+
		"\u000009\u00b8\u0000\u0001\u0001\u0000\u0000\u0000\u0000\u0003\u0001\u0000"+
		"\u0000\u0000\u0000\u0005\u0001\u0000\u0000\u0000\u0000\u0007\u0001\u0000"+
		"\u0000\u0000\u0000\t\u0001\u0000\u0000\u0000\u0000\u000b\u0001\u0000\u0000"+
		"\u0000\u0000\r\u0001\u0000\u0000\u0000\u0000\u000f\u0001\u0000\u0000\u0000"+
		"\u0000\u0011\u0001\u0000\u0000\u0000\u0000\u0013\u0001\u0000\u0000\u0000"+
		"\u0000\u0015\u0001\u0000\u0000\u0000\u0000\u0017\u0001\u0000\u0000\u0000"+
		"\u0000\u0019\u0001\u0000\u0000\u0000\u0000\u001b\u0001\u0000\u0000\u0000"+
		"\u0000\u001d\u0001\u0000\u0000\u0000\u0000\u001f\u0001\u0000\u0000\u0000"+
		"\u0000!\u0001\u0000\u0000\u0000\u0000#\u0001\u0000\u0000\u0000\u0000%"+
		"\u0001\u0000\u0000\u0000\u0000\'\u0001\u0000\u0000\u0000\u0000)\u0001"+
		"\u0000\u0000\u0000\u0000+\u0001\u0000\u0000\u0000\u0000-\u0001\u0000\u0000"+
		"\u0000\u0000/\u0001\u0000\u0000\u0000\u00001\u0001\u0000\u0000\u0000\u0001"+
		"3\u0001\u0000\u0000\u0000\u00035\u0001\u0000\u0000\u0000\u00057\u0001"+
		"\u0000\u0000\u0000\u00079\u0001\u0000\u0000\u0000\t;\u0001\u0000\u0000"+
		"\u0000\u000b=\u0001\u0000\u0000\u0000\r?\u0001\u0000\u0000\u0000\u000f"+
		"A\u0001\u0000\u0000\u0000\u0011C\u0001\u0000\u0000\u0000\u0013E\u0001"+
		"\u0000\u0000\u0000\u0015G\u0001\u0000\u0000\u0000\u0017I\u0001\u0000\u0000"+
		"\u0000\u0019K\u0001\u0000\u0000\u0000\u001bM\u0001\u0000\u0000\u0000\u001d"+
		"O\u0001\u0000\u0000\u0000\u001fU\u0001\u0000\u0000\u0000![\u0001\u0000"+
		"\u0000\u0000#j\u0001\u0000\u0000\u0000%u\u0001\u0000\u0000\u0000\'\u0081"+
		"\u0001\u0000\u0000\u0000)\u008d\u0001\u0000\u0000\u0000+\u0094\u0001\u0000"+
		"\u0000\u0000-\u009a\u0001\u0000\u0000\u0000/\u00a0\u0001\u0000\u0000\u0000"+
		"1\u00a8\u0001\u0000\u0000\u000034\u0005$\u0000\u00004\u0002\u0001\u0000"+
		"\u0000\u000056\u0005_\u0000\u00006\u0004\u0001\u0000\u0000\u000078\u0005"+
		"^\u0000\u00008\u0006\u0001\u0000\u0000\u00009:\u0005(\u0000\u0000:\b\u0001"+
		"\u0000\u0000\u0000;<\u0005)\u0000\u0000<\n\u0001\u0000\u0000\u0000=>\u0005"+
		"[\u0000\u0000>\f\u0001\u0000\u0000\u0000?@\u0005]\u0000\u0000@\u000e\u0001"+
		"\u0000\u0000\u0000AB\u0005{\u0000\u0000B\u0010\u0001\u0000\u0000\u0000"+
		"CD\u0005}\u0000\u0000D\u0012\u0001\u0000\u0000\u0000EF\u0005+\u0000\u0000"+
		"F\u0014\u0001\u0000\u0000\u0000GH\u0005-\u0000\u0000H\u0016\u0001\u0000"+
		"\u0000\u0000IJ\u0005*\u0000\u0000J\u0018\u0001\u0000\u0000\u0000KL\u0005"+
		"/\u0000\u0000L\u001a\u0001\u0000\u0000\u0000MN\u0005=\u0000\u0000N\u001c"+
		"\u0001\u0000\u0000\u0000OP\u0005\\\u0000\u0000PQ\u0005s\u0000\u0000QR"+
		"\u0005q\u0000\u0000RS\u0005r\u0000\u0000ST\u0005t\u0000\u0000T\u001e\u0001"+
		"\u0000\u0000\u0000UV\u0005\\\u0000\u0000VW\u0005f\u0000\u0000WX\u0005"+
		"r\u0000\u0000XY\u0005a\u0000\u0000YZ\u0005c\u0000\u0000Z \u0001\u0000"+
		"\u0000\u0000[\\\u0005\\\u0000\u0000\\]\u0005d\u0000\u0000]^\u0005o\u0000"+
		"\u0000^_\u0005c\u0000\u0000_`\u0005u\u0000\u0000`a\u0005m\u0000\u0000"+
		"ab\u0005e\u0000\u0000bc\u0005n\u0000\u0000cd\u0005t\u0000\u0000de\u0005"+
		"c\u0000\u0000ef\u0005l\u0000\u0000fg\u0005a\u0000\u0000gh\u0005s\u0000"+
		"\u0000hi\u0005s\u0000\u0000i\"\u0001\u0000\u0000\u0000jk\u0003\u000f\u0007"+
		"\u0000kl\u0005a\u0000\u0000lm\u0005r\u0000\u0000mn\u0005t\u0000\u0000"+
		"no\u0005i\u0000\u0000op\u0005c\u0000\u0000pq\u0005l\u0000\u0000qr\u0005"+
		"e\u0000\u0000rs\u0001\u0000\u0000\u0000st\u0003\u0011\b\u0000t$\u0001"+
		"\u0000\u0000\u0000uv\u0005\\\u0000\u0000vw\u0005u\u0000\u0000wx\u0005"+
		"s\u0000\u0000xy\u0005e\u0000\u0000yz\u0005p\u0000\u0000z{\u0005a\u0000"+
		"\u0000{|\u0005c\u0000\u0000|}\u0005k\u0000\u0000}~\u0005a\u0000\u0000"+
		"~\u007f\u0005g\u0000\u0000\u007f\u0080\u0005e\u0000\u0000\u0080&\u0001"+
		"\u0000\u0000\u0000\u0081\u0082\u0003\u000f\u0007\u0000\u0082\u0083\u0005"+
		"d\u0000\u0000\u0083\u0084\u0005o\u0000\u0000\u0084\u0085\u0005c\u0000"+
		"\u0000\u0085\u0086\u0005u\u0000\u0000\u0086\u0087\u0005m\u0000\u0000\u0087"+
		"\u0088\u0005e\u0000\u0000\u0088\u0089\u0005n\u0000\u0000\u0089\u008a\u0005"+
		"t\u0000\u0000\u008a\u008b\u0001\u0000\u0000\u0000\u008b\u008c\u0003\u0011"+
		"\b\u0000\u008c(\u0001\u0000\u0000\u0000\u008d\u008e\u0005\\\u0000\u0000"+
		"\u008e\u008f\u0005b\u0000\u0000\u008f\u0090\u0005e\u0000\u0000\u0090\u0091"+
		"\u0005g\u0000\u0000\u0091\u0092\u0005i\u0000\u0000\u0092\u0093\u0005n"+
		"\u0000\u0000\u0093*\u0001\u0000\u0000\u0000\u0094\u0095\u0005\\\u0000"+
		"\u0000\u0095\u0096\u0005e\u0000\u0000\u0096\u0097\u0005n\u0000\u0000\u0097"+
		"\u0098\u0005d\u0000\u0000\u0098,\u0001\u0000\u0000\u0000\u0099\u009b\u0007"+
		"\u0000\u0000\u0000\u009a\u0099\u0001\u0000\u0000\u0000\u009b\u009c\u0001"+
		"\u0000\u0000\u0000\u009c\u009a\u0001\u0000\u0000\u0000\u009c\u009d\u0001"+
		"\u0000\u0000\u0000\u009d\u009e\u0001\u0000\u0000\u0000\u009e\u009f\u0006"+
		"\u0016\u0000\u0000\u009f.\u0001\u0000\u0000\u0000\u00a0\u00a4\u0007\u0001"+
		"\u0000\u0000\u00a1\u00a3\u0007\u0002\u0000\u0000\u00a2\u00a1\u0001\u0000"+
		"\u0000\u0000\u00a3\u00a6\u0001\u0000\u0000\u0000\u00a4\u00a2\u0001\u0000"+
		"\u0000\u0000\u00a4\u00a5\u0001\u0000\u0000\u0000\u00a50\u0001\u0000\u0000"+
		"\u0000\u00a6\u00a4\u0001\u0000\u0000\u0000\u00a7\u00a9\u0005-\u0000\u0000"+
		"\u00a8\u00a7\u0001\u0000\u0000\u0000\u00a8\u00a9\u0001\u0000\u0000\u0000"+
		"\u00a9\u00b2\u0001\u0000\u0000\u0000\u00aa\u00b3\u00050\u0000\u0000\u00ab"+
		"\u00af\u0007\u0003\u0000\u0000\u00ac\u00ae\u0007\u0004\u0000\u0000\u00ad"+
		"\u00ac\u0001\u0000\u0000\u0000\u00ae\u00b1\u0001\u0000\u0000\u0000\u00af"+
		"\u00ad\u0001\u0000\u0000\u0000\u00af\u00b0\u0001\u0000\u0000\u0000\u00b0"+
		"\u00b3\u0001\u0000\u0000\u0000\u00b1\u00af\u0001\u0000\u0000\u0000\u00b2"+
		"\u00aa\u0001\u0000\u0000\u0000\u00b2\u00ab\u0001\u0000\u0000\u0000\u00b3"+
		"2\u0001\u0000\u0000\u0000\u0006\u0000\u009c\u00a4\u00a8\u00af\u00b2\u0001"+
		"\u0006\u0000\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}