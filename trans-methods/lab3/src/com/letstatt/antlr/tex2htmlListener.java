// Generated from java-escape by ANTLR 4.11.1

package com.letstatt.antlr;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link tex2htmlParser}.
 */
public interface tex2htmlListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#tex2html}.
	 * @param ctx the parse tree
	 */
	void enterTex2html(tex2htmlParser.Tex2htmlContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#tex2html}.
	 * @param ctx the parse tree
	 */
	void exitTex2html(tex2htmlParser.Tex2htmlContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#header}.
	 * @param ctx the parse tree
	 */
	void enterHeader(tex2htmlParser.HeaderContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#header}.
	 * @param ctx the parse tree
	 */
	void exitHeader(tex2htmlParser.HeaderContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#packages}.
	 * @param ctx the parse tree
	 */
	void enterPackages(tex2htmlParser.PackagesContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#packages}.
	 * @param ctx the parse tree
	 */
	void exitPackages(tex2htmlParser.PackagesContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#body}.
	 * @param ctx the parse tree
	 */
	void enterBody(tex2htmlParser.BodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#body}.
	 * @param ctx the parse tree
	 */
	void exitBody(tex2htmlParser.BodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#text}.
	 * @param ctx the parse tree
	 */
	void enterText(tex2htmlParser.TextContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#text}.
	 * @param ctx the parse tree
	 */
	void exitText(tex2htmlParser.TextContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#math}.
	 * @param ctx the parse tree
	 */
	void enterMath(tex2htmlParser.MathContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#math}.
	 * @param ctx the parse tree
	 */
	void exitMath(tex2htmlParser.MathContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterExpr(tex2htmlParser.ExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitExpr(tex2htmlParser.ExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#expr1}.
	 * @param ctx the parse tree
	 */
	void enterExpr1(tex2htmlParser.Expr1Context ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#expr1}.
	 * @param ctx the parse tree
	 */
	void exitExpr1(tex2htmlParser.Expr1Context ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#term}.
	 * @param ctx the parse tree
	 */
	void enterTerm(tex2htmlParser.TermContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#term}.
	 * @param ctx the parse tree
	 */
	void exitTerm(tex2htmlParser.TermContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#term1}.
	 * @param ctx the parse tree
	 */
	void enterTerm1(tex2htmlParser.Term1Context ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#term1}.
	 * @param ctx the parse tree
	 */
	void exitTerm1(tex2htmlParser.Term1Context ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#factor}.
	 * @param ctx the parse tree
	 */
	void enterFactor(tex2htmlParser.FactorContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#factor}.
	 * @param ctx the parse tree
	 */
	void exitFactor(tex2htmlParser.FactorContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#group}.
	 * @param ctx the parse tree
	 */
	void enterGroup(tex2htmlParser.GroupContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#group}.
	 * @param ctx the parse tree
	 */
	void exitGroup(tex2htmlParser.GroupContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#subsup}.
	 * @param ctx the parse tree
	 */
	void enterSubsup(tex2htmlParser.SubsupContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#subsup}.
	 * @param ctx the parse tree
	 */
	void exitSubsup(tex2htmlParser.SubsupContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#plus}.
	 * @param ctx the parse tree
	 */
	void enterPlus(tex2htmlParser.PlusContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#plus}.
	 * @param ctx the parse tree
	 */
	void exitPlus(tex2htmlParser.PlusContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#minus}.
	 * @param ctx the parse tree
	 */
	void enterMinus(tex2htmlParser.MinusContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#minus}.
	 * @param ctx the parse tree
	 */
	void exitMinus(tex2htmlParser.MinusContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#eq}.
	 * @param ctx the parse tree
	 */
	void enterEq(tex2htmlParser.EqContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#eq}.
	 * @param ctx the parse tree
	 */
	void exitEq(tex2htmlParser.EqContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#mul}.
	 * @param ctx the parse tree
	 */
	void enterMul(tex2htmlParser.MulContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#mul}.
	 * @param ctx the parse tree
	 */
	void exitMul(tex2htmlParser.MulContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#div}.
	 * @param ctx the parse tree
	 */
	void enterDiv(tex2htmlParser.DivContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#div}.
	 * @param ctx the parse tree
	 */
	void exitDiv(tex2htmlParser.DivContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#sup}.
	 * @param ctx the parse tree
	 */
	void enterSup(tex2htmlParser.SupContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#sup}.
	 * @param ctx the parse tree
	 */
	void exitSup(tex2htmlParser.SupContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#sub}.
	 * @param ctx the parse tree
	 */
	void enterSub(tex2htmlParser.SubContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#sub}.
	 * @param ctx the parse tree
	 */
	void exitSub(tex2htmlParser.SubContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#number}.
	 * @param ctx the parse tree
	 */
	void enterNumber(tex2htmlParser.NumberContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#number}.
	 * @param ctx the parse tree
	 */
	void exitNumber(tex2htmlParser.NumberContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#word}.
	 * @param ctx the parse tree
	 */
	void enterWord(tex2htmlParser.WordContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#word}.
	 * @param ctx the parse tree
	 */
	void exitWord(tex2htmlParser.WordContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#sqrt}.
	 * @param ctx the parse tree
	 */
	void enterSqrt(tex2htmlParser.SqrtContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#sqrt}.
	 * @param ctx the parse tree
	 */
	void exitSqrt(tex2htmlParser.SqrtContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#sqrt_base}.
	 * @param ctx the parse tree
	 */
	void enterSqrt_base(tex2htmlParser.Sqrt_baseContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#sqrt_base}.
	 * @param ctx the parse tree
	 */
	void exitSqrt_base(tex2htmlParser.Sqrt_baseContext ctx);
	/**
	 * Enter a parse tree produced by {@link tex2htmlParser#frac}.
	 * @param ctx the parse tree
	 */
	void enterFrac(tex2htmlParser.FracContext ctx);
	/**
	 * Exit a parse tree produced by {@link tex2htmlParser#frac}.
	 * @param ctx the parse tree
	 */
	void exitFrac(tex2htmlParser.FracContext ctx);
}