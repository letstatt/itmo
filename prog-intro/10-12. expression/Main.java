package expression;

import expression.exceptions.ExpressionParser;

public class Main {

    public static void main(String[] args) {
        //System.out.println(new Multiply(new Divide(new Const(10), new Variable("x")), new Const(5)).toMiniString());

        //check("x*y+(z-1   )+/10");
        check("(-564739020) Z+ (z)");
    }

    private static void check(String arg) {
        ExpressionParser parser = new ExpressionParser();
        System.out.println("reference: " + arg);

        try {
            System.out.println("answer:" + parser.parse(arg));
        } catch (Exception e) {
            System.out.println("alright");
            e.printStackTrace();
        }
    }
}
