package expression;

import expression.operators.*;

import java.util.Map;

public class OperatorsMap<T> {

    public static final Map<String, Priority> priors = Map.of(
            "+", Priority.ADDITION,
            "-", Priority.ADDITION,
            "*", Priority.MULTIPLY,
            "/", Priority.MULTIPLY,
            "mod", Priority.MULTIPLY
    );

    public AbstractOperator<T> createBinaryOperator(final String token, final AbstractOperator<T> x, final AbstractOperator<T> y) {
        switch (token) {
            case "+":
                return new Add<>(x, y);
            case "-":
                return new Subtract<>(x, y);
            case "*":
                return new Multiply<>(x, y);
            case "/":
                return new Divide<>(x, y);
            case "mod":
                return new Mod<>(x, y);
            default:
                return null;
        }
    }

    public AbstractOperator<T> createUnaryOperator(final String token, final AbstractOperator<T> x) {
        switch (token) {
            case "-":
                return new Negate<>(x);
            case "abs":
                return new Abs<>(x);
            case "square":
                return new Square<>(x);
            default:
                return null;
        }
    }

    public boolean findOperator(final String op) {
        return (unaryOperatorExists(op) || binaryOperatorExists(op));
    }

    public boolean unaryOperatorExists(final String op) {
        return (createUnaryOperator(op, null) != null);
    }

    public boolean binaryOperatorExists(final String op) {
        return (createBinaryOperator(op, null, null) != null);
    }

    public static Priority binaryOperatorPriority(final String op) {
        return priors.get(op);
    }
}
