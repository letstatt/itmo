package expression;

import expression.exceptions.ParseException;
import expression.operators.*;

import java.util.Iterator;

public class Parser<T> {

    private static final Priority[] priorities;
    private final AbstractCalculator<T> calculator;
    private final OperatorsMap<T> operatorsMap;
    private final Tokenizer<T> t;

    private Iterator<String> it;
    private String currentToken;

    static {
        priorities = Priority.values();
    }

    public Parser(final AbstractCalculator<T> calculator) {
        this.operatorsMap = new OperatorsMap<>();
        this.calculator = calculator;
        this.t = new Tokenizer<>();
    }

    public AbstractExpression<T> parse(String s) throws ParseException {
        t.tokenize(s, operatorsMap);
        it = t.iterator();
        nextToken();

        return parseByPriority(0);
    }

    public AbstractCalculator<T> getCalculator() {
        return calculator;
    }

    private AbstractOperator<T> parseByPriority(int priority) throws ParseException {
        if (priorities[priority] == Priority.HIGHEST) {
            return parseHighestPriority();
        }

        AbstractOperator<T> leftOperand = parseByPriority(priority + 1);

        while (true) {
            String token = currentToken();
            boolean operatorExists = operatorsMap.binaryOperatorExists(token);

            if (!operatorExists || priorities[priority] != OperatorsMap.binaryOperatorPriority(token)) {
                return leftOperand;
            }

            nextToken();
            leftOperand = operatorsMap.createBinaryOperator(token, leftOperand, parseByPriority(priority + 1));

            if (leftOperand == null) {
                throw new ParseException("Unknown operator: " + token);
            }
        }
    }

    private AbstractOperator<T> parseHighestPriority() throws ParseException {
        String x = currentToken();
        nextToken();

        switch (x) {
            case "(":
                AbstractOperator<T> parsed = parseByPriority(0);
                nextToken(); // skip closing bracket
                return parsed;
            case "-":
                String t = currentToken();
                if (isNumeric(t)) {
                    nextToken();
                    return parseConst(t, false);
                }
                return new Negate<>(parseHighestPriority());
        }

        if (operatorsMap.unaryOperatorExists(x)) {
            return operatorsMap.createUnaryOperator(
                    x, parseHighestPriority()
            );

        } else if (isNumeric(x)) {
            return parseConst(x, true);

        } else {
            return parseVariable(x);
        }
    }

    private AbstractOperator<T> parseConst(String token, boolean positive) throws ParseException {
        try {
            return new Const<>(
                    calculator.parse((positive ? "+" : "-") + token)
            );

        } catch (NumberFormatException e) {
            throw new ParseException(
                    String.format("Unable to parse token %s as a number", token)
            );
        }
    }

    private AbstractOperator<T> parseVariable(String token) {
        return new Variable<>(token);
    }

    private boolean isNumeric(String x) {
        return Character.isDigit(x.charAt(0));
    }

    private String currentToken() {
        return currentToken;
    }

    private void nextToken() {
        currentToken = (it.hasNext() ? it.next() : "/none/");
    }
}
