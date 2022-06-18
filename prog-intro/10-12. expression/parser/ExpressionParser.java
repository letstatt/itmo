package expression.parser;

import expression.*;

import java.util.ArrayList;

public class ExpressionParser implements Parser {

    private static final Priority[] priorities;
    private ArrayList<String> tokens;
    private int it = 0;

    static {
        priorities = Priority.values();
    }

    @Override
    public TripleExpression parse(String expression) {
        tokenize(expression);
        return parseByPriority(0);
    }

    private AbstractOperator parseByPriority(int priority) {
        if (priorities[priority] == Priority.HIGHEST) {
            return parseHighestPriority();
        }

        AbstractOperator leftOperand = parseByPriority(priority + 1);

        while (true) {
            BinaryOperatorsMap operator = BinaryOperatorsMap.hints.getOrDefault(currentToken(), null);

            if (operator == null || priorities[priority] != BinaryOperatorsMap.priors.get(operator)) {
                return leftOperand;
            }

            nextToken();
            leftOperand = merge(leftOperand, parseByPriority(priority + 1), operator);
        }
    }

    private AbstractOperator merge(AbstractOperator left, AbstractOperator right, BinaryOperatorsMap op) {
        switch (op) {
            case ADD:
                return new Add(left, right);
            case SUBTRACT:
                return new Subtract(left, right);
            case MULTIPLY:
                return new Multiply(left, right);
            case DIVIDE:
                return new Divide(left, right);
            case BITWISE_AND:
                return new BitwiseAnd(left, right);
            case BITWISE_XOR:
                return new BitwiseXor(left, right);
            case BITWISE_OR:
                return new BitwiseOr(left, right);
            default:
                throw new IllegalArgumentException("Unknown operator: " + op);
        }
    }

    private AbstractOperator parseHighestPriority() {
        String x = currentToken();
        nextToken();

        switch (x) {
            case "(":
                AbstractOperator parsed = parseByPriority(0);
                nextToken(); // skip closing bracket
                return parsed;
            case "~":
                return new BitwiseNegation(parseHighestPriority());
            case "count":
                return new BitwiseCount(parseHighestPriority());
            case "-":
                String t = currentToken();
                if (isNumeric(t)) {
                    nextToken();
                    return parseConst(t, false);
                }
                return new UnaryMinus(parseHighestPriority());
        }

        if (isNumeric(x)) {
            return parseConst(x, true);

        } else {
            return parseVariable(x);
        }
    }

    private AbstractOperator parseConst(String token, boolean positive) {
        try {
            return new Const(Integer.parseInt((positive ? "+" : "-") + token));

        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(
                    String.format("Unable to parse token %s as a number", token)
            );
        }
    }

    private AbstractOperator parseVariable(String token) {
        return new Variable(token);
    }

    private boolean isNumeric(String x) {
        return Character.isDigit(x.charAt(0));
    }

    private String currentToken() {
        if (it > tokens.size() - 1) {
            return "/none/";
        }
        return tokens.get(it);
    }

    private void nextToken() {
        it++;
    }

    private void tokenize(String exp) {
        tokens = new ArrayList<>();
        int i = 0, bal = 0;
        it = 0;

        for (; i < exp.length(); ++i) {
            char x = exp.charAt(i);
            String tmp = String.valueOf(x);

            if (Character.isWhitespace(x)) {
                continue;
            }

            if (BinaryOperatorsMap.hints.containsKey(tmp)) {
                tokens.add(tmp);
                continue;
            }

            switch (x) {
                case '~':
                    tokens.add("~");
                    break;
                case '(':
                    tokens.add("(");
                    bal += 1;
                    break;
                case ')':
                    tokens.add(")");
                    if (bal == 0) {
                        throw new IllegalArgumentException("Unexpected closing bracket at index " + i);
                    }
                    bal -= 1;
                    break;
                default:
                    int k = i + 1;
                    if (Character.isDigit(x)) {
                        while (k < exp.length() && Character.isDigit(exp.charAt(k))) {
                            k += 1;
                        }
                    } else if (Character.isLetter(x)) {
                        while (k < exp.length() && Character.isLetter(exp.charAt(k))) {
                            k += 1;
                        }
                    } else {
                        throw new IllegalArgumentException("Unknown token at index " + i);
                    }

                    tokens.add(exp.substring(i, k));
                    i = k - 1;
            }
        }

        if (bal > 0) {
            throw new IllegalArgumentException("Expected closing bracket at index " + exp.length());
        }
    }
}
