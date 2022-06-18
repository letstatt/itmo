package expression.exceptions;

import expression.*;

import java.util.ArrayList;
import java.util.List;

public class ExpressionParser implements Parser {

    private static final Priority[] priorities;
    private ArrayList<String> tokens;
    private int it = 0;
    private int index = 0;

    static {
        ArrayList<Priority> tmp = new ArrayList<>(List.of(Priority.values()));
        tmp.remove(Priority.BITWISE_AND);
        tmp.remove(Priority.BITWISE_XOR);
        tmp.remove(Priority.BITWISE_AND);
        priorities = new Priority[tmp.size()];
        for (int i = 0; i < tmp.size(); ++i) {
            priorities[i] = tmp.get(i);
        }
    }

    @Override
    public TripleExpression parse(String expression) throws ParsingException {
        tokenize(expression);
        TripleExpression exp = parseByPriority(0);

        if (it != tokens.size()) {
            throw new ParsingException("Unexpected token at index " + index);
        }

        return exp;
    }

    private AbstractOperator parseByPriority(int priority) throws ParsingException {
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

    private AbstractOperator merge(AbstractOperator left, AbstractOperator right, BinaryOperatorsMap op) throws ParsingException {
        switch (op) {
            case ADD:
                return new CheckedAdd(left, right);
            case SUBTRACT:
                return new CheckedSubtract(left, right);
            case MULTIPLY:
                return new CheckedMultiply(left, right);
            case DIVIDE:
                return new CheckedDivide(left, right);
            case MIN:
                return new Min(left, right);
            case MAX:
                return new Max(left, right);
            default:
                throw new InvalidOperatorException(op.toString());
        }
    }

    private AbstractOperator parseHighestPriority() throws ParsingException {
        String x = currentToken();
        nextToken();

        switch (x) {
            case "(":
                AbstractOperator parsed = parseByPriority(0);
                if (!currentToken().equals(")")) {
                    throw new MissingParenthesisException("Expected closing bracket at index " + (index - 1));
                }
                nextToken(); // skip closing bracket
                return parsed;
            case ")":
                throw new MissingParenthesisException("Unexpected closing bracket at index " + (index - 1));
            case "~":
                return new BitwiseNegation(parseHighestPriority());
            case "count":
                return new BitwiseCount(parseHighestPriority());
            case "abs":
                return new CheckedAbs(parseHighestPriority());
            case "sqrt":
                return new CheckedSqrt(parseHighestPriority());
            case "-":
                String t = currentToken();
                if (isNumeric(t)) {
                    nextToken();
                    return parseConst(t, false);
                }
                return new CheckedNegate(parseHighestPriority());
        }

        if (isNumeric(x)) {
            return parseConst(x, true);

        } else {
            return parseVariable(x);
        }
    }

    private AbstractOperator parseConst(String token, boolean positive) throws ParsingException {
        String num = (positive ? "+" : "-") + token;
        try {
            return new Const(Integer.parseInt(num));
        } catch (NumberFormatException e) {
            throw new IllegalConstException(num, index - token.length());
        }
    }

    private AbstractOperator parseVariable(String token) throws ParsingException {
        switch (token) {
            case "x":
            case "y":
            case "z":
                return new Variable(token);
            default:
                throw new IllegalVariableException(token, index - token.length());
        }
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
        index += tokens.get(it).length();
        it++;
    }

    private void tokenize(String exp) throws ParsingException {
        tokens = new ArrayList<>();
        int i = 0, bal = 0;
        it = 0;

        boolean lastTokenIsConst = false;

        for (; i < exp.length(); ++i) {
            char x = exp.charAt(i);
            String tmp = String.valueOf(x);

            if (Character.isWhitespace(x)) {
                continue;
            }

            if (BinaryOperatorsMap.hints.containsKey(tmp)) {
                lastTokenIsConst = false;
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
                        throw new MissingParenthesisException("Unexpected closing bracket at index " + i);
                    }
                    lastTokenIsConst = false;
                    bal -= 1;
                    break;
                default:
                    int k = i + 1;
                    if (Character.isDigit(x)) {
                        if (lastTokenIsConst) {
                            throw new ParsingException("Unexpected const at index " + i);
                        }
                        while (k < exp.length() && Character.isDigit(exp.charAt(k))) {
                            k += 1;
                        }
                        lastTokenIsConst = true;
                    } else if (Character.isLetter(x)) {
                        while (k < exp.length() && (Character.isLetter(exp.charAt(k)) || Character.isDigit(exp.charAt(k)))) {
                            k += 1;
                        }
                        lastTokenIsConst = false;
                    } else {
                        throw new ParsingException("Unknown token at index " + i);
                    }

                    tokens.add(exp.substring(i, k));
                    i = k - 1;
            }
        }

        if (bal > 0) {
            throw new MissingParenthesisException("Expected closing bracket at index " + exp.length());
        }
    }
}
