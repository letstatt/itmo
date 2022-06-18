package expression;

import expression.exceptions.ParseException;

import java.util.ArrayList;
import java.util.Iterator;

public class Tokenizer<T> implements Iterable<String> {

    private final ArrayList<String> tokens;

    public Tokenizer() {
        tokens = new ArrayList<>();
    }

    public void tokenize(String s, final OperatorsMap<T> operatorsMap) throws ParseException {
        tokens.clear();
        int depth = 0;

        for (int i = 0; i < s.length(); ++i) {
            char c = s.charAt(i);

            if (Character.isWhitespace(c)) {
                continue;
            }

            switch (c) {
                case '(':
                    depth++;
                    tokens.add("(");
                    break;
                case ')':
                    depth--;
                    if (depth < 0) {
                        throw new ParseException("Unexpected closing bracket at index " + i);
                    }
                    tokens.add(")");
                    break;
                default:
                    int k = i + 1;
                    if (Character.isDigit(c)) {
                        while (k < s.length() && Character.isDigit(s.charAt(k))) {
                            k += 1;
                        }
                    } else if (Character.isLetter(c)) {
                        while (k < s.length() && Character.isLetter(s.charAt(k))) {
                            k += 1;
                        }
                    } else if (!operatorsMap.findOperator(String.valueOf(c))) {
                        throw new ParseException("Unknown token at index " + i);
                    }

                    String token = s.substring(i, k);

                    tokens.add(token);
                    i = k - 1;
            }
        }

        if (depth > 0) {
            throw new ParseException("Expected closing bracket at index " + s.length());
        }
    }

    @Override
    public Iterator<String> iterator() {
        return tokens.iterator();
    }
}
