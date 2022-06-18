package expression.exceptions;

public class MissingParenthesisException extends ParsingException {
    public MissingParenthesisException(String message) {
        super(message);
    }
}
