package expression.exceptions;

public class InvalidOperatorException extends ParsingException {
    public InvalidOperatorException(String parsed) {
        super("Invalid operator: '" + parsed + "'");
    }
}
