package expression.exceptions;

public class IllegalConstException extends ParsingException {
    public IllegalConstException(String parsed, int index) {
        super("Illegal const: '" + parsed + "' at index " + index);
    }
}