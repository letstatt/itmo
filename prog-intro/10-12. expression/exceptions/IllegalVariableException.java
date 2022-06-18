package expression.exceptions;

public class IllegalVariableException extends ParsingException {
    public IllegalVariableException(String variable, int index) {
        super("Invalid variable: '" + variable + "' at index " + index);
    }
}
