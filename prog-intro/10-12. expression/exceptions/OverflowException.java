package expression.exceptions;

public class OverflowException extends CalculatingException {
    public OverflowException() {
        super("overflow");
    }
}