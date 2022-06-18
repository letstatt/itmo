package expression.exceptions;

public class SqrtException extends CalculatingException {
    public SqrtException() {
        super("Square root only accepts non-negative values");
    }
}
