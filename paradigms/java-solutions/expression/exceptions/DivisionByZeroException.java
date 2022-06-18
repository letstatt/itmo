package expression.exceptions;

public class DivisionByZeroException extends CalculatingException {
    public DivisionByZeroException() {
        super("Division by zero detected");
    }
}
