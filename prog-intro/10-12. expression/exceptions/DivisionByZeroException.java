package expression.exceptions;

public class DivisionByZeroException extends CalculatingException {
    public DivisionByZeroException() {
        super("division by zero");
    }
}
