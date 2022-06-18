package expression;

public class Divide extends BinaryOperator {
    public Divide(AbstractOperator a, AbstractOperator b) {
        super(a, b);
    }

    @Override
    public Priority getPriority() {
        return Priority.MULTIPLY;
    }

    @Override
    protected String getType() {
        return "/";
    }

    @Override
    protected double internalCalc(double x, double y) {
        return x / y;
    }

    @Override
    protected int internalCalc(int x, int y) {
        return x / y; // to avoid wrong casting from +inf, -inf and nan to int
    }

    @Override
    public boolean unwrappable() {
        return false;
    }
}
