package expression;

public class Min extends BinaryOperator {
    public Min(AbstractOperator a, AbstractOperator b) {
        super(a, b);
    }

    @Override
    public Priority getPriority() {
        return Priority.LOWEST;
    }

    @Override
    protected String getType() {
        return "min";
    }

    @Override
    protected double internalCalc(double x, double y) {
        throw new UnsupportedOperationException("Min for doubles is not implemented");
    }

    @Override
    protected int internalCalc(int x, int y) {
        return (x < y ? x : y);
    }
}
