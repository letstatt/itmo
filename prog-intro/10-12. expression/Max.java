package expression;

public class Max extends BinaryOperator {
    public Max(AbstractOperator a, AbstractOperator b) {
        super(a, b);
    }

    @Override
    public Priority getPriority() {
        return Priority.LOWEST;
    }

    @Override
    protected String getType() {
        return "max";
    }

    @Override
    protected double internalCalc(double x, double y) {
        throw new UnsupportedOperationException("Max for doubles is not implemented");
    }

    @Override
    protected int internalCalc(int x, int y) {
        return (x > y ? x : y);
    }
}
