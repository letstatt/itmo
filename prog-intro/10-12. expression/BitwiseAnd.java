package expression;

public class BitwiseAnd extends BinaryOperator {
    public BitwiseAnd(AbstractOperator a, AbstractOperator b) {
        super(a, b);
    }

    @Override
    public Priority getPriority() {
        return Priority.BITWISE_AND;
    }

    @Override
    protected String getType() {
        return "&";
    }

    @Override
    protected double internalCalc(double x, double y) {
        throw new IllegalArgumentException("Bitwise and is not defined for doubles");
    }

    @Override
    protected int internalCalc(int x, int y) {
        return x & y;
    }
}
