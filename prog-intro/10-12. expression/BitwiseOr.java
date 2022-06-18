package expression;

public class BitwiseOr extends BinaryOperator {
    public BitwiseOr(AbstractOperator a, AbstractOperator b) {
        super(a, b);
    }

    @Override
    public Priority getPriority() {
        return Priority.BITWISE_OR;
    }

    @Override
    protected String getType() {
        return "|";
    }

    @Override
    protected double internalCalc(double x, double y) {
        throw new IllegalArgumentException("Bitwise or is not defined for doubles");
    }

    @Override
    protected int internalCalc(int x, int y) {
        return x | y;
    }
}
