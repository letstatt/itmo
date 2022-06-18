package expression;

public class BitwiseCount extends UnaryOperator {

    public BitwiseCount(AbstractOperator first) {
        super(first);
    }

    @Override
    protected String getType() {
        return "count";
    }

    @Override
    protected double internalCalc(double x) {
        throw new IllegalArgumentException("Bitwise count is not defined for doubles");
    }

    @Override
    protected int internalCalc(int x) {
        return Integer.bitCount(x);
    }
}
