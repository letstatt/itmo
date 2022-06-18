package expression;

public class BitwiseNegation extends UnaryOperator {

    public BitwiseNegation(AbstractOperator first) {
        super(first);
    }

    @Override
    protected String getType() {
        return "~";
    }

    @Override
    protected double internalCalc(double x) {
        throw new IllegalArgumentException("Bitwise and is not defined for doubles");
    }

    @Override
    protected int internalCalc(int x) {
        return ~x;
    }
}