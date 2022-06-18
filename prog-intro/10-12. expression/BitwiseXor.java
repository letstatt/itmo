package expression;

public class BitwiseXor extends BinaryOperator {
    public BitwiseXor(AbstractOperator a, AbstractOperator b) {
        super(a, b);
    }

    @Override
    public Priority getPriority() {
        return Priority.BITWISE_XOR;
    }

    @Override
    protected String getType() {
        return "^";
    }

    @Override
    protected double internalCalc(double x, double y) {
        throw new IllegalArgumentException("Bitwise xor is not defined for doubles");
    }

    @Override
    protected int internalCalc(int x, int y) {
        return x ^ y;
    }
}
