package expression;

public class UnaryMinus extends UnaryOperator {

    public UnaryMinus(AbstractOperator first) {
        super(first);
    }

    @Override
    protected String getType() {
        return "-";
    }

    @Override
    protected double internalCalc(double x) {
        return -x;
    }

    @Override
    protected int internalCalc(int x) {
        return -x;
    }
}