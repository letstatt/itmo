package expression;

public class Multiply extends BinaryOperator {
    public Multiply(AbstractOperator a, AbstractOperator b) {
        super(a, b);
    }

    @Override
    public Priority getPriority() {
        return Priority.MULTIPLY;
    }

    @Override
    protected String getType() {
        return "*";
    }

    @Override
    protected double internalCalc(double x, double y) {
        return x * y;
    }

    @Override
    protected int internalCalc(int x, int y) {
        return x * y;
    }
}
