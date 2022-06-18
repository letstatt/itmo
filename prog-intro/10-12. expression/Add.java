package expression;

public class Add extends BinaryOperator {

    public Add(AbstractOperator a, AbstractOperator b) {
        super(a, b);
    }

    @Override
    public Priority getPriority() {
        return Priority.ADDITION;
    }

    @Override
    protected String getType() {
        return "+";
    }

    @Override
    protected double internalCalc(double x, double y) {
        return x + y;
    }

    @Override
    protected int internalCalc(int x, int y) {
        return x + y;
    }
}
