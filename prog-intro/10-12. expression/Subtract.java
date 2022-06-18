package expression;

public class Subtract extends BinaryOperator {

    public Subtract(AbstractOperator a, AbstractOperator b) {
        super(a, b);
    }

    @Override
    public Priority getPriority() {
        return Priority.ADDITION;
    }

    @Override
    protected String getType() {
        return "-";
    }

    @Override
    protected double internalCalc(double x, double y) {
        return x - y;
    }

    @Override
    protected int internalCalc(int x, int y) {
        return x - y;
    }

    @Override
    public boolean unwrappable() {
        return false;
    }
}
