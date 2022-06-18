package expression;

public class Const extends AbstractOperator {
    private final double x;

    public Const(final double x) {
        this.x = x;
    }

    public Const(final int x) {
        this.x = x;
    }

    @Override
    public Priority getPriority() {
        return Priority.HIGHEST;
    }

    @Override
    public double evaluate(double x) {
        return this.x;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return (int) this.x;
    }

    @Override
    public String toString() {
        return ((int) x == x ? String.valueOf((int) x) : String.valueOf(x));
    }

    @Override
    public String toMiniString() {
        return toString();
    }

    @Override
    public int hashCode() {
        return Double.hashCode(x);
    }
}
