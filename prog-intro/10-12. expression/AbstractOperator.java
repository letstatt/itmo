package expression;

public abstract class AbstractOperator implements Comparable<AbstractOperator>, TripleExpression, DoubleExpression, Expression {

    public abstract Priority getPriority();

    protected String getType() {
        return "$"; // why not?
    }

    @Override
    public int evaluate(int x) {
        return (int) evaluate((double) x);
    }

    protected boolean unwrappable() {
        return true;
    }

    @Override
    public int compareTo(AbstractOperator o) {
        return this.getPriority().compareTo(o.getPriority());
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AbstractOperator) {
            return obj.hashCode() == hashCode();
        } else {
            return false;
        }
    }
}
