package expression;

public abstract class AbstractCalculator<T> {

    public abstract T negate(final T o);
    public abstract boolean sign(final T o); // x < 0 --> true
    public abstract T add(final T x, final T y);
    public abstract T subtract(final T x, final T y);
    public abstract T multiply(final T x, final T y);
    public abstract T div(final T x, final T y);
    public abstract T mod(final T x, final T y);
    public abstract T parse(final String x);
}
