package expression;

public interface AbstractExpression<T> {

    T evaluate(T x, T y, T z, AbstractCalculator<T> calculator);
}
