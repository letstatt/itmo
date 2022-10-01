import org.jetbrains.annotations.NotNull;

/**
 * В теле класса решения разрешено использовать только финальные переменные типа RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 */
public class Solution implements MonotonicClock {
    private final RegularInt c11 = new RegularInt(0);
    private final RegularInt c12 = new RegularInt(0);
    private final RegularInt c13 = new RegularInt(0);

    private final RegularInt c21 = new RegularInt(0);
    private final RegularInt c22 = new RegularInt(0);
    private final RegularInt c23 = new RegularInt(0);

    @Override
    public void write(@NotNull Time time) {
        // write left-to-right
        c21.setValue(time.getD1());
        c22.setValue(time.getD2());
        c23.setValue(time.getD3());
        // write right-to-left
        c13.setValue(c23.getValue());
        c12.setValue(c22.getValue());
        c11.setValue(c21.getValue());
    }

    @NotNull
    @Override
    public Time read() {
        int v1, v2, v3;
        // read left-to-right
        v1 = c11.getValue();
        v2 = c12.getValue();
        v3 = c13.getValue();
        final Time r1 = new Time(v1, v2, v3);
        // read right-to-left
        v3 = c23.getValue();
        v2 = c22.getValue();
        v1 = c21.getValue();
        final Time r2 = new Time(v1, v2, v3);

        if (r1.equals(r2)) {
            return r1;
        } else {
            if (r1.getD1() != r2.getD1()) {
                return new Time(Math.max(r1.getD1(), r2.getD1()), 0, 0);
            } else if (r1.getD2() != r2.getD2()) {
                return new Time(r1.getD1(), Math.max(r1.getD2(), r2.getD2()), 0);
            } else {
                return new Time(r1.getD1(), r1.getD2(), Math.max(r1.getD3(), r2.getD3()));
            }
        }
    }
}
