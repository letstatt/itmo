package expression.generic;

import expression.AbstractCalculator;
import expression.AbstractExpression;
import expression.Parser;
import expression.calculators.*;
import expression.exceptions.CalculatingException;
import expression.exceptions.ParseException;

import java.util.Arrays;
import java.util.stream.Collectors;

public class GenericTabulator implements Tabulator {

    public static void main(String[] args) {
        if (args.length < 2 || args[0] == null || args[1] == null || args[0].length() < 2) {
            usage();
            return;
        }

        String exp = Arrays.stream(args).skip(1).collect(Collectors.joining(" "));

        GenericTabulator tabulator = new GenericTabulator();
        Object[][][] table;

        try {
            table = tabulator.tabulate(args[0].substring(1), exp, -2, 2, -2, 2, -2, 2);

        } catch (Exception e) {
            e.printStackTrace();
            return;
        }

        for (int i = 0; i < 5; ++i) {
            for (int j = 0; j < 5; ++j) {
                for (int k = 0; k < 5; ++k) {
                    System.out.print(table[i][j][k]);
                    System.out.print(" ");
                }
                System.out.println();
            }
            System.out.println();
        }


    }

    public static void usage() {
        System.out.println("Usage: mode math_expression");
        System.out.println("Modes: -i, -d, -bi, -u, -l, -s");
    }

    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        return makeTable(createCalculator(mode), expression, x1, x2, y1, y2, z1, z2);
    }

    private <T> Object[][][] makeTable(final AbstractCalculator<T> calculator, final String expression,
                                       int x1, int x2, int y1, int y2, int z1, int z2) throws ParseException {
        int x = x2 - x1 + 1;
        int y = y2 - y1 + 1;
        int z = z2 - z1 + 1;
        Object[][][] table = new Object[x][y][z];

        Parser<T> parser = new Parser<>(calculator);
        AbstractExpression<T> exp = parser.parse(expression);

        for (int i = 0; i < x; ++i) {
            for (int j = 0; j < y; ++j) {
                for (int k = 0; k < z; ++k) {
                    String a = String.valueOf(x1 + i);
                    String b = String.valueOf(y1 + j);
                    String c = String.valueOf(z1 + k);

                    try {
                        table[i][j][k] = exp.evaluate(
                                calculator.parse(a),
                                calculator.parse(b),
                                calculator.parse(c),
                                calculator
                        );
                    } catch (CalculatingException e) {
                        // skip
                    }
                }
            }
        }

        return table;
    }

    private AbstractCalculator<?> createCalculator(String mode) {
        switch (mode) {
            case "i":
                return new IntegerCalculator(true);
            case "d":
                return new DoubleCalculator();
            case "bi":
                return new BigIntegerCalculator();
            case "u":
                return new IntegerCalculator(false);
            case "l":
                return new LongCalculator();
            case "s":
                return new ShortCalculator();
            default:
                throw new UnsupportedOperationException(
                        "Unknown mode \"" + mode + "\""
                );
        }
    }
}
