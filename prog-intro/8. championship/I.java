import java.util.Scanner;

public class I {

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();

        int x_l = Integer.MAX_VALUE;
        int x_r = Integer.MIN_VALUE;
        int y_l = Integer.MAX_VALUE;
        int y_r = Integer.MIN_VALUE;

        for (int i = 0; i < n; ++i) {
            int x, y, h;
            x = in.nextInt();
            y = in.nextInt();
            h = in.nextInt();

            x_l = Math.min(x_l, x - h);
            x_r = Math.max(x_r, x + h);
            y_l = Math.min(y_l, y - h);
            y_r = Math.max(y_r, y + h);
        }

        in.close();

        int h = (int) Math.ceil((double) Math.max(x_r - x_l, y_r - y_l) / 2);
        int x = (x_l + x_r) / 2;
        int y = (y_l + y_r) / 2;

        System.out.printf("%d %d %d", x, y, h);
    }
}