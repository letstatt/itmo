import java.util.Scanner;

public class A {

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int a, b, n;

        a = in.nextInt();
        b = in.nextInt();
        n = in.nextInt();

        System.out.println((int) (2 * Math.ceil((n - b) / (float) (b - a)) + 1));
        in.close();
    }
}