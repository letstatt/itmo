import java.util.Scanner;

public class B {

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int t = -710 * 25000;
        int n = in.nextInt();
        in.close();

        for (int i = 0; i < n; ++i) {
            System.out.println(t);
            t += 710;
        }
    }
}