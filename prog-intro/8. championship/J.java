import java.util.Scanner;

public class J {

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();

        int[][] g = new int[n][n];
        int[][] t = new int[n][n];

        for (int i = 0; i < n; ++i) {
            String s = in.next();
            for (int j = 0; j < n; ++j) {
                g[i][j] = Integer.parseInt(String.valueOf(s.charAt(j)));
            }
        }

        in.close();

        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                if (i == j || g[i][j] != 1) continue;
                t[i][j] = 1;

                for (int k = j + 1; k < n; ++k) {
                    g[i][k] = (10 + (g[i][k] - g[j][k]) % 10) % 10;
                }
            }
        }

        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                System.out.print(t[i][j]);
            }
            System.out.println();
        }
    }
}
