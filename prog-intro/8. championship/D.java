import java.util.Scanner;

public class D {

    private final static long mod = 998_244_353;

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int n = in.nextInt();
        long k = in.nextInt();
        in.close();

        long[] pow_k = new long[n + 1];
        long[] R = new long[n + 1];
        long[] D = new long[n + 1];

        pow_k[0] = 1;

        for (int i = 1; i < pow_k.length; ++i) {
            pow_k[i] = (pow_k[i - 1] * k) % mod;
        }

        for (int i = 1; i <= n; ++i) {
            R[i] = ((i & 1) == 1 ?
                    i * pow_k[i / 2 + 1] :
                    i / 2 * ((pow_k[i / 2] + pow_k[i / 2 + 1]))
            ) % mod;
        }

        D[1] = R[1];

        for (int i = 2; i <= n; ++i) {
            D[i] = R[i];

            D[i] -= i * D[1]; // j = 1
            D[i] = (D[i] + mod) % mod; // neg -> pos

            for (int j = 2; j * j <= i; ++j) {
                if (i % j == 0) {
                    D[i] -= j * D[i / j];

                    if (j * j != i) {
                        D[i] -= i / j * D[j];
                    }

                    D[i] = (D[i] + 2 * mod) % mod;
                }
            }
        }

        long ans = 0;

        for (int i = 1; i <= n; ++i) {
            for (int j = 1; j * j <= i; ++j) {
                if (i % j == 0) {
                    ans = (ans + D[j]) % mod;

                    if (j * j != i) {
                        ans = (ans + D[i / j]) % mod;
                    }
                }
            }
        }

        ans += mod * mod;
        ans %= mod;

        System.out.println(ans);

        /*
        R(n, k) = ∑ l=0,n−1 k^ceil(l/2)·k^ceil((n−l)/2)
        R(1, k) = k^0 * k^1 ==> k
        R(2, k) = k^0 * k^1 + k^1 * k^1 ==> k + k^2
        R(3, k) = k^0 * k^2 + k^1 * k^1 + k^1 * k^1 ==> 3 * k^2
        R(4, k) = k^0 * k^2 + k^1 * k^2 + k^1 * k^1 + k^2 * k^1 ==> 2 * k^2 + 2 * k^3
        R(5, k) = k^0 * k^3 + k^1 * k^2 + k^1 * k^2 + k^2 * k^1 + k^2 * k^1 ==> 5 * k^3
        R(6, k) = k^0 * k^3 + k^1 * k^3 + k^1 * k^2 + k^2 * k^2 + k^2 * k^1 + k^3 * k^1 = 3 * k^3 + 3 * k^4

        R(n & 1 == 1, k) = i * k^(i/2+1)
        R(n & 1 == 0, k) = i/2 * k^(i/2) + i/2 * k^(i/2 + 1)
        */
    }
}