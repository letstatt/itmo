import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

public class E {

    private static IntList[] g;
    private static boolean[] cities;
    private static int max_d, min_d;

    private static int dfs(int[] p, int s) {
        IntList st = new IntList();
        st.add(s).add(-1);
        int u = s;

        max_d = 0;
        min_d = Integer.MAX_VALUE;

        Arrays.fill(p, -1);

        while (st.size() > 0) {
            int j = st.pop();
            int v = st.pop();

            if (cities[v]) {
                if (st.size() / 2 > max_d) {
                    max_d = st.size() / 2;
                    u = v;
                } else if (st.size() / 2 < min_d) {
                    min_d = st.size() / 2;
                }
            }

            for (int i = j + 1; i < g[v].size(); ++i) {
                if (p[v] != g[v].get(i)) {
                    p[g[v].get(i)] = v;
                    st.add(v);
                    st.add(i);
                    st.add(g[v].get(i));
                    st.add(-1);
                    break;
                }
            }
        }

        return u;
    }

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int m = in.nextInt();
        g = new IntList[n];
        cities = new boolean[n];

        for (int i = 0; i < n; ++i) {
            g[i] = new IntList();
        }

        for (int i = 1, x, y; i < n; ++i) {
            x = in.nextInt();
            y = in.nextInt();
            x -= 1;
            y -= 1;

            g[x].add(y);
            g[y].add(x);
        }

        int root = n;

        for (int i = 0, j; i < m; ++i) {
            j = in.nextInt() - 1;
            cities[j] = true;
            root = Math.min(root, j);
        }

        in.close();

        int[] p = new int[n];
        root = dfs(p, root);

        if (max_d % 2 == 1) {
            System.out.println("NO");
            System.exit(0);
        }

        //System.out.println(max_d);

        int d = max_d / 2;

        for (int i = 0; i < d; ++i) {
            root = p[root];
        }

        dfs(p, root);

        //System.out.println(max_d + " " + min_d);
        System.out.println(d != max_d || min_d != max_d ? "NO" : "YES\n" + (root + 1));
    }

    /*
    6 3
    1 2
    2 3
    3 4
    4 5
    4 6
    1 5 6

    3 3
    1 2
    2 3
    1 2 3

    1 1 1

     */

    public static class IntList {

        private final int initialCapacity = 10;
        private int sz = 0;
        private int[] a;

        IntList() {
            a = new int[initialCapacity];
        }

        IntList(int capacity) {
            a = new int[Math.max(capacity, initialCapacity)];
        }

        public int get(int i) {
            return a[i];
        }

        public IntList add(int i) {
            if (sz == a.length) {
                realloc();
            }

            a[sz++] = i;
            return this;
        }

        /* void set(int i, int j) {
            a[i] = j;
        }*/

        public int pop() {
            return a[--sz];
        }

        private void realloc() {
            int[] b = new int[a.length + (a.length >> 1)];
            System.arraycopy(a, 0, b, 0, sz);
            a = b;
        }

        public int size() {
            return sz;
        }
    }
}