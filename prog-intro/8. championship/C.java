import java.util.Scanner;

public class C {

    private static final int p = 997;
    private static IntList[] g = null;

    private static int linearize(int i, int j) {
        return i * p + j;
    }

    private static int[] normalize(int linearCoord) {
        int[] res = new int[2];
        res[0] = linearCoord / p;
        res[1] = linearCoord % p;
        return res;
    }

    private static void makeEdge(int i, int j) {
        g[i].add(j);
        g[j].add(i);
    }

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int m = in.nextInt();
        int n = in.nextInt();
        int linearSize = linearize(2 * n + 2, 2 * m + 2) + 1;
        int start = -1, end = -1;

        g = new IntList[linearSize];

        for (int i = 0; i < linearSize; ++i) {
            g[i] = new IntList();
        }

        for (int i = 1; i <= n; ++i) {
            String s = in.next();

            for (int j = 1; j <= m; ++j) {
                if (s.charAt(j - 1) == 'X') {
                    int a = linearize(i, j);
                    int b = linearize(i, j + 1);
                    int c = linearize(i + 1, j);
                    int d = linearize(i + 1, j + 1);

                    makeEdge(c, b);
                    makeEdge(d, a);
                    makeEdge(c, a);
                    makeEdge(b, d);

                    if (start == -1) {
                        start = c;
                        end = a;
                    }
                }
            }
        }

        in.close();

        IntList st = new IntList();
        IntList path = new IntList();

        st.add(start);
        st.add(1); // do face stitch

        while (st.size() > 0) {
            int face = st.pop();
            int v = st.pop();

            int[] v_ij = normalize(v);
            boolean f = false;

            for (int i = 0; i < g[v].size(); ++i) {
                if (g[v].get(i) == -1) {
                    continue;
                }

                int[] i_ij = normalize(g[v].get(i));

                if (v_ij[1] != i_ij[1] && face == 0) {
                    continue;
                } else if (v_ij[1] == i_ij[1] && face == 1) {
                    continue;
                }

                st.add(v);
                st.add(face);
                st.add(g[v].get(i));
                st.add(face ^ 1);

                int x = g[v].get(i);

                for (int j = 0; j < g[x].size(); ++j) {
                    if (g[x].get(j) == v) {
                        g[x].set(j, -1);
                    }
                }

                g[v].set(i, -1);
                f = true;
                break;
            }

            if (!f) {
                path.add(v);
            }
        }

        for (int i = 0; i < path.size() - 1; ++i) {
            if (path.get(i) == start && path.get(i + 1) == end || path.get(i) == end && path.get(i + 1) == start) {
                IntList path2 = new IntList(path.size());

                for (int j = i + 1; j < path.size(); ++j) {
                    path2.add(path.get(j));
                }
                for (int j = 1; j <= i; ++j) {
                    path2.add(path.get(j));
                }
                path = path2;
                break;
            }
        }

        System.out.println(path.size() - 1);

        for (int i = 0; i < path.size(); ++i) {
            int d = path.get(i);
            System.out.printf("%d %d\n", (d % p) - 1, d / p - 1);
        }
    }

    /*
    3 2
    .XX
    ..X

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

        public void add(int i) {
            if (sz == a.length) {
                realloc();
            }

            a[sz++] = i;
        }

        public void set(int k, int v) {
            a[k] = v;
        }

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