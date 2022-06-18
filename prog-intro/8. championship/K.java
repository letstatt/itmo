import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;
import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public class K {

    public static final class CustomScanner implements Closeable { // whole implementation
        private final Pattern pattern; // our implementation (see below)
        private BufferedReader reader;
        private StringBuilder buf; // = token
        private String token;

        private IOException lastException = null;
        private boolean noMoreElements = false;
        private int controlSymbol = 0; // \r \n
        private boolean EOL = false; // end of line

        public CustomScanner(Reader in, CustomScanner.Pattern p) {
            reader = new BufferedReader(in);
            buf = new StringBuilder();
            token = "";

            pattern = (p != null) ? p : new CustomScanner.Pattern() {
                public boolean isDelimiter(int c) {
                    int t = Character.getType(c);
                    return (t == Character.SPACE_SEPARATOR || t == Character.CONTROL);
                }
            };
        }

        public CustomScanner(Reader in) {
            this(in, null);
        }

        public CustomScanner(InputStream in) {
            this(new InputStreamReader(in, StandardCharsets.UTF_8));
        }

        public CustomScanner(InputStream in, Charset charset, CustomScanner.Pattern p) {
            this(new InputStreamReader(in, charset), p);
        }

        public CustomScanner(File in) throws IOException {
            this(new FileReader(in, StandardCharsets.UTF_8));
        }

        public CustomScanner(File in, Charset charset, CustomScanner.Pattern p) throws IOException {
            this(new FileReader(in, charset), p);
        }

        public CustomScanner(String in) {
            this(new StringReader(in));
        }

        public CustomScanner(String in, CustomScanner.Pattern p) {
            this(new StringReader(in), p);
        }

        public void close() {
            if (reader == null) {
                return;
            }

            if (reader instanceof Closeable) {
                try {
                    reader.close();

                } catch (IOException e) {
                    lastException = e;
                }
            }

            noMoreElements = true;
            EOL = true;

            reader = null;
            token = null;
            buf = null;
        }

        public IOException ioException() {
            return lastException;
        }

        private boolean complementToken() { // tokenizer
            if (buf.length() > 0) {
                return true;
            }

            if (reader == null) {
                throw new IllegalStateException();
            }

            if (noMoreElements || EOL) {
                throw new NoSuchElementException();
            }

            try {
                while (true) {
                    int c = reader.read();

                    if (c == -1) {
                        noMoreElements = true;
                        EOL = true;
                        break;

                    } else if (!pattern.isDelimiter(c)) {
                        buf.append((char) c);
                        controlSymbol = 0;

                    } else if (c == '\r' || c == '\n') {
                        if (controlSymbol == '\r' && c == '\n') {
                            controlSymbol = c;
                            continue; // "\r\n" sequence, drop it
                        }

                        controlSymbol = c;
                        EOL = true;
                        break;

                    } else if (buf.length() > 0) {
                        break;
                    }
                }

            } catch (IOException e) { // handle as c == -1
                lastException = e;
                noMoreElements = true;
                EOL = true;
            }

            token = buf.toString(); // store buf (word in general) as string to improve speed of hasNext*
            return (buf.length() > 0);
        }

        private String flushToken() {
            buf.setLength(0);
            return token;
        }

        public boolean hasNextLine() { // can it be read?
            return buf.length() > 0 || !noMoreElements && !EOL;
        }

        public void skipEOL() { // allow to go to the next line
            EOL = false;
        }

        public boolean hasNext() {
            if (!hasNextLine()) {
                return false;
            }

            return complementToken();
        }

        public boolean hasNextInt(int radix, boolean unsigned) {
            if (!hasNext()) {
                return false;
            }

            try {
                parseInt(token, radix, unsigned);
                return true;

            } catch (InputMismatchException e) {
                return false;
            }
        }

        public boolean hasNextInt() {
            return hasNextInt(10, false);
        }

        public boolean hasNextIntAuto() {
            if (!hasNext()) {
                return false;
            }

            boolean h = isHexadecimal(token);
            return hasNextInt(h ? 16 : 10, h);
        }

        public String next() {
            complementToken();
            return flushToken();
        }

        public int nextInt(int radix, boolean unsigned) {
            return parseInt(next(), radix, unsigned);
        }

        public int nextInt() {
            return nextInt(10, false);
        }

        public int nextIntAuto() {
            String s = next();
            boolean h = isHexadecimal(s);
            return parseInt(s, h ? 16 : 10, h); // hex as two's complement
        }

        private int parseInt(String s, int radix, boolean unsigned) {
            String t = decimalTo(s, radix);

            try {
                return unsigned ?
                        Integer.parseUnsignedInt(t, radix) :
                        Integer.parseInt(t, radix);

            } catch (NumberFormatException e) {
                throw new InputMismatchException();
            }
        }

        private String decimalTo(String s, int radix) {
            switch(radix) {
                case 10:
                    return s;
                case 16:
                    return truncateHex(s);
                default:
                    throw new IllegalArgumentException();
            }
        }

        private boolean isHexadecimal(String s) {
            return (s.length() > 2 && s.charAt(0) == '0' && Character.toLowerCase(s.charAt(1)) == 'x');
        }

        // String "s" must match (0(X|x)[0-9a-fA-F]+)
        private String truncateHex(String s) {
            return s.substring(2);
        }

        public interface Pattern {
            boolean isDelimiter(int c);
        }
    }

    public static void main(String[] args) {
        CustomScanner in = new CustomScanner(System.in);

        int n = in.nextInt();
        in.skipEOL();
        int m = in.nextInt();
        in.skipEOL();

        char[][] a = new char[n][];
        int x0 = -1, y0 = -1;

        for (int i = 0; i < n; ++i) {
            a[i] = in.next().toCharArray(); // a[i].length is exactly equal m
            for (int j = 0; j < m; ++j) {
                if (a[i][j] == 'A') {
                    y0 = i;
                    x0 = j;
                }
            }
            in.skipEOL();
        }

        in.close();

        a[y0][x0] = '.';
        int x1 = -1, x2 = -1, y1 = -1, y2 = -1, s = 0;

        IntList d = new IntList(m, -1);
        IntList d1 = new IntList(m);
        IntList d2 = new IntList(m);
        IntList st = new IntList(m);

        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (a[i][j] != '.') {
                    d.set(j, i);
                }
            }

            st.trickyClear();

            for (int j = 0; j < m; ++j) {
                while (st.size() > 0 && d.get(st.back()) <= d.get(j)) st.pop();
                d1.set(j, st.size() == 0 ? -1 : st.back());
                st.add(j);
            }

            st.trickyClear();

            for (int j = m - 1; j >= 0; --j) {
                while (st.size() > 0 && d.get(st.back()) <= d.get(j)) st.pop();
                d2.set(j, st.size() == 0 ? m : st.back());
                st.add(j);
            }

            if (i < y0) {
                continue;
            }

            for (int j = 0; j < m; ++j) {
                int j1 = d1.get(j) + 1;
                int j2 = d2.get(j) - 1;
                int i1 = d.get(j) + 1;
                int i2 = i;

                int area = (i - d.get(j)) * (d2.get(j) - d1.get(j) - 1);
                boolean inner = (j1 <= x0 && x0 <= j2 && i1 <= y0 && y0 <= i2);

                //System.out.println(j1 + " " + j2 + " " + i1 + " " + i2);
                //System.out.println(area);

                if (area > s && inner) {
                    s = area;
                    x1 = j1;
                    x2 = j2;
                    y1 = i1;
                    y2 = i2;
                    //System.out.println(x1 + " " +  x2 + " " + y1 + " " + y2 + " " + area);
                }
            }
        }

        for (int i = y1; i <= y2; ++i) {
            for (int j = x1; j <= x2; ++j) {
                a[i][j] = 'a';
            }
        }

        a[y0][x0] = 'A';

        repair(0, x1, y1, x2 + 1, a); // up
        repair(y2 + 1, x1, n, x2 + 1, a); // down

        repair(0, 0, n, x1, a); // left
        repair(0, x2 + 1, n, m, a); // right

        PrintWriter out = new PrintWriter(System.out);

        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                out.print(a[i][j]);
            }
            out.println();
        }

        out.close();
    }

    private static void repair(int a1, int b1, int a2, int b2, char[][] a) {
        for (int i = a1; i < a2; ++i) {
            for (int j = b1; j < b2; ++j) {
                if (Character.isUpperCase(a[i][j])) {
                    int i1 = i, i2 = i, j1 = j, j2 = j;
                    char c = Character.toLowerCase(a[i][j]);
                    boolean f = false;

                    while (i1 > a1 && a[i1 - 1][j] == '.') --i1;
                    while (i2 < a2 - 1 && a[i2 + 1][j] == '.') ++i2;

                    while (j1 > b1) {
                        for (int k = i1; k <= i2; ++k) {
                            if (a[k][j1 - 1] != '.') {
                                f = true;
                                break;
                            }
                        }

                        if (f) break;
                        --j1;
                    }

                    f = false;

                    while (j2 < b2 - 1) {
                        for (int k = i1; k <= i2; ++k) {
                            if (a[k][j2 + 1] != '.') {
                                f = true;
                                break;
                            }
                        }

                        if (f) break;
                        ++j2;
                    }

                    for (int p = i1; p <= i2; ++p) {
                        for (int q = j1; q <= j2; ++q) {
                            if (a[p][q] == '.') {
                                a[p][q] = c;
                            }
                        }
                    }
                }
            }
        }
    }

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

        IntList(int capacity, int defaultValue) {
            a = new int[Math.max(capacity, initialCapacity)];
            Arrays.fill(a, defaultValue);
        }

        public int get(int i) {
            return a[i];
        }

        public int back() {
            return a[sz - 1];
        }

        public IntList add(int i) {
            if (sz == a.length) {
                realloc();
            }

            a[sz++] = i;
            return this;
        }

        public void set(int k, int v) {
            a[k] = v;
        }

        public int pop() {
            return a[--sz];
        }

        public void trickyClear() {
            sz = 0;
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