import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.InputMismatchException;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.HashMap;

public class M {

    private static CustomScanner in;

    public static final class CustomScanner implements Closeable { // whole implementation
        private final Pattern pattern; // our implementation (see below)
        private BufferedReader reader;
        private StringBuilder buf; // = token
        private String token;

        private IOException lastException = null;
        private boolean noMoreElements = false;
        private int controlSymbol = 0; // \r \n
        private boolean EOL = false; // end of line

        public CustomScanner(Reader in, Pattern p) {
            reader = new BufferedReader(in);
            buf = new StringBuilder();
            token = "";

            pattern = (p != null) ? p : new Pattern() {
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

        public CustomScanner(InputStream in, Charset charset, Pattern p) {
            this(new InputStreamReader(in, charset), p);
        }

        public CustomScanner(File in) throws IOException {
            this(new FileReader(in, StandardCharsets.UTF_8));
        }

        public CustomScanner(File in, Charset charset, Pattern p) throws IOException {
            this(new FileReader(in, charset), p);
        }

        public CustomScanner(String in) {
            this(new StringReader(in));
        }

        public CustomScanner(String in, Pattern p) {
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
        in = new CustomScanner(System.in);
        int t = in.nextInt();
        in.skipEOL();

        for (int i = 0; i < t; i++) {
            solve();
        }

        in.close();
    }

    public static void solve() {
        int n = in.nextInt();
        int[] a = new int[n];
        in.skipEOL();

        for (int i = 0; i < n; ++i) {
            a[i] = in.nextInt();
        }

        in.skipEOL();
        Map<Integer, Integer> m = new HashMap<>();
        int ans = 0;

        for (int j = n - 1; j > 0; --j) {
            for (int i = 0; i < j; ++i) {
                ans += m.getOrDefault(2 * a[j] - a[i], 0);
            }

            m.compute(a[j], (k, v) -> (v == null ? 0 : v) + 1);
        }

        System.out.println(ans);
    }
}