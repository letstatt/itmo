package md2html;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Map;
import java.util.function.Function;

public class Md2Html {

    private static final Map<String, String> TAGS = Map.of(
            "*", "em",
            "_", "em",
            "**", "strong",
            "__", "strong",
            "`", "code",
            "--", "s",
            "~", "mark"
    );

    private static final Map<Character, String> ESCAPE_SEQ = Map.of(
            '<', "&lt;",
            '>', "&gt;",
            '&', "&amp;"
    );

    private static final int INITIAL_CAPACITY = 64;

    private static Deque<String> openedTags;
    private static Deque<StringBuilder> contexts;
    private static StringBuilder buf;
    private static String openedTag;
    private static String src;
    private static int i;

    private static String convert(String s) {
        openedTags = new ArrayDeque<>(INITIAL_CAPACITY);
        contexts = new ArrayDeque<>(INITIAL_CAPACITY);
        src = s;
        i = 0;

        int headerRank = countHeaderRank();
        String tag = headerRank > 0 ? "h" + headerRank : "p";

        createContext("");
        convert();

        return wrap(tag, buf);
    }

    private static int countHeaderRank() {
        String head = parseWhile(0, x -> src.charAt(x) == '#');
        i = expect(" ", head.length()) ? head.length() : 0;
        return (i > 0 ? i++ : i);
    }

    private static String wrap(String tag, StringBuilder content) {
        return String.format("<%s>%s</%s>", tag, content, tag);
    }

    private static void loadContext() {
        openedTag = openedTags.getLast();
        buf = contexts.getLast();
    }

    private static void createContext(String tag) {
        contexts.addLast(new StringBuilder());
        openedTags.addLast(tag);
        loadContext();
    }

    private static void popContext(boolean tagClosed) {
        StringBuilder content = contexts.removeLast();
        String md = openedTags.removeLast();
        loadContext();
        i += 1;

        if (tagClosed) {
            buf.append(wrap(TAGS.get(md), content));

        } else {
            buf.append(md).append(content);
        }
    }

    private static boolean expect(String s, int j) {
        return (j + s.length() <= src.length() && s.equals(src.substring(j, j + s.length())));
    }

    private static String parseWhile(int i, Function<Integer, Boolean> f) {
        int j = i;
        while (j < src.length() && f.apply(j)) {
            j++;
        }
        return src.substring(i, j);
    }

    private static String parseUntilChar(int i, char c) {
        return parseWhile(i, x -> src.charAt(x) != c);
    }

    private static String parseSymmetricTag(int j) {
        return parseWhile(j, x -> TAGS.containsKey(src.substring(j, x + 1)) || src.substring(j, x + 1).equals("-"));
    }

    private static boolean parseImageTag(int j) {
        if (!expect("![", j)) {
            return false;
        }

        String alt = parseUntilChar(j + 2, ']');
        j += 2 + alt.length();

        if (!expect("](", j)) {
            return false;
        }

        String link = parseUntilChar(j + 2, ')');
        j += 2 + link.length();

        if (!expect(")", j)) {
            return false;
        }

        buf.append(
                String.format("<img alt='%s' src='%s'>", alt, link)
        );

        i = j;
        return true;
    }

    private static void convert() {
        String md;

        while (i < src.length()) {
            char c = src.charAt(i);

            if (c == '\\' && !parseSymmetricTag(i + 1).isEmpty()) {
                buf.append(src.charAt(++i));
                i += 1;
                continue;
            }

            md = parseSymmetricTag(i);

            if (!md.isEmpty() && !md.equals("-")) {
                i += md.length() - 1;

                if (md.equals(openedTag)) {
                    popContext(true);
                    continue;
                }

                createContext(md);

            } else if (!parseImageTag(i)) {
                buf.append(
                        ESCAPE_SEQ.getOrDefault(c, String.valueOf(c))
                );
            }

            i += 1;
        }

        while (contexts.size() > 1) {
            popContext(false);
        }
    }

    public static void main(String[] args) {
        StringBuilder res = new StringBuilder();

        // read

        try {
            BufferedReader in = new BufferedReader(
                    new FileReader(args[0], StandardCharsets.UTF_8)
            );

            String s = "";

            while (s != null) {
                StringBuilder p = new StringBuilder();

                while (s != null && s.isEmpty()) {
                    s = in.readLine();
                }

                while (s != null && !s.isEmpty()) {
                    if (p.length() > 0) {
                        p.append('\n');
                    }

                    p.append(s);
                    s = in.readLine();
                }

                if (p.length() > 0) {
                    res.append(convert(p.toString()));
                    res.append('\n');
                }
            }

            // :NOTE: Утечка ресурсов
            in.close();

        } catch (IOException e) {
            System.err.println("Reading error: " + e.getMessage());
            // :NOTE: Убийство JVM
            System.exit(1);
        }

        try {
            BufferedWriter out = new BufferedWriter(
                    new FileWriter(args[1], StandardCharsets.UTF_8)
            );

            out.write(res.toString());
            out.close();

        } catch (IOException e) {
            System.err.println("Writing error: " + e.getMessage());
            System.exit(2);
        }

        System.out.println("Done!");
        clear();
    }

    // write

    private static void clear() {
        buf.delete(0, buf.length());
        openedTags.clear();
        contexts.clear();
        openedTag = null;
        src = null;
    }
}
