package com.letstatt.tex2html;

import com.letstatt.antlr.tex2htmlLexer;
import com.letstatt.antlr.tex2htmlParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.*;
import java.nio.charset.StandardCharsets;

public class Main {
    public static void main(final String[] args) throws IOException {
        if (args == null) {
            throw new IllegalArgumentException("args == null");
        }

        InputStream input = null;
        OutputStream output = null;

        try {
            for (int i = 0; i < args.length; i++) {
                if (args[i] == null) {
                    throw new IllegalArgumentException("args[i] == null");
                }
                if (args[i].equals("-i")) {
                    if (i + 1 != args.length) {
                        if (input == null) {
                            input = new FileInputStream(args[i + 1]);
                            i++;
                        } else {
                            throw new IllegalArgumentException("-i already defined");
                        }
                    } else {
                        throw new IllegalArgumentException("-i expects a path");
                    }
                }
                if (args[i].equals("-o")) {
                    if (i + 1 != args.length) {
                        if (output == null) {
                            output = new FileOutputStream(args[i + 1]);
                            i++;
                        } else {
                            throw new IllegalArgumentException("-o already defined");
                        }
                    } else {
                        throw new IllegalArgumentException("-o expects a path");
                    }
                }
            }
        } catch (Exception e) {
            if (input != null) {
                input.close();
            }
            if (output != null) {
                output.close();
            }
            throw e;
        }

        final CharStream charStream = CharStreams.fromStream(
                input == null ? System.in : input
        );

        final tex2htmlLexer lexer = new tex2htmlLexer(charStream);
        final tex2htmlParser parser = new tex2htmlParser(new CommonTokenStream(lexer));

        final StringBuilder res = parser.tex2html().res;
        byte[] out = res.toString().getBytes(StandardCharsets.UTF_8);
        (output == null ? System.out : output).write(out);
    }
}
