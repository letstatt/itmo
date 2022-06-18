package info.kgeorgiy.ja.yakupov.walk;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.security.NoSuchAlgorithmException;

public class RecursiveWalk {
    // :NOTE: throws Exception
    private static Path testPath(final String path, final String category) throws Exception {
        Path tested;
        try {
            tested = Path.of(path);
        } catch (final InvalidPathException e) {
            // :NOTE: throw new Exception
            throw new Exception("Invalid " + category + " file path: " + e.getMessage(), e);
        }
        try {
            tested = tested.toAbsolutePath();
        } catch (final SecurityException e) {
            /* try to ignore */
        }
        return tested;
    }

    public static void main(final String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.out.println("Usage: java Walk <input file> <output file>");
            return;
        }

        final Path input;
        final Path output;

        try {
            input = testPath(args[0], "input");
            output = testPath(args[1], "output");
            // :NOTE: catch (Exception e)
        } catch (final Exception e) {
            System.err.println(e.getMessage());
            return;
        }

        try {
            if (Files.isSameFile(input, output)) {
                System.err.println("Output file cannot be same as input file");
                return;
            }
        } catch (final IOException | SecurityException e) {
            /* try to ignore */
        }

        final Path parentDirectory = output.getParent();

        // :NOTE: Нет родителя
        if (parentDirectory == null) {
            System.err.println("Expected path to output file, got " + output);
            return;
        }

        try {
            if (Files.notExists(parentDirectory)) {
                try {
                    Files.createDirectories(parentDirectory);
                } catch (final FileAlreadyExistsException e) {
                    System.err.println("Wrong output path: " + e.getMessage());
                    return;
                }
            }
        } catch (final SecurityException | IOException e) {
            // skip, it might be possible not to create dir, but to create output file
        }

        try (final BufferedReader reader = Files.newBufferedReader(input)) {
            try (final BufferedWriter writer = Files.newBufferedWriter(output)) {
                try {

                    final HashingFileVisitor fileVisitor;
                    try {
                        fileVisitor = new HashingFileVisitor(writer);
                    } catch (final NoSuchAlgorithmException e) {
                        System.err.println("SHA-1 is not supported");
                        return;
                    }

                    String filename;
                    boolean writerStatus = true;
                    while ((filename = reader.readLine()) != null && writerStatus) {
                        try {
                            Files.walkFileTree(Path.of(filename), fileVisitor);

                        } catch (final InvalidPathException e) {
                            // :NOTE: Дубль
                            System.err.println("Invalid filename \"" + filename + "\": " + e.getMessage());
                            writerStatus = fileVisitor.write(HashingFileVisitor.HASH_FALLBACK, filename);

                        } catch (final SecurityException e) {
                            System.err.println("Unable to access file \"" + filename + "\": " + e.getMessage());
                            writerStatus = fileVisitor.write(HashingFileVisitor.HASH_FALLBACK, filename);
                        }
                    }

                } catch (final IOException e) {
                    System.err.println("Error during reading input file: " + e.getMessage());
                }

            } catch (final IllegalArgumentException | UnsupportedOperationException | IOException | SecurityException e) {
                System.err.println("Unable to create output file: " + e.getMessage());
            }

        } catch (final IOException | SecurityException e) {
            System.err.println("Unable to open input file: " + e.getMessage());
        }
    }
}
