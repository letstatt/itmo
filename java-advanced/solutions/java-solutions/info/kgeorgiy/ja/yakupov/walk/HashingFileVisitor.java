package info.kgeorgiy.ja.yakupov.walk;

import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

public class HashingFileVisitor extends SimpleFileVisitor<Path> {
    final public static String HASH_FALLBACK = "0".repeat(40);
    final private static int HASH_BUFFER_SIZE = 512;
    final private BufferedWriter writer;

    final private MessageDigest messageDigest;
    final private HexFormat hexFormatter;
    final byte[] buffer;

    HashingFileVisitor(final BufferedWriter writer) throws NoSuchAlgorithmException {
        this.writer = writer;
        this.hexFormatter = HexFormat.of();
        this.buffer = new byte[HASH_BUFFER_SIZE];
        this.messageDigest = MessageDigest.getInstance("SHA-1");
    }

    private String getDigest(final Path file) {
        try (final BufferedInputStream stream = new BufferedInputStream(Files.newInputStream(file))) {
            int bytesRead;

            while ((bytesRead = stream.read(buffer)) >= 0) {
                messageDigest.update(buffer, 0, bytesRead);
            }
            return hexFormatter.formatHex(messageDigest.digest());
        } catch (final IOException | SecurityException e) {
            System.err.println("Hashing error on \"" + file + "\": " + e.getMessage());
            return HASH_FALLBACK;
        }

    }

    public boolean write(final String digest, final String filename) {
        try {
            // pattern: "${digest} ${filename}${newLine}"
            writer.write(digest);
            writer.write(' ');
            writer.write(filename);
            writer.newLine();
            return true;
        } catch (final IOException e) {
            System.err.println("Error during writing to output file: " + e.getMessage());
            return false;
        }
    }

    @Override
    public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) {
        return write(getDigest(file), file.toString()) ? FileVisitResult.CONTINUE : FileVisitResult.TERMINATE;
    }

    @Override
    public FileVisitResult visitFileFailed(final Path file, final IOException e) {
        System.err.println("Hashing error, couldn't read \"" + file + "\": " + e.getMessage());
        return write(HASH_FALLBACK, file.toString()) ? FileVisitResult.CONTINUE : FileVisitResult.TERMINATE;
    }
};
