package info.kgeorgiy.ja.yakupov.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;

/**
 * Console application that implements and optionally compile given class or interface.
 * If meant to be compiled, pack the class or interface into {@code .jar} file.
 *
 */
public class Implementor implements JarImpler {

    /**
     * Creates {@code .java} file containing implementation of given class or interface into given directory.
     * Resulting file named as {@code token.getSimpleName() + "Impl.java"}.
     * @param token type token to create implementation for
     * @param root root directory
     * @throws ImplerException if valid implementation couldn't be created.
     * Caused by providing illegal arguments, FS errors, I/O errors, reflection fails or SecurityManager restrictions.
     */
    @Override
    public void implement(final Class<?> token, final Path root) throws ImplerException {
        if (token == null || root == null) {
            throw new ImplerException((token == null ? "token" : "root") + " can't be null");
        }

        if (token.isPrimitive() || token.isArray() || token == Enum.class) {
            throw new ImplerException("Only interfaces and classes can be implemented");
        }

        if (Modifier.isFinal(token.getModifiers()) || Modifier.isPrivate(token.getModifiers())) {
            throw new ImplerException("Could not be implemented: object is private");
        }

        final String packageName = token.getPackageName();
        final String className = token.getSimpleName() + "Impl";

        try {
            final Path directory = Path.of(root.toString(), packageName.split("\\."));
            final Path file = Path.of(directory.toString(), className + ".java");

            try {
                if (Files.notExists(directory)) {
                    Files.createDirectories(directory);
                }
            } catch (final SecurityException e) {
                /* try to ignore */
            }

            try (final BufferedWriter writer = Files.newBufferedWriter(file)) {
                ImplementorLibrary.writer(token, packageName, className, writer);
            }

        } catch (final InvalidPathException e) {
            throw new ImplerException("Couldn't form path", e);

        } catch (final IOException | SecurityException e) {
            throw new ImplerException("Couldn't save implementation", e);
        }
    }

    /**
     * Produces {@code .jar} file implementing class or interface specified by provided token.
     * Generated class classes name should be same as classes name of the type token with Impl suffix added.
     * @param token type token to create implementation for.
     * @param jarFile target {@code .jar} file.
     * @throws ImplerException if implementing, compiling or packing couldn't be completed
     */
    @Override
    public void implementJar(final Class<?> token, final Path jarFile) throws ImplerException {
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();

        if (compiler == null) {
            throw new ImplerException("Could not find java compiler, include tools.jar to classpath");
        }

        final Path root = Path.of(".");
        Path temp = null;

        try {
            // IMPLEMENT
            temp = Files.createTempDirectory(root, null);
            implement(token, temp);

            // COMPILE
            final Path packageDir = Path.of(token.getPackage().getName().replace('.', File.separatorChar));
            final String sourceFile = packageDir.resolve(token.getSimpleName() + "Impl").toString();
            final String in = temp.resolve(sourceFile + ".java").toString();
            final Path out = temp.resolve(sourceFile + ".class");

            final URI uri = token.getProtectionDomain().getCodeSource().getLocation().toURI();

            final int exitCode = compiler.run(
                    null, null, null,
                    "-cp", temp.resolve(Path.of(uri)).toString(), in);

            if (exitCode != 0) {
                throw new ImplerException("Class compilation failed, code=" + exitCode);
            }

            // PACK
            try (final JarOutputStream stream = new JarOutputStream(Files.newOutputStream(jarFile))) {
                // zip requires "/" as path separator
                final ZipEntry zipEntry = new ZipEntry(sourceFile.replace(File.separatorChar, '/') + ".class");
                stream.putNextEntry(zipEntry);
                Files.copy(out, stream);
                stream.closeEntry();
            } catch (final IOException e) {
                throw new ImplerException("Failed to write to jar file", e);
            }

        } catch (final NullPointerException | URISyntaxException e) {
            throw new ImplerException("Cannot get classpath", e);
        } catch (final IOException e) {
            throw new ImplerException("Cannot create temporary directory", e);
        } catch (final SecurityException e) {
            throw new ImplerException("No proper rights for implementing completion", e);
        } finally {
            try {
                // :NOTE: temp != null
                if (temp != null) {
                    Files.walkFileTree(temp, DELETE_VISITOR);
                }
            } catch (final IOException e) {
                /* ignore */
            }
        }
    }

    /**
     * Entry point for console application that implements and optionally compile given class or interface.
     * If meant to be compiled, pack the class or interface into {@code .jar} file.
     * @param args Expect non-null array, containing non-null strings.
     *             <ol>
     *             <li>First argument may be {@code -jar} option, that means packing
     *             compiled input class or interface into {@code .jar} file. Omitting
     *             the argument means just implementing class or interface and
     *             placing it in output path.</li>
     *             <li>Next argument (or first if previous omitted) is [fully qualified]
     *             Java class or interface name, which is meant to be processed.</li>
     *             <li>Last argument is output path</li>
     *             </ol>
     */
    public static void main(final String[] args) {
        if (args == null || (args.length != 2 && args.length != 3)) {
            usage("Expected two or three arguments");
            return;
        }

        for (final String s: args) {
            if (s == null) {
                usage("Arguments couldn't be null");
                return;
            }
        }

        try {
            final Class<?> token;
            final Path path;

            if (args.length == 3) {
                if (!args[0].equals("-jar")) {
                    usage("option -jar is only supported, got " + args[0]);
                    return;
                }
                // :NOTE: Дубль
                token = Class.forName(args[1]);
                path = Path.of(args[2]);
            } else {
                token = Class.forName(args[0]);
                path = Path.of(args[1]);
            }

            final Implementor implementor = new Implementor();

            if (args.length == 3) {
                implementor.implementJar(token, path);
            } else {
                implementor.implement(token, path);
            }

        } catch (final LinkageError e) {
            usage("Class couldn't be loaded: " + e.getMessage());

        } catch (final ClassNotFoundException e) {
            usage("Class not found: " + e.getMessage());

        } catch (final InvalidPathException e) {
            usage("Invalid output file: " + e.getMessage());

        } catch (final ImplerException e) {
            System.err.println("Class couldn't be implemented: " + e.getMessage());
        }
    }

    /**
     * Print reason describing application cancellation and usage template.
     * @param reason Non-null string describing cancellation
     */
    private static void usage(final String reason) {
        System.err.println(reason);
        System.err.println("Usage: java [-jar] <class path> <output file>");
    }

    /**
     * File visitor to delete files and folders
     */
    private static final SimpleFileVisitor<Path> DELETE_VISITOR = new SimpleFileVisitor<>() {
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    };
}
