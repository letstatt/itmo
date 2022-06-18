package info.kgeorgiy.ja.yakupov.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.io.IOException;
import java.io.Writer;
import java.lang.reflect.*;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;


/**
 * Class supporting necessary functions for creating
 * default implementations for given class or interface.
 *
 */
abstract class ImplementorLibrary implements Impler {

    // CONSTANTS

    /**
     * New-line separator used in the resulting {@code .java} files.
     */
    private static final String EOL = System.lineSeparator();

    // WRAPPERS

    /**
     * Wrapper providing signatures of methods distinguishing.
     * Useful in {@code HashSet}.
     *
     * @param method Name of method
     * @param params List of class tokens of method arguments
     * @see Record
     * @see Object#hashCode()
     * @see Object#equals(Object)
     */
    private record MethodWrapper(String method, List<Class<?>> params) {
        /**
         * Delegating constructor for {@link MethodWrapper#MethodWrapper(String, List)}
         *
         * @param method Name of method
         * @param params List of class tokens of method arguments
         */
        MethodWrapper(final String method, final Parameter[] params) {
            this(method, Arrays.stream(params).map(Parameter::getType).collect(Collectors.toList()));
        }
    }

    // UTILITIES

    /**
     * Returns canonical name of return type of {@code Method}
     *
     * @param m Method to get the return type for
     * @return String representation of canonical name
     */
    private static String returnTypeName(final Method m) {
        return m.getReturnType().getCanonicalName();
    }

    /**
     * Returns representation of an argument list of {@code Executable}, including types (optional).
     *
     * @param e         Executable to get representation for
     * @param showTypes Flag indicating if types must be included in the result
     * @return String representation of comma-separated argument list
     */
    // :NOTE: private parameters
    private static String paramsToString(final Executable e, final boolean showTypes) {
        return Arrays.stream(e.getParameters()).map(
                param -> (showTypes ? param.getType().getCanonicalName() : "") + " " + param.getName()
        ).collect(Collectors.joining(", "));
    }

    /**
     * Returns representation of exception types list of {@code Executable}.
     * Includes keyword "{@code throws}" or is empty string if there are no checked exceptions.
     *
     * @param e Executable to get representation for
     * @return String representation of exception types
     */
    // :NOTE: private exceptions
    private static String exceptionsTokens(final Executable e) {
        final String exceptions = Arrays.stream(e.getExceptionTypes())
                .map(Class::getCanonicalName).collect(Collectors.joining(", "));
        return (exceptions.isEmpty() ? "" : String.format("throws %s", exceptions));
    }

    /**
     * Returns representation of default value of given object.
     *
     * @param c Class token of object to give default value representation for
     * @return String representation of default value
     */
    private static String defaultValueRepresentation(final Class<?> c) {
        if (c.equals(void.class)) {
            return "";
        } else if (c.equals(boolean.class)) {
            return "false";
        } else if (c.isPrimitive()) {
            return "0";
        } else {
            return "null";
        }
    }

    /**
     * Returns representation of default implementation of given {@code Method}.
     *
     * @param m Method to get representation for
     * @return String representation of default implementation
     */
    private static String defaultMethodAction(final Method m) {
        return String.format("return %s;", defaultValueRepresentation(m.getReturnType()));
    }

    /**
     * Returns representation of default implementation of given {@code Constructor}.
     *
     * @param e Constructor to get representation for
     * @return String representation of default implementation
     */
    private static String defaultConstructorAction(final Constructor<?> e) {
        return String.format("super(%s);", paramsToString(e, false));
    }

    /**
     * Escape all non-ASCII symbols by unicode sequence
     *
     * @param seq Any non-null string
     * @return Escaped string
     */
    private static String escape(final String seq) {
        final StringBuilder b = new StringBuilder();

        for (final char c : seq.toCharArray()) {
            if (c >= 128) {
                b.append(String.format("\\u%04x", (int) c));
            } else {
                b.append(c);
            }
        }

        return b.toString();
    }

    // IMPLEMENTORS

    /**
     * Generates valid Java definition of {@code Executable}.
     * Result is suitable in {@code .java} files.
     *
     * @param e               {@code Executable} to get modifiers, exceptions and parameters from.
     * @param returnTypeToken String representation for return type, suitable in {@code .java} files.
     * @param name            Name of generating {@code Executable}
     * @param defaultAction   String representation for body of {@code Executable}, suitable in {@code .java} files.
     * @return valid Java definition of {@code Executable}
     */
    private static String executableTemplateGenerator(final Executable e, final String returnTypeToken, final String name, final String defaultAction) {
        // :NOTE: String.format
        return "\t" +
                // public static
                // :NOTE: Modifier.TRANSIENT
                Modifier.toString(e.getModifiers() & ~Modifier.ABSTRACT & ~Modifier.TRANSIENT) +
                // void
                " " + returnTypeToken + " " +
                // main or MyClassImpl
                name +
                // (String[] args)
                "(" + paramsToString(e, true) + ") " +
                // throws MyException {
                exceptionsTokens(e) + " {" + EOL +
                // return something; or super(args);
                "\t\t" + defaultAction + EOL +
                // }
                "\t}" + EOL;
    }

    /**
     * Generates valid Java definitions for constructors of
     * implementation of given class. Result is suitable in {@code .java} files.
     *
     * @param token     Class token, representing class to create implementation constructors for
     * @param className Class name the constructors are defined in
     * @return String representation of generated constructors
     * @throws ImplerException   if there are no accessible constructors to implement
     * @throws SecurityException if unable to get list of declared constructors
     */
    private static String constructorsImplementor(final Class<?> token, final String className) throws ImplerException, SecurityException {
        final String res = Arrays.stream(token.getDeclaredConstructors())
                .parallel()
                .filter(c -> !Modifier.isPrivate(c.getModifiers()))
                .map(c -> executableTemplateGenerator(c, "", className, defaultConstructorAction(c)))
                .collect(Collectors.joining());

        if (res.isEmpty()) {
            throw new ImplerException("No constructor can be implemented");
        }
        return res;
    }

    /**
     * Generates valid Java definitions for methods of
     * implementation of given class or interface.
     * Result is suitable in {@code .java} files.
     *
     * @param token Class token, representing class to create implementation methods for
     * @return String representation of generated methods
     * @throws SecurityException if unable to get list of methods of superclasses and of used interfaces
     */
    private static String methodsImplementor(Class<?> token) throws SecurityException {
        final Set<MethodWrapper> implementedMethods = new HashSet<>();
        final StringBuilder tmp = new StringBuilder();

        final Consumer<Method[]> implement =
                a -> tmp.append(Arrays.stream(a)
                        .map(m -> {
                            final MethodWrapper wrapper = new MethodWrapper(m.getName(), m.getParameters());
                            if (!implementedMethods.contains(wrapper)) {
                                implementedMethods.add(wrapper);
                                return m;
                            }
                            return null;
                        })
                        .filter(Objects::nonNull)
                        .filter(m -> Modifier.isAbstract(m.getModifiers()))
                        .map(m -> executableTemplateGenerator(m, returnTypeName(m), m.getName(), defaultMethodAction(m)))
                        .collect(Collectors.joining()));

        // traverse the whole inheritance tree

        while (token != null && Modifier.isAbstract(token.getModifiers())) {
            // implement abstract methods of class representing by token
            implement.accept(token.getDeclaredMethods());

            // implement abstract methods of interfaces
            Arrays.stream(token.getInterfaces())
                    .forEach(i -> implement.accept(i.getMethods()));

            token = token.getSuperclass();
        }
        return tmp.toString();
    }

    // MAIN PART

    /**
     * Writes string representation of valid Java implementation
     * of class or interface. Result is suitable in {@code .java} files.
     *
     * @param token       Class token, representing class to create implementation for
     * @param packageName Package name of given class
     * @param className   Desired class name of implementation
     * @param writer      Writer opened to file to write implementation in
     * @throws ImplerException   If implementation couldn't be generated valid or written properly
     */
    static void writer(final Class<?> token, final String packageName, final String className, final Writer writer) throws ImplerException {
        try {
            // preamble
            if (!packageName.isEmpty()) {
                writer.write("package " + escape(packageName) + ";" + EOL.repeat(2));
            }

            // declaration
            writer.write("public class " + escape(className) + " ");
            writer.write((token.isInterface() ? "implements" : "extends") + " ");
            writer.write(escape(token.getCanonicalName()) + " {" + EOL);

            // constructors (if given token represents class) or default implied
            if (!token.isInterface()) {
                writer.write(escape(constructorsImplementor(token, className)));
            }

            // methods
            writer.write(escape(methodsImplementor(token)));

            // ending
            writer.write("}" + EOL);

        } catch (final IOException e) {
            throw new ImplerException("Couldn't write implementation", e);

        } catch (final SecurityException e) {
            throw new ImplerException("No proper rights for implementing completion", e);
        }
    }
}
