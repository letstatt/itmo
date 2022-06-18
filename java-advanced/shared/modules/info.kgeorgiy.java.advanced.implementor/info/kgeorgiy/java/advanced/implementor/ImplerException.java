package info.kgeorgiy.java.advanced.implementor;

/**
 * Thrown by {@link info.kgeorgiy.java.advanced.implementor.Impler} when an error occurred.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ImplerException extends Exception {
    /** 
     * Creates an <code>IOException</code> with <code>null</code> as its error detail message.
     */
    public ImplerException() {
    }

    /** 
     * Creates an <code>IOException</code> with specified error detail message.
     *
     * @param message error detail message.
     */
    public ImplerException(final String message) {
        super(message);
    }

    /** 
     * Creates an <code>IOException</code> with specified error detail message and a cause.
     *
     * @param message error detail message.
     * @param cause error cause.
     */
    public ImplerException(final String message, final Throwable cause) {
        super(message, cause);
    }

    /** 
     * Creates an <code>IOException</code> with <code>null</code> as an 
     * error detail message and a cause.
     *
     * @param cause error cause.
     */
    public ImplerException(final Throwable cause) {
        super(cause);
    }
}
