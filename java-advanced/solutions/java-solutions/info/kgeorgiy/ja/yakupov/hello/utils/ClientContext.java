package info.kgeorgiy.ja.yakupov.hello.utils;

import java.nio.BufferOverflowException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Class, providing tools for UDP client's requests/responses handling.
 * It tries to make as little allocations as possible during work.
 * UTF-8 charset will be used for encoding/decoding.
 */
public class ClientContext {
    private static final int MAX_FAILS_COUNT = 15;
    private final String placeholder;
    private final byte[] placeholderBytes;
    private final StringBuilder suffix;
    private final CharBuffer suffixBuffer;
    private final int requests;
    private int currentRequest;
    private int fails;

    private final CharBuffer searching;
    private final Matcher matcher;
    private final CharsetDecoder decoder;
    private final CharBuffer logging;

    /**
     * Constructs {@code ClientContext}
     * @param prefix Prefix
     * @param order Number of thread
     * @param requests Total number of requests to send
     * @param receiveBufferSize Buffer size for receiving in bytes
     */
    public ClientContext(
            final String prefix,
            final int order,
            final int requests,
            final int requestBufferSize,
            final int receiveBufferSize) {
        placeholder = prefix + order + "_";

        placeholderBytes = placeholder.getBytes(StandardCharsets.UTF_8);
        final int intLength = unsignedIntegerRepresentationLength(requests);
        suffix = new StringBuilder(intLength);
        suffixBuffer = CharBuffer.allocate(intLength);

        this.requests = requests;
        currentRequest = 0;
        fails = 0;

        searching = CharBuffer.allocate(receiveBufferSize);
        final Pattern pattern = Pattern.compile(Pattern.quote(placeholder) + "\\d+");
        matcher = pattern.matcher(searching);

        decoder = StandardCharsets.UTF_8.newDecoder();
        logging = CharBuffer.allocate(Math.max(requestBufferSize, receiveBufferSize));
    }

    /**
     * Check if necessary requests haven't already been sent
     * @return {@code True} if all necessary requests were sent
     */
    public boolean requestsEnded() {
        return currentRequest >= requests;
    }

    /**
     * Increase fails counter. If it provided too many fails,
     * then {@code ClientContext} state will be advanced.
     * @return True if {@code ClientContext} state was advanced due to numerous fails in a row
     */
    public boolean failed() {
        if (fails++ >= MAX_FAILS_COUNT) {
            advance();
            return true;
        }
        return false;
    }

    /**
     * Advance current state.
     * Increase request number and reset fails counter.
     */
    public void advance() {
        fails = 0;
        currentRequest++;
    }

    /**
     * Encode current state from chars to given buffer in bytes.
     * Buffer will already be flipped after returning.
     * Second argument is used for copy current state in chars.
     * It can be useful for logs or {@code null} can be provided.
     * All given buffers will already be flipped after returning.
     *
     * @param buffer ByteBuffer to encode current state in bytes
     */
    public void encode(final ByteBuffer buffer) {
        suffix.setLength(0);
        suffix.append(currentRequest); // no allocations

        suffixBuffer.clear();
        suffix.getChars(0, suffix.length(), suffixBuffer.array(), 0); // no allocations
        suffixBuffer.limit(suffix.length());

        logging.clear();
        logging.put(placeholder); // still no allocations
        logging.put(suffixBuffer);
        suffixBuffer.rewind();
        logging.flip();

        if (fails == 0) {
            System.out.print("Client sent: ");
            System.out.println(logging);
        }

        try {
            buffer.clear();
            buffer.put(placeholderBytes);
            // actually, suffixBuffer contains ASCII chars, so it doesn't need encoders at all.
            while (suffixBuffer.hasRemaining()) {
                buffer.put((byte) (suffixBuffer.get() & 0xFF));
            }
            buffer.flip();

        } catch (final BufferOverflowException e) {
            throw new IllegalArgumentException("Prefix is too long");
        }
    }

    /**
     * Checks if given ByteBuffer contains valid request string for current state.
     * ByteBuffer state will not be changed. CharBuffer will be cleared and flipped after returning.
     * Second argument is used for decoding given ByteBuffer to chars.
     * It can be useful for logs or {@code null} can be provided.
     *
     * @param buffer ByteBuffer to check if it contains valid request string
     */
    public boolean check(final ByteBuffer buffer) {
        searching.clear();
        buffer.mark();
        // (!) possibly allocates an object, containing 2 ints
        // or maybe not - encodeLoop() source code hidden.
        // but profiler said it makes no allocations, such a magic.
        decoder.decode(buffer, searching, true);
        buffer.reset();
        searching.flip();

        logging.clear();
        logging.put(searching);
        logging.flip();
        searching.rewind();

        boolean success = false;
        matcher.reset(); // no allocations
        searching.rewind();

        while (matcher.find() && !success) { // matcher.find() uses no allocations
            int start = matcher.start() + placeholder.length();
            success = true;
            if (suffix.length() > searching.capacity() - start) {
                success = false;
                continue;
            }
            for (int i = 0; start < searching.length() && i < suffix.length(); start++, i++) {
                if (searching.get(start) != suffix.charAt(i)) {
                    success = false;
                    break;
                }
            }
        }

        if (success) {
            System.out.print("Client received: ");
            System.out.println(logging);
            advance();
        } else {
            failed();
        }

        return success;
    }


    private static int unsignedIntegerRepresentationLength(final int i) {
        if (i < 10) {
            return 1;
        }
        return (int) Math.floor(Math.log10(i)) + 1;
    }
}
