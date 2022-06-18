package info.kgeorgiy.ja.yakupov.hello.utils;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.util.Arrays;
import java.util.Objects;

/**
 * Abstract class-platform for UDP clients
 */
public abstract class AbstractUDPClient {
    private static final int DEFAULT_TIMEOUT = 150;
    protected final int receiveTimeout;

    /**
     * Default constructor
     */
    public AbstractUDPClient() {
        this(DEFAULT_TIMEOUT);
    }

    /**
     * Constructor with parameterized timeout
     * @param receiveTimeout Timeout to receive from socket
     */
    public AbstractUDPClient(final int receiveTimeout) {
        if (receiveTimeout < 10) {
            throw new IllegalArgumentException("Timeout is too small");
        }
        this.receiveTimeout = receiveTimeout;
    }

    /**
     * Start console application to run HelloUDP*Client
     * @param args 5 required arguments of HelloUDP*Client
     */
    public static void cli(final HelloClient client, final String[] args) {
        if (args == null || args.length != 5 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.out.printf("Usage: %s hostname port prefix threads repeats", client.getClass().getName());
            return;
        }

        try {
            client.run(
                    args[0],
                    Integer.parseUnsignedInt(args[1]),
                    args[2],
                    Integer.parseUnsignedInt(args[3]),
                    Integer.parseUnsignedInt(args[4])
            );
        } catch (final NumberFormatException e) {
            System.err.println("Port, threads and requests count are expected to be unsigned ints");
        } catch (final RuntimeException e) {
            System.err.println(e.getMessage());
        }
    }
}
