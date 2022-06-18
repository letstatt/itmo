package info.kgeorgiy.ja.yakupov.hello.utils;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Objects;

/**
 * Abstract class-platform for UDP servers
 */
public abstract class AbstractUDPServer implements HelloServer {
    protected static final byte[] placeholder = "Hello, ".getBytes(StandardCharsets.UTF_8);

    /**
     * Start console application to run HelloUDP*Server
     * @param args 2 required arguments of HelloUDP*Server
     */
    public static void cli(final Class<? extends HelloServer> helloServer, final String[] args) {
        if (args == null || args.length != 2 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.out.printf("Usage: %s port threads", helloServer.getName());
            return;
        }

        try (final HelloServer server = helloServer.getConstructor().newInstance()) {
            server.start(
                    Integer.parseUnsignedInt(args[0]),
                    Integer.parseUnsignedInt(args[1])
            );

            try {
                Thread.currentThread().join(); // join myself to enter passive waiting
            } catch (final InterruptedException e) {
                server.close();
            }

        } catch (final NumberFormatException e) {
            System.err.println("Arguments are expected to be unsigned ints");
        } catch (final Exception e) {
            System.err.println("Unexpected error happened: " + e.getMessage());
        }
    }
}
