package info.kgeorgiy.ja.yakupov.hello.utils;

import java.io.Closeable;
import java.io.IOException;
import java.net.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Utilities for UDP* classes
 */
public class Utils {

    /**
     * Creates {@code SocketAddress} instance from given parameters
     * @param host Hostname
     * @param port Port
     * @return {@code SocketAddress} instance
     */
    public static SocketAddress getSocketAddress(final String host, final int port) {
        try {
            return new InetSocketAddress(InetAddress.getByName(host), port);
        } catch (final UnknownHostException e) {
            throw new RuntimeException("Hostname resolving failed", e);
        }
    }

    /**
     * Creates {@code DatagramPacket} with pre-created buffer
     * @param bufferSize Buffer size for pre-cretead buffer
     * @return {@code DatagramPacket} with pre-created buffer
     */
    public static DatagramPacket createDatagramPacket(final int bufferSize) {
        return createDatagramPacket(new byte[bufferSize]);
    }

    /**
     * Creates {@code DatagramPacket} with set buffer
     * @param buffer Buffer to set
     * @return {@code DatagramPacket} with set buffer
     */
    public static DatagramPacket createDatagramPacket(final byte[] buffer) {
        return new DatagramPacket(buffer, buffer.length);
    }

    /**
     * Silently close {@code Closeable} object without exceptions handling
     * @param closeable Object to close
     */
    public static void silentClose(final Closeable closeable) {
        if (closeable != null) {
            try {
                closeable.close();
            } catch (final IOException ignored) {}
        }
    }

    /**
     * Silently close {@code Closeable} objects without exceptions handling
     * @param arr Array of objects to close
     */
    public static void silentClose(final Closeable[] arr) {
        for (final Closeable closeable: arr) {
            silentClose(closeable);
        }
    }

    /**
     * Stop {@code ExecutorService} by passing either interruption or just waiting until current tasks ended
     * @param executorService Service to stop
     * @param hard Is interruption for current tasks should be sent
     */
    public static void shutdownExecutorService(final ExecutorService executorService, final boolean hard) {
        boolean interrupted = false;
        if (hard) {
            executorService.shutdownNow();
        } else {
            executorService.shutdown();
        }
        while (true) {
            try {
                if (executorService.awaitTermination(1L, TimeUnit.MINUTES)) {
                    break;
                }
            } catch (final InterruptedException ignored) {
                interrupted = true;
            }
        }
        if (interrupted) {
            Thread.currentThread().interrupt();
        }
    }
}
