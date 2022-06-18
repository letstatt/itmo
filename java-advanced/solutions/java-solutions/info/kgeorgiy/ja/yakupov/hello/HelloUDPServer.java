package info.kgeorgiy.ja.yakupov.hello;

import info.kgeorgiy.ja.yakupov.hello.utils.AbstractUDPServer;
import info.kgeorgiy.ja.yakupov.hello.utils.Utils;
import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.IntStream;

/**
 * Echo server.
 * @see HelloUDPClient
 */
public class HelloUDPServer extends AbstractUDPServer implements HelloServer {
    private static final int MAX_FAILS_COUNT = 10;
    private static final int FAIL_TIME_SENSITIVITY_IN_SEC = 10;
    private int receiveBufferCapacity;
    private int sendBufferCapacity;
    private ExecutorService workers;
    private DatagramSocket socket;
    private boolean started = false;

    /**
     * Default constructor. Do nothing.
     */
    public HelloUDPServer() {}

    /**
     * Start server. Open UDP socket and start working threads.
     * @param port server port.
     * @param threads number of working threads.
     */
    @Override
    public void start(final int port, final int threads) {
        if (started) {
            throw new IllegalStateException("Server has already been started");
        }
        started = true;

        try {
            socket = new DatagramSocket(port);
            receiveBufferCapacity = socket.getReceiveBufferSize();
            sendBufferCapacity = socket.getSendBufferSize();

            workers = Executors.newFixedThreadPool(threads);
            IntStream.range(0, threads).forEach(i -> workers.submit(this::serverInstance));

        } catch (final SocketException e) {
            System.err.println("Unable to open socket: " + e.getMessage());
        } catch (final IllegalArgumentException e) {
            System.err.println("Port is out of range: " + e.getMessage());
        } catch (final SecurityException e) {
            System.err.println("Unable to bind socket: " + e.getMessage());
        }
    }

    private void serverInstance() {
        final DatagramPacket received = Utils.createDatagramPacket(receiveBufferCapacity);
        final DatagramPacket response = Utils.createDatagramPacket(sendBufferCapacity);

        System.arraycopy(placeholder, 0, response.getData(), 0, placeholder.length);

        int fails = 0;
        long lastFailTime = 0;

        while (true) {
            if (Thread.interrupted()) {
                Thread.currentThread().interrupt();
                break;
            }

            try {
                socket.receive(received);
            } catch (final IOException e) {
                if (socket.isClosed()) {
                    System.err.println("Socket closed, shutting down...");
                    break;
                }

                System.err.println("I/O exception happened during receiving: " + e.getMessage());

                final long currentTime = System.currentTimeMillis();

                if (currentTime - lastFailTime < FAIL_TIME_SENSITIVITY_IN_SEC * 1000) {
                    if (fails++ == MAX_FAILS_COUNT) {
                        System.err.println("Errors limit exceeded, shutting down...");
                        socket.close();
                        break;
                    }
                } else {
                    fails = 1;
                }

                lastFailTime = currentTime;
                continue;
            }

            final int msgLength = Math.min(
                    response.getData().length - placeholder.length,
                    received.getLength());

            System.arraycopy(
                    received.getData(), // src
                    0,                  // srcPos
                    response.getData(), // dest
                    placeholder.length, // destPos
                    msgLength           // length
            );

            response.setLength(placeholder.length + msgLength);
            response.setSocketAddress(received.getSocketAddress());

            try {
                socket.send(response);
            } catch (final IOException e) {
                System.err.println("Unable to respond: " + e.getMessage());
            }
        }
    }

    /**
     * Stop server.
     * Close socket and stop server threads.
     */
    @Override
    public void close() {
        Utils.silentClose(socket);
        if (workers != null) {
            Utils.shutdownExecutorService(workers, true);
        }
    }

    /**
     * Start CLI application
     * @param args command-line args
     */
    public static void main(final String[] args) {
        cli(HelloUDPServer.class, args);
    }
}
