package info.kgeorgiy.ja.yakupov.hello;

import info.kgeorgiy.ja.yakupov.hello.utils.AbstractUDPClient;
import info.kgeorgiy.ja.yakupov.hello.utils.ClientContext;
import info.kgeorgiy.ja.yakupov.hello.utils.Utils;
import info.kgeorgiy.java.advanced.hello.HelloClient;
import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.net.SocketTimeoutException;
import java.nio.ByteBuffer;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.IntStream;

/**
 * Class providing requests sending to {@code HelloServer}
 * @see HelloServer
 */
public class HelloUDPClient extends AbstractUDPClient implements HelloClient {

    /**
     * Default constructor. Do nothing.
     */
    public HelloUDPClient() {}

    /**
     * Construct {@code HelloUDPClient} with specified timeout
     * @param receiveTimeout timeout to receive per request (must be >= 10)
     */
    public HelloUDPClient(final int receiveTimeout) {
        super(receiveTimeout);
    }

    /**
     * Start threads to send requests to {@code HelloUDPServer}
     * @param host server host
     * @param port server port
     * @param prefix request prefix
     * @param threadsCount number of request threads
     * @param requests number of requests per thread.
     */
    @Override
    public void run(final String host, final int port, final String prefix, final int threadsCount, final int requests) {
        final SocketAddress sa = Utils.getSocketAddress(host, port);
        final ExecutorService executor = Executors.newFixedThreadPool(threadsCount);

        IntStream.range(0, threadsCount)
                .mapToObj(i -> new Thread(() -> clientInit(sa, prefix, i, requests)))
                .forEach(executor::submit);

        Utils.shutdownExecutorService(executor, false);
    }

    private void clientInit(final SocketAddress sa, final String prefix, final int order, final int requests) {
        try (final DatagramSocket socket = new DatagramSocket()) {
            socket.setSoTimeout(receiveTimeout);

            final int sendBufferSize = socket.getSendBufferSize();
            final int receiveBufferSize = socket.getReceiveBufferSize();

            final ClientContext context = new ClientContext(
                    prefix, order, requests, sendBufferSize, receiveBufferSize);

            final ByteBuffer requestBytes = ByteBuffer.allocate(sendBufferSize);
            final ByteBuffer responseBytes = ByteBuffer.allocate(receiveBufferSize);

            final DatagramPacket request = Utils.createDatagramPacket(requestBytes.array());
            final DatagramPacket response = Utils.createDatagramPacket(responseBytes.array());
            request.setSocketAddress(sa);

            clientRunner(requests, context, socket, requestBytes, request, responseBytes, response);

        } catch (final IOException e) {
            System.err.printf("Thread number %d got I/O failed: %s", order, e.getMessage());
        } catch (final SecurityException e) {
            System.err.printf("Thread number %d made prohibited action: %s", order, e.getMessage());
        }
    }

    private static void clientRunner(
            final int requests,
            final ClientContext context,
            final DatagramSocket socket,
            final ByteBuffer requestBytes,
            final DatagramPacket request,
            final ByteBuffer responseBytes,
            final DatagramPacket response) throws IOException {

        for (int i = 0; i < requests; i++) {
            context.encode(requestBytes);
            request.setLength(requestBytes.remaining());

            while (true) {
                // make stop possible
                if (Thread.interrupted()) {
                    Thread.currentThread().interrupt();
                    break;
                }
                try {
                    socket.send(request);
                    socket.receive(response);

                    responseBytes.clear();
                    responseBytes.limit(response.getLength());

                    if (context.check(responseBytes)) {
                        break;
                    }

                } catch (final SocketTimeoutException ignored) {
                    if (context.failed()) {
                        System.err.print("request skipped due to errors");
                        break;
                    }
                }
            }
        }
    }

    /**
     * Start CLI application
     * @param args command-line args
     */
    public static void main(final String[] args) {
        cli(new HelloUDPClient(), args);
    }
}
