//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by FernFlower decompiler)
//

package info.kgeorgiy.java.advanced.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.Assert;



public final class Util {
    public static final Charset CHARSET;
    private static final List<String> ANSWER;
    private static Mode mode;
    private static final List<Function<String, String>> CORRUPTIONS;
    private static final List<Function<String, String>> EVIL_CORRUPTIONS;
    private static final List<BiFunction<String, Random, String>> EVIL_MODIFICATIONS;

    private Util() {
    }



    public static String getString(DatagramPacket packet) {
        return getString(packet.getData(), packet.getOffset(), packet.getLength());
    }

    public static String getString(byte[] data, int offset, int length) {
        return new String(data, offset, length, CHARSET);
    }

    public static void setString(DatagramPacket packet, String string) {
        byte[] bytes = getBytes(string);
        packet.setData(bytes);
        packet.setLength(packet.getData().length);
    }

    public static byte[] getBytes(String string) {
        return string.getBytes(CHARSET);
    }

    public static DatagramPacket createPacket(DatagramSocket socket) throws SocketException {
        return new DatagramPacket(new byte[socket.getReceiveBufferSize()], socket.getReceiveBufferSize());
    }

    public static String request(String string, DatagramSocket socket, SocketAddress address) throws IOException {
        send(socket, string, address);
        return receive(socket);
    }

    public static String receive(DatagramSocket socket) throws IOException {
        DatagramPacket inPacket = createPacket(socket);
        socket.receive(inPacket);
        return getString(inPacket);
    }

    public static void send(DatagramSocket socket, String request, SocketAddress address) throws IOException {
        DatagramPacket outPacket = new DatagramPacket(new byte[0], 0);
        setString(outPacket, request);
        outPacket.setSocketAddress(address);
        socket.send(outPacket);
    }

    public static String response(String request) {
        return String.format("Hello, %s", request);
    }

    public static AtomicInteger[] server(String prefix, int treads, double p, DatagramSocket socket) {
        AtomicInteger[] expected = (AtomicInteger[])Stream.generate(AtomicInteger::new).limit((long)treads).toArray((x$0) -> {
            return new AtomicInteger[x$0];
        });
        (new Thread(() -> {
            Random random = new Random(4357204587045842850L + (long)Objects.hash(new Object[]{prefix, treads, p}));

            try {
                while(true) {
                    DatagramPacket packet = createPacket(socket);
                    socket.receive(packet);
                    String request = getString(packet);
                    String message = "Invalid or unexpected request " + request;
                    Assert.assertTrue(message, request.startsWith(prefix));
                    String[] parts = request.substring(prefix.length()).split("_");
                    Assert.assertEquals(message, 2L, (long)parts.length);

                    try {
                        int thread = Integer.parseInt(parts[0]);
                        int no = Integer.parseInt(parts[1]);
                        Assert.assertTrue(message, 0 <= thread && thread < expected.length);
                        Assert.assertEquals(message, (long)expected[thread].get(), (long)no);
                        String response = mode.response(request, random);
                        if (p >= random.nextDouble()) {
                            expected[thread].incrementAndGet();
                            setString(packet, response);
                            socket.send(packet);
                        } else if (random.nextBoolean()) {
                            setString(packet, mode.corrupt(response, random));
                            socket.send(packet);
                        }
                    } catch (NumberFormatException var14) {
                        throw new AssertionError(message);
                    }
                }
            } catch (IOException var15) {
                System.err.println(var15.getMessage());
            }
        })).start();
        return expected;
    }

    private static <T> T select(List<T> items, Random random) {
        return items.get(random.nextInt(items.size()));
    }

    static void setMode(String test) {
        mode = test.endsWith("-i18n") ? Util.Mode.I18N : (test.endsWith("-evil") ? Util.Mode.EVIL : Util.Mode.NORMAL);
    }

    static {
        CHARSET = StandardCharsets.UTF_8;
        ANSWER = List.of("Hello, %s", "%s ආයුබෝවන්", "Բարեւ, %s", "مرحبا %s", "Салом %s", "Здраво %s", "Здравейте %s", "Прывітанне %s", "Привіт %s", "Привет, %s", "Поздрав %s", "سلام به %s", "שלום %s", "Γεια σας %s", "העלא %s", "ہیل%s٪ ے", "Bonjou %s", "Bonjour %s", "Bună ziua %s", "Ciao %s", "Dia duit %s", "Dobrý deň %s", "Dobrý den, %s", "Habari %s", "Halló %s", "Hallo %s", "Halo %s", "Hei %s", "Hej %s", "Hello  %s", "Hello %s", "Hello %s", "Helo %s", "Hola %s", "Kaixo %s", "Kamusta %s", "Merhaba %s", "Olá %s", "Ola %s", "Përshëndetje %s", "Pozdrav %s", "Pozdravljeni %s", "Salom %s", "Sawubona %s", "Sveiki %s", "Tere %s", "Witaj %s", "Xin chào %s", "ສະບາຍດີ %s", "สวัสดี %s", "ഹലോ %s", "ಹಲೋ %s", "హలో %s", "हॅलो %s", "नमस्कार%sको", "হ্যালো %s", "ਹੈਲੋ %s", "હેલો %s", "வணக்கம் %s", "ကို %s မင်္ဂလာပါ", "გამარჯობა %s", "ជំរាបសួរ %s បាន", "こんにちは%s", "你好%s", "안녕하세요  %s");
        CORRUPTIONS = List.of((s) -> {
            return s.replaceAll("[_\\-]", "0");
        }, (s) -> {
            return s.replaceAll("([0-9])", "$1$1");
        }, (s) -> {
            return s.replaceFirst("[0-9]", "-");
        }, (s) -> {
            return "";
        }, (s) -> {
            return "~";
        });
        EVIL_CORRUPTIONS = (List)Stream.concat(CORRUPTIONS.stream(), Stream.of((s) -> {
            return s + "0";
        }, (s) -> {
            return "0" + s;
        }, (s) -> {
            return s.replaceFirst("([0-9])", "$1$1");
        })).collect(Collectors.toUnmodifiableList());
        EVIL_MODIFICATIONS = List.of((s, r) -> {
            return s;
        }, (s, r) -> {
            return s;
        }, (s, r) -> {
            return s;
        }, (s, r) -> {
            return s;
        }, (s, r) -> {
            return s;
        }, (s, r) -> {
            return s;
        }, (s, r) -> {
            return s.replaceAll("[^0-9]", "_");
        }, (s, r) -> {
            return s.replaceAll("[^0-9]", "-");
        }, (s, r) -> {
            return Pattern.compile("([^0-9]+)").matcher(s).replaceAll((m) -> {
                return (String)select(ANSWER, r);
            });
        }, (s, r) -> {
            return s.replaceAll("([^0-9])", "$1$1");
        });
    }

    static enum Mode {
        NORMAL((request, random) -> {
            return Util.response((String) request);
        }, Util.CORRUPTIONS),
        I18N((request, random) -> {
            return String.format((String)Util.select(Util.ANSWER, (Random) random), request);
        }, Util.CORRUPTIONS),
        EVIL((request, random) -> {
            return I18N.response((String)((BiFunction)Util.select(Util.EVIL_MODIFICATIONS, (Random) random)).apply(request, random),
                    (Random) random);
        }, Util.EVIL_CORRUPTIONS);

        private final BiFunction<String, Random, String> f;
        private final List<Function<String, String>> corruptions;

        private Mode(BiFunction f, List corruptions) {
            this.f = f;
            this.corruptions = corruptions;
        }

        public String response(String request, Random random) {
            return (String)this.f.apply(request, random);
        }

        public String corrupt(String request, Random random) {
            return (String)((Function)Util.select(this.corruptions, random)).apply(request);
        }
    }
}
