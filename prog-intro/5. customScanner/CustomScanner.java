package util;

import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.StringReader;
import java.io.FileReader;
import java.io.InputStream;
import java.io.Reader;
import java.io.File;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.io.IOException;
import java.io.Closeable;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;

public final class CustomScanner implements Closeable {
	private final Pattern pattern; // our implementation (see below)
	private BufferedReader reader;
	private StringBuilder buf; // = token
	private String token;
	
	private IOException lastException = null;
	private boolean noMoreElements = false;
	private int controlSymbol = 0; // \r \n
	private boolean EOL = false; // end of line
	
	public CustomScanner(Reader in, Pattern p) {
		reader = new BufferedReader(in);
		buf = new StringBuilder();
		token = "";
		
		pattern = (p != null) ? p : new Pattern() {
			public boolean isDelimiter(int c) {
				int t = Character.getType(c);
				return (t == Character.SPACE_SEPARATOR || t == Character.CONTROL);
			}
		};
	}
	
	public CustomScanner(Reader in) {
		this(in, null);
	}
	
	public CustomScanner(InputStream in) {
		this(new InputStreamReader(in, StandardCharsets.UTF_8));
	}
	
	public CustomScanner(InputStream in, Charset charset, Pattern p) {
		this(new InputStreamReader(in, charset), p);
	}
	
	public CustomScanner(File in) throws IOException {
		this(new FileReader(in, StandardCharsets.UTF_8));
	}
	
	public CustomScanner(File in, Charset charset, Pattern p) throws IOException {
		this(new FileReader(in, charset), p);
	}
	
	public CustomScanner(String in) {
		this(new StringReader(in));
	}
	
	public CustomScanner(String in, Pattern p) {
		this(new StringReader(in), p);
	}
	
	public void close() {
		if (reader == null) {
			return;
		}

		try {
			reader.close();

		} catch (IOException e) {
			lastException = e;
		}
		
		noMoreElements = true;
		EOL = true;
		
		reader = null;
		token = null;
		buf = null;
	}
	
	public IOException ioException() {
		return lastException;
	}
	
	private boolean complementToken() { // tokenizer
		if (buf.length() > 0) {
			return true;
		}
		
		if (reader == null) {
			throw new IllegalStateException();
		}
		
		if (noMoreElements || EOL) {
			throw new NoSuchElementException();
		}
		
		try {
			while (true) {
				int c = reader.read();
				
				if (c == -1) {
					noMoreElements = true;
					EOL = true;
					break;
					
				} else if (!pattern.isDelimiter(c)) {
					buf.append((char) c);
					controlSymbol = 0;
				
				} else if (c == '\r' || c == '\n') {
					if (controlSymbol == '\r' && c == '\n') {
						controlSymbol = c;
						continue; // "\r\n" sequence, drop it
					}
					
					controlSymbol = c;
					EOL = true;
					break;
					
				} else if (buf.length() > 0) {
					break;
				}
			}
			
		} catch (IOException e) { // handle as c == -1
			lastException = e;
			noMoreElements = true;
			EOL = true;
		}
		
		token = buf.toString(); // store buf (word in general) as string to improve speed of hasNext*
		return (buf.length() > 0);
	}

	public void skipLine() {
		while (hasNext()) {
			flushToken();
		}
		skipEOL();
	}
	
	private String flushToken() {
		buf.setLength(0);
		return token;
	}
	
	public boolean hasNextLine() { // can it be read?
		return buf.length() > 0 || !noMoreElements && !EOL;
	}
	
	public void skipEOL() { // allow to go to the next line
		EOL = false;
	}
	
	public boolean hasNext() {
		if (!hasNextLine()) {
			return false;
		}
		
		return complementToken();
	}
	
	public boolean hasNextInt(int radix, boolean unsigned) {
		if (!hasNext()) {
			return false;
		}
		
		try {
			parseInt(token, radix, unsigned);
			return true;
			
		} catch (InputMismatchException e) {
			return false;
		}
	}
	
	public boolean hasNextInt() {
		return hasNextInt(10, false);
	}
	
	public boolean hasNextIntAuto() {
		if (!hasNext()) {
			return false;
		}
		
		boolean h = isHexadecimal(token);
		return hasNextInt(h ? 16 : 10, h);
	}
	
	public String next() {
		complementToken();
		return flushToken();
	}
	
	public int nextInt(int radix, boolean unsigned) {
		return parseInt(next(), radix, unsigned);
	}
	
	public int nextInt() {
		return nextInt(10, false);
	}
	
	public int nextIntAuto() {
		String s = next();
		boolean h = isHexadecimal(s);
		return parseInt(s, h ? 16 : 10, h); // hex as two's complement
	}
	
	private int parseInt(String s, int radix, boolean unsigned) {
		String t = decimalTo(s, radix);
		
		try {
			return unsigned ?
				Integer.parseUnsignedInt(t, radix) :
				Integer.parseInt(t, radix);
				
		} catch (NumberFormatException e) {
			throw new InputMismatchException();
		}
	}
	
	private String decimalTo(String s, int radix) {
		switch(radix) {
			case 10:
				return s;
			case 16:
				return truncateHex(s);
			default:
				throw new IllegalArgumentException();
		}
	}
	
	private boolean isHexadecimal(String s) {
		return (s.length() > 2 && s.charAt(0) == '0' && Character.toLowerCase(s.charAt(1)) == 'x');
	}
	
	// String "s" must match (0(X|x)[0-9a-fA-F]+)
	private String truncateHex(String s) {
		return s.substring(2);
	}
	
	public interface Pattern {
		boolean isDelimiter(int c);
	}
}