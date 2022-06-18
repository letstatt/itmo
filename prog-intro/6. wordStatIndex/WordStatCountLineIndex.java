import java.io.IOException;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.BufferedReader;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Iterator;
import java.util.Map;

public class WordStatCountLineIndex {
	
	private static int counter;
	private static int lineCounter;
	
	public static void main(String[] args) throws IOException { // ~20000 ms
		Map<String,IntList> map = new LinkedHashMap<>();
		CustomScanner in = null;
		FileWriter out = null;
		lineCounter = 1;
		
		try {
			in = new CustomScanner(
				new FileReader(args[0], StandardCharsets.UTF_8),
				new CustomScanner.Pattern() {
					public boolean isDelimiter(int c) {
						return !isAllowed(c);
					}
				}
			);
			
			while (in.hasNextLine()) {
				counter = 1;
				
				while (in.hasNext()) {
					String s = in.next().toLowerCase();
					map.compute(
						s, (k, v) -> (
							v == null ? new IntList() : v
						).add(lineCounter).add(counter)
					);
					
					++counter;
				}
				
				in.skipEOL(); // End of line
				++lineCounter;
			}
			
			in.close();
			out = new FileWriter(args[1], StandardCharsets.UTF_8);
			
			Iterator<Map.Entry<String, IntList>> it = map.entrySet().stream().sorted((i, j) -> {
				return Integer.compare(i.getValue().size(), j.getValue().size());
			}).iterator();
			
			while (it.hasNext()) {
				Map.Entry<String, IntList> i = it.next();
				IntList j = i.getValue();
				out.write(i.getKey());
				out.write(" " + j.size() / 2);
				
				for (int k = 0; k < j.size(); k += 2) {
					out.write(String.format(" %d:%d", j.get(k), j.get(k + 1)));
				}
				
				out.write('\n');
			}
			
		} catch (IOException e) {
			e.printStackTrace();
			
		} finally {
			if (in != null) {
				in.close();
			}
			if (out != null) {
				out.close();
			}
		}
	}
	
	public static boolean isAllowed(int c) {
		if (c == '\'') return true;
		int type = Character.getType(c);
		
		switch (type) {
			case Character.DASH_PUNCTUATION:
			case Character.LOWERCASE_LETTER:
			case Character.UPPERCASE_LETTER:
			return true;
		}
		
		return false;
	}
	
	public static class IntList {
		
		private final int initialCapacity = 10;
		private int[] a = null;
		private int sz = 0;
		
		IntList() {
			a = new int[initialCapacity];
		}
		
		public int get(int i) {
			return a[i];
		}
		
		public IntList add(int i) {
			if (sz == a.length) {
				realloc();
			}
			
			a[sz++] = i;
			return this;
		}
		
		private void realloc() {
			int[] b = new int[a.length + (a.length >> 1)];
			System.arraycopy(a, 0, b, 0, sz);
			a = b;
		}
		
		public int size() {
			return sz;
		}
	}
}
