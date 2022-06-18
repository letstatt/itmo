import java.io.IOException;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.BufferedReader;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;
//import java.nio.file.Files;
//import java.nio.file.StandardCopyOption;

public class WordStatIndex {
	
	private static int counter;
	
	public static void main(String[] args) throws IOException { // ~17000 ms
		LinkedHashMap<String,IntList> t = new LinkedHashMap<>();
		CustomScanner in = null;
		FileWriter out = null;
		counter = 1;
		
		try {
			in = new CustomScanner(
				new FileReader(args[0], StandardCharsets.UTF_8),
				new CustomScanner.Pattern() {
					public boolean isDelimiter(int c) {
						return !isAllowed(c);
					}
				}
			);
			
			try {
				out = new FileWriter(args[1], StandardCharsets.UTF_8);
				
				while (in.isUnlocked()) {
					while (in.hasNext()) {
						String s = in.next().toLowerCase();
						t.compute(s, (k, v) -> v == null ? new IntList().add(counter) : v.add(counter));
						++counter;
					}
					
					in.skipEOL(); // End of line
				}
				
				for (Map.Entry<String, IntList> i: t.entrySet()) {
					IntList j = i.getValue();
					out.write(i.getKey());
					out.write(" " + j.size());
					
					for (int k = 0; k < j.size(); ++k) {
						out.write(" " + j.get(k));
					}
					
					out.write('\n');
				}
			
			} catch (IOException e) {
				e.printStackTrace();
				
			} finally {
				if (out != null) {
					out.close();
				}
			}
			
		} catch (IOException e) {
			e.printStackTrace();
			
		} finally {
			if (in != null) {
				in.close();
			}
		}
		
		//copy(args[0], args[0] + ".1");
		//copy(args[1], args[1] + ".1");
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
	
	/*private static void copy(String src, String dst) throws IOException {
		File file1 = new File(src);
		File file2 = new File(dst);
		Files.copy(file1.toPath(), file2.toPath(), StandardCopyOption.REPLACE_EXISTING);
	}*/
}
