import java.io.IOException;
import java.io.FileWriter;
import java.io.FileReader;

import java.io.BufferedReader;
import java.nio.charset.StandardCharsets;
import java.util.TreeMap;
import java.util.Map;

//import java.io.File;
//import java.nio.file.Files;
//import java.nio.file.StandardCopyOption;

public class WordStatWordsPrefix {
	
	public static void main(String[] args) throws IOException { // ~1700 ms
		TreeMap<String,Integer> t = new TreeMap<>();
		CustomScanner in = null;
		FileWriter out = null;
		
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
						if (s.length() >= 3) {
							s = s.substring(0, 3);
							t.compute(s, (key, val) -> val == null ? 1 : val + 1);
						}
					}
					
					in.skipEOL(); // End of line
				}
				
				for (Map.Entry<String, Integer> i: t.entrySet()) {
					out.write(i.getKey());
					out.write(" " + i.getValue());
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
	
	/*private static void copy(String src, String dst) throws IOException {
		File file1 = new File(src);
		File file2 = new File(dst);
		Files.copy(file1.toPath(), file2.toPath(), StandardCopyOption.REPLACE_EXISTING);
	}*/
}
