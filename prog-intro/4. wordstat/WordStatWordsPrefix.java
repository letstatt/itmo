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
		BufferedReader in = null;
		FileWriter out = null;
		
		try {
			in = new BufferedReader(
				new FileReader(args[0], StandardCharsets.UTF_8)
			);
			
			int c = in.read();
			StringBuilder buf = new StringBuilder();
			
			while (c != -1) {
				c = Character.toLowerCase(c);
				
				if (isAllowed(c)) {
					if (buf.length() < 3) {
						buf.appendCodePoint(c);
					}
					
				} else if (buf.length() > 0) {
					if (buf.length() == 3) {
						String s = buf.toString().toLowerCase();
						t.compute(s, (key, val) -> val == null ? 1 : val + 1);
					}
					
					buf.setLength(0);
				}
				
				c = in.read();
			}
			
			in.close();
			out = new FileWriter(args[1], StandardCharsets.UTF_8);
			
			for (Map.Entry<String, Integer> i: t.entrySet()) {
				out.write(i.getKey());
				out.write(" " + i.getValue());
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
		
		//copy(args[0], args[0] + ".1");
		//copy(args[1], args[1] + ".1");
	}
	
	public static boolean isAllowed(int c) {
		if (c == '\'') return true;
		int type = Character.getType(c);
		
		switch (type) {
			case Character.DASH_PUNCTUATION:
			case Character.LOWERCASE_LETTER:
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
