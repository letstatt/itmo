import java.util.ArrayList;
import java.nio.charset.StandardCharsets;

/**

FastReverseHexDecTest.jar: 58000ms

*/

public class ReverseHexDec {
	
	public static void main(String[] args) {
		ArrayList<DynamicArray> a = new ArrayList<>();
		CustomScanner in = new CustomScanner(System.in);
		DynamicArray tmp = new DynamicArray();
		
		while (in.isUnlocked()) {
			while (in.hasNext()) {
				tmp.add(in.nextIntAuto());
			}
			
			in.skipEOL(); // End of line
			
			a.add(tmp);
			tmp = new DynamicArray();
		}
		
		a.remove(a.size() - 1);
		
		for (int i = a.size() - 1; i >= 0; --i) {
			DynamicArray line = a.get(i);
			
			for (int j = line.size() - 1; j >= 0; --j) {
				System.out.print(line.get(j) + " ");
			}
			
			System.out.print('\n');
		}
		
		in.close();
	}
	
	public static class DynamicArray {
		
		private final int initialCapacity = 10;
		private int[] a = null;
		private int sz = 0;
		
		DynamicArray() {
			a = new int[initialCapacity];
		}
		
		public int get(int i) {
			return a[i];
		}
		
		public void add(int i) {
			if (sz == a.length) {
				realloc();
			}
			
			a[sz++] = i;
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
