import java.util.Scanner;
import java.util.ArrayList;

public class ReverseAvg { // ~ 11s
	
	public static void main(String[] args) {
		ArrayList<DynamicArray> a = new ArrayList<>();
		Scanner lineScanner = new Scanner(System.in);
		
		while (lineScanner.hasNextLine()) {
			Scanner intScanner = new Scanner(lineScanner.nextLine());
			DynamicArray ints = new DynamicArray();
			
			while(intScanner.hasNextInt()) {
				ints.add(intScanner.nextInt());
			}
			
			a.add(ints);
		}
		
		DynamicLongArray accumulatedAvgColumns = new DynamicLongArray();
		DynamicArray counterColumns = new DynamicArray();
		
		DynamicLongArray accumulatedAvgRows = new DynamicLongArray();
		DynamicArray counterRows = new DynamicArray();
		
		for (DynamicArray ints: a) {
			long avg = 0;
			
			for (int i = 0; i < ints.size(); ++i) {
				if (i >= accumulatedAvgColumns.size()) {
					accumulatedAvgColumns.add(0);
					counterColumns.add(0);
				}
				
				accumulatedAvgColumns.incr(ints.get(i), i);
				counterColumns.incr(1, i);
				
				avg += ints.get(i);
			}
			
			accumulatedAvgRows.add(avg);
			counterRows.add(ints.size());
		}
		
		//System.out.println(accumulatedAvgColumns);
		//System.out.println(accumulatedAvgRows);
		//System.out.println();
		
		for (int i = 0; i < a.size(); ++i) {
			DynamicArray line = a.get(i);
			
			for (int j = 0; j < line.size(); ++j) {
				long count = counterColumns.get(j) + counterRows.get(i) - 1;
				
				//if (count >= 0) {
					long avg = (accumulatedAvgColumns.get(j) + accumulatedAvgRows.get(i) - line.get(j)) / count;
					System.out.print(avg + " ");
				//}
			}
			
				
			System.out.println();
		}
		
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
		
		public void incr(int i, int ind) {
			a[ind] += i;
		}
		
		public void pop() {
			if (sz == 0) {
				throw new ArrayIndexOutOfBoundsException();
			}
			
			--sz;
		}
		
		private void realloc() {
			int[] b = new int[a.length + (a.length >> 1)];
			System.arraycopy(a, 0, b, 0, sz);
			a = b;
		}
		
		public int size() {
			return sz;
		}
		
		public String toString() {
			StringBuilder s = new StringBuilder();
			for (int i = 0; i < sz - 1; ++i) {
				s.append(a[i]);
				s.append(' ');
			}
			
			if (sz > 0) s.append(a[sz - 1]);
			return s.toString();
		}
	}
	
	public static class DynamicLongArray {
		
		private final int initialCapacity = 10;
		private long[] a = null;
		private int sz = 0;
		
		DynamicLongArray() {
			a = new long[initialCapacity];
		}
		
		public long get(int i) {
			return a[i];
		}
		
		public void add(long i) {
			if (sz == a.length) {
				realloc();
			}
			
			a[sz++] = i;
		}
		
		public void incr(long i, int ind) {
			a[ind] += i;
		}
		
		public void pop() {
			if (sz == 0) {
				throw new ArrayIndexOutOfBoundsException();
			}
			
			--sz;
		}
		
		private void realloc() {
			long[] b = new long[a.length + (a.length >> 1)];
			System.arraycopy(a, 0, b, 0, sz);
			a = b;
		}
		
		public int size() {
			return sz;
		}
		
		public String toString() {
			StringBuilder s = new StringBuilder();
			for (int i = 0; i < sz - 1; ++i) {
				s.append(a[i]);
				s.append(' ');
			}
			
			if (sz > 0) s.append(a[sz - 1]);
			return s.toString();
		}
	}
}
