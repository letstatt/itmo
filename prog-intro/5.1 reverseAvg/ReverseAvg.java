import java.util.Scanner;
import java.util.ArrayList;

public class ReverseAvg { // ~ 2.5s
	
	public static void main(String[] args) {
		ArrayList<DynamicArray> a = new ArrayList<>();
		CustomScanner in = new CustomScanner(System.in);
		DynamicArray tmp = new DynamicArray();
		
		while (in.isUnlocked()) {
			while (in.hasNext()) {
				tmp.add(in.nextInt());
			}
			
			in.skipEOL(); // End of line
			
			a.add(tmp);
			tmp = new DynamicArray();
		}
		
		a.remove(a.size() - 1);
		
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
		
		for (int i = 0; i < a.size(); ++i) {
			DynamicArray line = a.get(i);
			
			for (int j = 0; j < line.size(); ++j) {
				long count = counterColumns.get(j) + counterRows.get(i) - 1;
				long avg = (accumulatedAvgColumns.get(j) + accumulatedAvgRows.get(i) - line.get(j)) / count;
				System.out.print(avg + " ");
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
		
		private void realloc() {
			int[] b = new int[a.length + (a.length >> 1)];
			System.arraycopy(a, 0, b, 0, sz);
			a = b;
		}
		
		public int size() {
			return sz;
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
		
		private void realloc() {
			long[] b = new long[a.length + (a.length >> 1)];
			System.arraycopy(a, 0, b, 0, sz);
			a = b;
		}
		
		public int size() {
			return sz;
		}
	}
}
