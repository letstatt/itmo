
public class SumLongSpace {

    public static void main(String[] args) {
        long sum = 0;

        for (String s: args) {
            char[] c = s.toCharArray();
            int j = -1;

            for (int i = 0; i < c.length; ++i) {
                if (c[i] == '-') j = i;
				else if (isNumeric(c[i]) && j == -1) j = i;
                else if (!isNumeric(c[i]) && j != -1) {
                    sum += Long.valueOf(s.substring(j, i));
                    j = -1;
                }
            }
			
			if (j != -1) {
                sum += Long.valueOf(s.substring(j, c.length));
                j = -1;
			}
        }

        System.out.println(sum);
    }

    private static boolean isNumeric(char c) {
        return '0' <= c && c <= '9';
    }
}