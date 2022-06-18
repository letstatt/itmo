import java.util.Scanner;

public class Kek {
	
	public static Scanner in;
	public static final int bitLength = 32;
	public static int radix = 10;
	public static char c = 'x';
	public static int num;
	
	public static void main(String[] args) {
		in = new Scanner(System.in);
		
		if (args.length > 0 && args[0].equals("binary")) {
			System.out.println("You have entered binary mode - in, out radix is 2.\nNotice: this programm uses 31-bit length numbers.\nTo set a sign use unary '-' or '+'.\n");
			
			radix = 2;
		}
		
		while (true) {
			try {
				inputCom();
				
				switch (c) {
					case 'e':
						inputNumber("Enter number to encode: ");
						encode();
						break;
					case 'd':
						inputNumber("Enter number to decode: ");
						decode();
						break;
					case 'q':
						in.close();
						return;
					default:
						parsingError();
				}
				
			} catch (Exception e) {
				parsingError();
			}
		}
	}
	
	public static void inputCom() throws Exception {
		System.out.print("Encode, decode, quit [E,D,Q]: ");
		String input = in.next();
		
		if (input.length() != 1) {
			throw new Exception();
		}
		
		c = Character.toLowerCase(input.charAt(0));
	}
	
	public static void inputNumber(String tip) throws Exception {
		System.out.print(tip);
		String input = in.next();
		num = Integer.parseInt(input, radix);
	}

	public static void parsingError() {
		System.out.println("Parsing error\n");
	}
	
	public static void encode() {
		int conv = (num >> (bitLength - 1)) ^ (num << 1);
		System.out.println("Result: " + Integer.toString(conv, radix) + "\n");
	}
	
	public static void decode() {
		int conv = (num >>> 1) ^ -(num & 1);
		System.out.println("Result: " + Integer.toString(conv, radix) + "\n");
	}
}