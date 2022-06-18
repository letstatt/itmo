package markup;

public class Text implements TextElement {
	private String text;
	
	public Text(String s) {
		text = s;
	}
	
	public void toTex(StringBuilder s) {
		s.append(text);
	}

	public void toMarkdown(StringBuilder s) {
		s.append(text);
	}
}