package markup;

import java.util.List;

public class Emphasis extends Container<TextElement> implements TextElement {
	public Emphasis(List<TextElement> siblings) {
		super(siblings);
	}
	
	public void toTex(StringBuilder s) {
		toTex(s, "\\emph{", "}");
	}

	public void toMarkdown(StringBuilder s) {
		toMarkdown(s, "*");
	}
}