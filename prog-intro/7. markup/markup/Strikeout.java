package markup;

import java.util.List;

public class Strikeout extends Container<TextElement> implements TextElement {
	public Strikeout(List<TextElement> siblings) {
		super(siblings);
	}
	
	public void toTex(StringBuilder s) {
		toTex(s, "\\textst{", "}");
	}

	public void toMarkdown(StringBuilder s) {
		toMarkdown(s, "~");
	}
}