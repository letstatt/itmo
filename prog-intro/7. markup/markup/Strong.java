package markup;

import java.util.List;

public class Strong extends Container<TextElement> implements TextElement {
	public Strong(List<TextElement> siblings) {
		super(siblings);
	}
	
	public void toTex(StringBuilder s) {
		toTex(s, "\\textbf{", "}");
	}

	public void toMarkdown(StringBuilder s) {
		toMarkdown(s, "__");
	}
}