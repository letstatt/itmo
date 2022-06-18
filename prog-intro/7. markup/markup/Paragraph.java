package markup;

import java.util.List;

public class Paragraph extends Container<TextElement> implements ListItemElement {
	public Paragraph(List<TextElement> siblings) {
		super(siblings);
	}
	
	public void toTex(StringBuilder s) {
		toTex(s, "", "");
	}

	public void toMarkdown(StringBuilder s) {
		toMarkdown(s, "");
	}
}