package markup;

import java.util.List;

public abstract class Container<T extends MarkupElement> implements MarkupElement {
	private final List<T> siblings;
	
	public Container(List<T> siblings) {
		this.siblings = siblings;
	}
	
	// we delegate implementing toTex(StringBuilder) to children

	protected void toMarkdown(StringBuilder s, String border) {
		s.append(border);
		for (MarkupElement i: siblings) {
			i.toMarkdown(s);
		}
		s.append(border);
	}
	
	protected void toTex(StringBuilder s, String leftBorder, String rightBorder) {
		s.append(leftBorder);
		for (MarkupElement i: siblings) {
			i.toTex(s);
		}
		s.append(rightBorder);
	}
}