package markup;

import java.util.List;

public class ListItem extends Container<ListItemElement> {
	public ListItem(List<ListItemElement> siblings) {
		super(siblings);
	}
	
	public void toTex(StringBuilder s) {
		toTex(s, "\\item ", "");
	}

	public void toMarkdown(StringBuilder s) {
		throw new UnsupportedOperationException();
	}
}