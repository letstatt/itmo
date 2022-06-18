package markup;

import java.util.List;

public class UnorderedList extends Container<ListItem> implements ListItemElement {
	public UnorderedList(List<ListItem> siblings) {
		super(siblings);
	}
	
	public void toTex(StringBuilder s) {
		toTex(s, "\\begin{itemize}", "\\end{itemize}");
	}
}