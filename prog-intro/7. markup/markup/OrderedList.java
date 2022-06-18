package markup;

import java.util.List;

public class OrderedList extends Container<ListItem> implements ListItemElement {
	public OrderedList(List<ListItem> siblings) {
		super(siblings);
	}
	
	public void toTex(StringBuilder s) {
		toTex(s, "\\begin{enumerate}", "\\end{enumerate}");
	}
}