package markup;

public interface MarkupElement {
	void toTex(StringBuilder s);
	void toMarkdown(StringBuilder s);
}