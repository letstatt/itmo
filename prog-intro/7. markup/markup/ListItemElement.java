package markup;

public interface ListItemElement extends MarkupElement {

    default void toMarkdown(StringBuilder s) {
        throw new UnsupportedOperationException();
    }
}
