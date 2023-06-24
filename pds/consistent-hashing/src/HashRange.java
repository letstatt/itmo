import java.util.Objects;

public class HashRange {
    private int leftBorder;
    private int rightBorder;

    public HashRange(int leftBorder, int rightBorder) {
        this.leftBorder = leftBorder;
        this.rightBorder = rightBorder;
    }

    public int getLeftBorder() {
        return leftBorder;
    }

    public void setLeftBorder(int leftBorder) {
        this.leftBorder = leftBorder;
    }

    public int getRightBorder() {
        return rightBorder;
    }

    public void setRightBorder(int rightBorder) {
        this.rightBorder = rightBorder;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        HashRange hashRange = (HashRange) o;
        return leftBorder == hashRange.leftBorder && rightBorder == hashRange.rightBorder;
    }

    @Override
    public int hashCode() {
        return Objects.hash(leftBorder, rightBorder);
    }
}
