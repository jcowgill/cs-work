package jcowgill.prac16.shapes;

/**
 * A shape which is bounded by a box with a width and a height
 */
public abstract class BoxedShape extends Shape
{
    private int width, height;

    /**
     * Gets the width of this shape
     */
    public int getWidth()
    {
        return width;
    }

    /**
     * Sets the width of this shape
     *
     * @param width shape's new width
     */
    public void setWidth(int width)
    {
        this.width = width;
    }

    /**
     * Gets the height of this shape
     */
    public int getHeight()
    {
        return height;
    }

    /**
     * Sets the height of this shape
     *
     * @param height shape's new height
     */
    public void setHeight(int height)
    {
        this.height = height;
    }
}
