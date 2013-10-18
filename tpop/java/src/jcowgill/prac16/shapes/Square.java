package jcowgill.prac16.shapes;

/**
 * A shape which draws a square
 */
public class Square extends Rectangle
{
    @Override
    public final void setWidth(int width)
    {
        // Ensure width always equals height
        super.setWidth(width);
        super.setHeight(width);
    }

    @Override
    public final void setHeight(int height)
    {
        // Ensure width always equals height
        super.setWidth(height);
        super.setHeight(height);
    }
}
