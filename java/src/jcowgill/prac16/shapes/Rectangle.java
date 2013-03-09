package jcowgill.prac16.shapes;

/**
 * A shape which draws a rectangle
 */
public class Rectangle extends BoxedShape
{
    @Override
    public void draw(char[][] canvas)
    {
        final int height = getHeight();
        final int width = getWidth();

        final int yEnd = (height + 1) / 2;
        final int xEnd = (width + 1) / 2;

        // Draw each point using correct relative offset
        for (int offY = -height / 2; offY < yEnd; offY++)
            for (int offX = -width / 2; offX < xEnd; offX++)
                this.drawRelative(canvas, offX, offY);
    }

    @Override
    public double calculateArea()
    {
        return getHeight() * getWidth();
    }

    @Override
    public double calculatePerimeter()
    {
        return 2 * (getHeight() + getWidth());
    }
}
