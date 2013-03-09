package jcowgill.prac16.shapes;

/**
 * A shape which draws a rectangle
 */
public class Rectangle extends Shape
{
    private int width, height;

    @Override
    public void draw(char[][] canvas)
    {
        int yEnd = (height + 1) / 2;
        int xEnd = (width + 1) / 2;

        // Draw each point using correct relative offset
        for (int offY = -height / 2; offY < yEnd; offY++)
            for (int offX = -width / 2; offX < xEnd; offX++)
                this.drawRelative(canvas, offX, offY);
    }

    public int getWidth()
    {
        return width;
    }

    public void setWidth(int width)
    {
        this.width = width;
    }

    public int getHeight()
    {
        return height;
    }

    public void setHeight(int height)
    {
        this.height = height;
    }
}
