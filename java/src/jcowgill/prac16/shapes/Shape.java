package jcowgill.prac16.shapes;

/**
 * Class representing a drawable ASCII shape
 */
public abstract class Shape implements Drawable, GeometricCalculus
{
    private char colour;
    private int x, y;

    protected Shape()
    {
    }

    /**
     * Helper method for draw implementers to transform and draw a point
     *
     * This method translates the point's position and draws it using the shape's colour
     *
     * @param canvas canvas to draw to
     * @param x relative x offset of point
     * @param y relative y offset of point
     */
    protected void drawRelative(AsciiCanvas canvas, int x, int y)
    {
        // Translate and draw point
        canvas.setPoint(x + this.x, y + this.y, colour);
    }

    public char getColour()
    {
        return colour;
    }

    public void setColour(char colour)
    {
        this.colour = colour;
    }

    public int getX()
    {
        return x;
    }

    public int getY()
    {
        return y;
    }

    public void setPosition(int x, int y)
    {
        this.x = x;
        this.y = y;
    }
}
