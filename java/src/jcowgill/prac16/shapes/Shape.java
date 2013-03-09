package jcowgill.prac16.shapes;

/**
 * Class representing a drawable ASCII shape
 */
public abstract class Shape implements GeometricCalculus
{
    private char colour;
    private int x, y;

    protected Shape()
    {
    }

    /**
     * Draws this shape onto the given canvas
     *
     * @param canvas canvas to draw onto (indexed canvas[x][y])
     */
    public abstract void draw(char[][] canvas);

    /**
     * Helper method for draw implementers to transform and draw a point
     *
     * This method translates the point's position and draws it using the shape's colour
     *
     * @param canvas canvas to draw to
     * @param x relative x offset of point
     * @param y relative y offset of point
     */
    protected void drawRelative(char[][] canvas, int x, int y)
    {
        // Move point
        int newX = x + this.x;
        int newY = y + this.y;

        // Draw point or clip if out of bounds
        if (newX >= 0 && newX < canvas.length)
        {
            char[] column = canvas[newX];

            if (newY >= 0 && newY < column.length)
                column[newY] = this.colour;
        }
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
