package jcowgill.prac16.shapes;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * A canvas containing many shapes drawn onto a character array
 */
public class AsciiCanvas
{
    private final ArrayList<Drawable> objects = new ArrayList<Drawable>();

    private final char[][] data;
    private final char background;
    private final int height;

    /**
     * Creates a new canvas with space as the background colour
     *
     * @param width canvas width
     * @param height canvas height
     */
    public AsciiCanvas(int width, int height)
    {
        this(width, height, ' ');
    }

    /**
     * Creates a new canvas
     *
     * @param width canvas width
     * @param height canvas height
     * @param background background colour of the canvas
     */
    public AsciiCanvas(int width, int height, char background)
    {
        this.data = new char[width][height];
        this.background = background;
        this.height = height;
    }

    /**
     * Gets the width of this canvas
     * @return the canvas's width
     */
    public int getWidth()
    {
        return data.length;
    }

    /**
     * Gets the height of this canvas
     * @return the canvas's height
     */
    public int getHeight()
    {
        return height;
    }

    /**
     * Gets the canvas's internal buffer
     *
     * @return the internal buffer of the canvas
     */
    public char[][] getData()
    {
        return data;
    }

    /**
     * Prints the canvas's internal buffer to the given output stream
     *
     * @param output stream to write to
     * @throws IOException thrown if there was an IO error
     */
    public void printData(Appendable output) throws IOException
    {
        for (int y = 0; y < height; y++)
        {
            for (int x = 0; x < data.length; x++)
                output.append(data[x][y]);

            output.append('\n');
        }
    }

    /**
     * Redraws all the objects on the canvas
     */
    public void redraw()
    {
        // Clear the canvas with the background colour
        for (int i = 0; i < data.length; i++)
            Arrays.fill(data[i], background);

        // Draw all the objects
        for (Drawable drawable : objects)
            drawable.draw(this);
    }

    /**
     * Sets a point on this canvas to the given colour
     *
     * Invalid positions are ignored
     *
     * @param x x position
     * @param y y position
     * @param colour colour
     */
    public void setPoint(int x, int y, char colour)
    {
        // Clip any invalid positions
        if (x >= 0 && y >= 0 && x < getWidth() && y < getHeight())
            data[x][y] = colour;
    }

    /**
     * Adds an object to the canvas to be drawn on top of all previous objects
     * @param drawable drawable object to add
     */
    public void add(Drawable drawable)
    {
        objects.add(drawable);
    }

    /**
     * Returns the mutable list of objects drawn by the canvas
     *
     * Objects are drawn from the start of the list to the end
     */
    public ArrayList<Drawable> getObjects()
    {
        return objects;
    }
}
