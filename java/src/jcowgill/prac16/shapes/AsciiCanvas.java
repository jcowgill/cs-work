package jcowgill.prac16.shapes;

import java.util.ArrayList;

/**
 * A canvas containing many shapes
 */
public class AsciiCanvas
{
    private ArrayList<Shape> shapes = new ArrayList<Shape>();

    /**
     * Adds a shape to the canvas to be drawn on top of all previous shapes
     * @param shape shape to add
     */
    public void add(Shape shape)
    {
        shapes.add(shape);
    }

    /**
     * Returns the mutable list of shapes drawn by the canvas
     *
     * Shapes are drawn from the start of the list to the end
     */
    public ArrayList<Shape> getShapes()
    {
        return shapes;
    }

    /**
     * Draws the shapes in this canvas onto the given char array
     * @param canvas canvas to draw onto
     */
    public void draw(char[][] canvas)
    {
        for (Shape shape : shapes)
            shape.draw(canvas);
    }
}
