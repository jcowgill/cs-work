package jcowgill.prac16.shapes;

/**
 * An object which can be drawn on an AsciiCanvas
 */
public interface Drawable
{
    /**
     * Draws the object onto the given canvas
     * @param canvas canvas to draw onto
     */
    void draw(AsciiCanvas canvas);
}
