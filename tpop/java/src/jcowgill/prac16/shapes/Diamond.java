package jcowgill.prac16.shapes;

/**
 * A shape which draws a diamond
 */
public class Diamond extends BoxedShape
{
    @Override
    public void draw(AsciiCanvas canvas)
    {
        final int height = getHeight();
        final int width = getWidth();

        final int yEnd = (height / 2) + 1;
        final int xEnd = (width / 2) + 1;

        final int sizeConstant = (height * width) / 2;

        // Go through each point in the bounding box
        //  Draw it if the distance is within the diamond's limits
        //   Abs(x/width) + Abs(y/height) = 1
        //   Abs(x)/width + Abs(y)/height = 1
        //   height * Abs(x) + width * Abs(y) = height * width
        for (int offY = -height / 2; offY < yEnd; offY++)
            for (int offX = -width / 2; offX < xEnd; offX++)
                if (height * Math.abs(offX) + width * Math.abs(offY) <= sizeConstant)
                    this.drawRelative(canvas, offX, offY);
    }

    @Override
    public double calculateArea()
    {
        return (getHeight() * getWidth()) / 2.0;
    }

    @Override
    public double calculatePerimeter()
    {
        return 4.0 * Math.sqrt(getHeight() * getHeight() + getWidth() * getWidth());
    }
}
