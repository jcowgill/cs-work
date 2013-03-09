package jcowgill.prac16.shapes;

public class Circle extends Shape
{
    private int radius;

    @Override
    public void draw(AsciiCanvas canvas)
    {
        final int radiusSq = radius * radius;

        // Go through each point in the bounding box
        //  Draw it if the distance is within the circle's limits
        for (int offY = -radius; offY <= radius; offY++)
            for (int offX = -radius; offX <= radius; offX++)
                if (offX * offX + offY * offY <= radiusSq)
                    this.drawRelative(canvas, offX, offY);
    }

    @Override
    public double calculateArea()
    {
        return Math.PI * radius * radius;
    }

    @Override
    public double calculatePerimeter()
    {
        return 2 * Math.PI * radius;
    }

    public int getRadius()
    {
        return radius;
    }

    public void setRadius(int radius)
    {
        this.radius = radius;
    }
}
