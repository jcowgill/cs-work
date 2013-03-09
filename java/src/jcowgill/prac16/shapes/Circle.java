package jcowgill.prac16.shapes;

public class Circle extends Shape
{
    private int radius;

    @Override
    public void draw(char[][] canvas)
    {
        final int radiusSq = radius * radius;

        // Go through each point in the bounding box
        //  Draw it if the distance is within the circle's limits
        for (int offY = -radius; offY <= radius; offY++)
            for (int offX = -radius; offX <= radius; offX++)
                if (offX * offX + offY * offY <= radiusSq)
                    this.drawRelative(canvas, offX, offY);
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
