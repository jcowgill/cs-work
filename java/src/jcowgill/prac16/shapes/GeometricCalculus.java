package jcowgill.prac16.shapes;

/**
 * Interface which calculates the area and perimeter of a shape
 */
public interface GeometricCalculus
{
    /**
     * Calculates the area of the shape
     * @return the shape's area
     */
    double calculateArea();

    /**
     * Calculates the perimeter of the shape
     * @return the shape's perimeter
     */
    double calculatePerimeter();
}
