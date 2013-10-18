package jcowgill.prac15;

import java.util.Arrays;

/**
 * A WeightedGraph which resizes itself to accommodate more cities
 */
public class AutoSizingWeightedGraph extends WeightedGraph
{
    private final int resizeFactor;

    /**
     * Creates a new AutoSizingWeightedGraph
     *
     * The resize factor controls how much the graph grows by when it runs out of space.
     * High numbers may increase performance at the cost of increased memory.
     *
     * @param n the resize factor of the graph
     */
    public AutoSizingWeightedGraph(int n)
    {
        // Start with empty arrays
        super(0);

        // n cannot be 0
        if (n <= 0)
            throw new IllegalArgumentException("n must be greater than 0");

        this.resizeFactor = n;
    }

    /**
     * Adds a new city to the graph
     *
     * You may not add a vertex if it is already on the graph
     *
     * @param city vertex to add
     * @return true if it was added, false if the city already exists
     */
    @Override
    public boolean addVertex(City city)
    {
        // Add vertex the normal way first
        try
        {
            return super.addVertex(city);
        }
        catch(GraphFullException e)
        {
            // Not enough space, grow the arrays
            int newSize = cityCount + resizeFactor;

            int[][] newEdges = new int[newSize][newSize];

            for (int i = 0; i < cityCount; i++)
            {
                // Copy array data to the new one
                System.arraycopy(edges[i], 0, newEdges[i], 0, cityCount);
            }

            edges = newEdges;
            cities = Arrays.copyOf(cities, newSize);

            // Add this city as the first 'new' city
            cities[cityCount] = city;
            cityCount++;
            return true;
        }
    }

    @Override
    public void removeVertex(City city)
    {
        // Remove city the normal way first
        super.removeVertex(city);

        // Can we reduce the size?
        if (cities.length - cityCount == resizeFactor)
        {
            // Create new arrays
            City[] newCities = new City[cityCount];
            int[][] newEdges = new int[cityCount][cityCount];

            // Create mapping array
            //  Maps newIDs to oldIDs
            int[] mappingArray = new int[cityCount];

            // Populate mapping array and copy newCities array
            int nextIndex = 0;
            for (int i = 0; i < cities.length; i++)
            {
                // City in use?
                if (cities[i] != null)
                {
                    // Transfer and allocate id for this city
                    newCities[nextIndex] = cities[i];
                    mappingArray[nextIndex] = i;
                    nextIndex++;
                }
            }

            // Copy edge data across
            for (int i = 0; i < cityCount; i++)
            {
                // Get original array for this set of edges
                int[] oldEdgeData = edges[mappingArray[i]];

                // Copy all the data
                for (int j = 0; j < cityCount; j++)
                    newEdges[i][j] = oldEdgeData[mappingArray[j]];
            }

            // Save new data
            cities = newCities;
            edges = newEdges;
        }
    }

    /**
     * Always returns Integer.MAX_VALUE
     */
    @Override
    public int getMaxSize()
    {
        return Integer.MAX_VALUE;
    }

    /**
     * Returns the resize factor for this graph
     *
     * @return the resize factor
     */
    public int getResizeFactor()
    {
        return resizeFactor;
    }
}
