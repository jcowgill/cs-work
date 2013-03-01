package jcowgill.prac15;

import java.util.ArrayList;

/**
 * A weighted graph of cities stored using an adjacency matrix
 */
public class WeightedCityGraph
{
    private City[] cities;
    private int[][] edges;

    /**
     * Creates a new WeightedGraph with a maximum number of cities
     * @param n maximum number of cities
     */
    public WeightedCityGraph(int n)
    {
        // Create arrays
        this.cities = new City[n];
        this.edges = new int[n][n];
    }

    /**
     * Adds a new city to the graph
     *
     * You may not add a vertex if it is already on the graph or if the graph is full
     *
     * @param city vertex to add
     * @return true if it was added
     */
    public boolean addVertex(City city)
    {
        // Try to find a place for the vertex
        for (int i = 0; i < cities.length; i++)
        {
            if (cities[i] == null)
            {
                cities[i] = city;
                return true;
            }
            else if (cities[i].equals(city))
            {
                return false;
            }
        }

        return false;
    }

    /**
     * Removes an city from the graph
     *
     * @param city city to remove
     * @return true if it was removed
     */
    public boolean removeVertex(City city)
    {
        // Find city
        int cityId = getIndex(city);

        if (cityId < 0)
            return false;

        // Remove all the edges connecting to it
        for (int i = 0; i < edges.length; i++)
        {
            edges[i][cityId] = 0;
            edges[cityId][i] = 0;
        }

        // Remove from cities list
        cities[cityId] = null;
        return true;
    }

    /**
     * Adds an edge between two cities
     *
     * You may not add an edge if the cities are not in the graph or
     * if there is already an edge between them
     *
     * @param city1 first city
     * @param city2 second city
     * @param weight weight of the edge between the cities
     * @return true if it was added
     */
    public boolean addEdge(City city1, City city2, int weight)
    {
        // Find cities
        int city1Id, city2Id;

        city1Id = getIndex(city1);
        city2Id = getIndex(city2);

        if (city1Id < 0 || city2Id < 0)
            return false;

        // Edge exists?
        if (edges[city1Id][city2Id] != 0)
            return false;

        // Insert edge
        edges[city1Id][city2Id] = weight;
        edges[city2Id][city1Id] = weight;
        return true;
    }

    /**
     * Removes an edge between two cities
     *
     * @param city1 first city
     * @param city2 second city
     * @return true if it was removed
     */
    public boolean removeEdge(City city1, City city2)
    {
        // Find cities
        int city1Id, city2Id;

        city1Id = getIndex(city1);
        city2Id = getIndex(city2);

        if (city1Id < 0 || city2Id < 0)
            return false;

        // Edge exists?
        if (edges[city1Id][city2Id] == 0)
            return false;

        // Remove edge
        edges[city1Id][city2Id] = 0;
        edges[city2Id][city1Id] = 0;
        return true;
    }

    /**
     * Returns the degree of a city
     *
     * This is the number of edges connecting to it
     *
     * @param city city to get degree of
     * @return its degree or -1 if the city wasn't found
     */
    public int getDegree(City city)
    {
        // Find city
        int cityId = getIndex(city);

        if (cityId < 0)
            return -1;

        // Count number of connecting edges
        int count = 0;

        for (int i = 0; i < edges[cityId].length; i++)
        {
            if (edges[cityId][i] != 0)
                count++;
        }

        return count;
    }

    /**
     * Returns the number of cities in this graph
     *
     * @return number of cities in the graph
     */
    public int getSize()
    {
        int count = 0;

        for (int i = 0; i < cities.length; i++)
        {
            if (cities[i] != null)
                count++;
        }

        return count;
    }

    /**
     * Returns the maximum number of cities in this graph
     *
     * @return maximum number of cities in the graph
     */
    public int getMaxSize()
    {
        return cities.length;
    }

    /**
     * Returns the ID of a city or -1 if it wasn't found
     *
     * @param city city to find
     * @return the id or null
     */
    public int getIndex(City city)
    {
        for (int i = 0; i < cities.length; i++)
        {
            // This city?
            if (cities[i] != null && cities[i].equals(city))
                return i;
        }

        return -1;
    }

    /**
     * Returns a list if cities which a city connects to
     *
     * @param city city to check
     * @return the array of cities or null if the city does not exist
     */
    public ArrayList<City> getNeighbours(City city)
    {
        // Find city
        int cityId = getIndex(city);

        if (cityId < 0)
            return null;

        // Search for neighbours
        ArrayList<City> list = new ArrayList<City>();

        for (int i = 0; i < edges[cityId].length; i++)
        {
            if (edges[cityId][i] != 0)
                list.add(cities[edges[cityId][i]]);
        }

        return list;
    }

    @Override
    public String toString()
    {
        StringBuilder builder = new StringBuilder();

        builder.append("Cities (Max = ");
        builder.append(cities.length);
        builder.append(")\n");

        for (int i = 0; i < cities.length; i++)
        {
            if (cities[i] != null)
            {
                builder.append(String.format("%02d", i));
                builder.append(": ");
                builder.append(cities[i]);
                builder.append("\n");
            }
        }

        builder.append("\n");

        builder.append("Edges\n");

        for (int i = 0; i < edges.length; i++)
        {
            for (int j = 0; j < edges.length; j++)
            {
                if (edges[i][j] != 0)
                {
                    builder.append(String.format("%02d", i));
                    builder.append("->");
                    builder.append(String.format("%02d", j));
                    builder.append(" = ");
                    builder.append(edges[i][j]);
                    builder.append("\n");
                }
            }
        }

        builder.append("\n");
        return builder.toString();
    }
}
