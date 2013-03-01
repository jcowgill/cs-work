package jcowgill.prac15;

/**
 * A node on the graph of cities
 */
public class City
{
    private final String name, country, county;
    private final int timeZone;
    private final double longitude, latitude;

    /**
     * Creates a new City class
     *
     * @param name name of the city
     * @param country country of the city
     * @param county county / state / province of the city
     * @param timeZone time zone in MINUTES plus UTC
     * @param longitude longitude EAST of 0 degrees
     * @param latitude latitude NORTH of 0 degrees
     */
    public City(String name, String country, String county, int timeZone,
            double longitude, double latitude)
    {
        this.name = name;
        this.country = country;
        this.county = county;
        this.timeZone = timeZone;
        this.longitude = longitude;
        this.latitude = latitude;
    }

    @Override
    public int hashCode()
    {
        final int prime = 31;
        int result = 1;
        long temp;

        result = prime * result + name.hashCode();
        result = prime * result + country.hashCode();
        result = prime * result + county.hashCode();

        temp = Double.doubleToLongBits(latitude);
        result = prime * result + (int) (temp ^ (temp >>> 32));

        temp = Double.doubleToLongBits(longitude);
        result = prime * result + (int) (temp ^ (temp >>> 32));

        result = prime * result + timeZone;
        return result;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;

        City other = (City) obj;

        return  name.equals(other.name) &&
                country.equals(other.country) &&
                county.equals(other.county) &&
                timeZone == other.timeZone &&
                longitude == other.longitude &&
                latitude == other.latitude;
    }

    /**
     * Gets the city's name
     */
    public String getName()
    {
        return name;
    }

    /**
     * Gets the city's country
     */
    public String getCountry()
    {
        return country;
    }

    /**
     * Gets the city's county / state / province
     */
    public String getCounty()
    {
        return county;
    }

    /**
     * Gets the city's time-zone in minutes plus UTC
     */
    public int getTimeZone()
    {
        return timeZone;
    }

    /**
     * Gets the city's longitude in degrees east
     */
    public double getLongitude()
    {
        return longitude;
    }

    /**
     * Gets the city's latitude in degrees north
     */
    public double getLatitude()
    {
        return latitude;
    }

    @Override
    public String toString()
    {
        return "City " + name + ", " + country;
    }
}
