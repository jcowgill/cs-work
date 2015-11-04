package jcowgill.embs;

import ptolemy.actor.TypedAtomicActor;
import ptolemy.actor.util.Time;
import ptolemy.data.DoubleToken;
import ptolemy.data.IntToken;
import ptolemy.data.RecordToken;
import ptolemy.data.Token;
import ptolemy.data.expr.StringConstantParameter;
import ptolemy.data.type.BaseType;
import ptolemy.domains.wireless.kernel.WirelessIOPort;
import ptolemy.kernel.CompositeEntity;
import ptolemy.kernel.util.IllegalActionException;
import ptolemy.kernel.util.NameDuplicationException;

public class FirefighterLocator extends TypedAtomicActor
{
    private final static double ONE_OVER_4_PI = 1.0 / (4.0 * Math.PI);

    /** Amount of time passing before a signal expires */
    private final static double SIGNAL_EXPIRY_TIME = 3;

    /** Number of map tiles each way in the map */
    private final static int MAP_RESOLUTION = 4;

    /* Convinience map calculation variables */
    private final static double MAP_START = 1.0 / (2 * MAP_RESOLUTION);
    private final static double MAP_STEP = 1.0 / MAP_RESOLUTION;

    /** Input port */
    private final WirelessIOPort input;

    /** Strengths of signals from each base station. null if the signal has expired */
    private final BaseStation[] stations = new BaseStation[] {
            new BaseStation(0, 0),
            new BaseStation(1, 0),
            new BaseStation(0, 1),
            new BaseStation(1, 1)
    };

    public FirefighterLocator(CompositeEntity container, String name)
            throws IllegalActionException, NameDuplicationException
    {
        super(container, name);

        input = new WirelessIOPort(this, "input", true, false);
        input.setTypeEquals(BaseType.INT);
        input.outsideChannel = new StringConstantParameter(container, "PowerLossChannel");
    }

    @Override
    public void initialize()
    {
        // Expire all signal strengths
        for (BaseStation station : stations)
            station.expireStrength();
    }

    @Override
    public void fire() throws IllegalActionException
    {
        // Update signal strengths from each input token
        while (input.hasNewToken(0)) {
            Token idToken = input.get(0);
            Token propToken = input.getProperties(0);

            int id = ((IntToken) idToken).intValue() - 1;
            double strength = ((DoubleToken) ((RecordToken) propToken).get("power")).doubleValue();

            if (id >= 0 && id < stations.length)
                stations[id].setStrength(strength);
        }

        // Estimate position on the map
        double bestY = 0.5;
        double bestX = 0.5;
        double bestScore = Double.NEGATIVE_INFINITY;

        for (double y = MAP_START; y < 1.0; y += MAP_STEP)
        {
            for (double x = MAP_START; x < 1.0; x += MAP_STEP)
            {
                // We give each map position a 'score' based on the surrounding signal strengths
                double score = 0;
                for (BaseStation station : stations)
                {
                    Double distance = station.getDistanceSq();
                    if (distance != null)
                    {
                        double pointDistance = station.getDistanceSqArbitrary(x, y);
                        score -= (distance - pointDistance) * (distance - pointDistance);
                    }
                }

                if (score > bestScore)
                {
                    bestScore = score;
                    bestY = y;
                    bestX = x;
                }
            }
        }

        _debug("Position = (" + bestX + ", " + bestY + ") score = " + bestScore);
    }

    /** Class used for base stations */
    private class BaseStation
    {
        public final double x, y;

        private Double strength;
        private Time expiryTime;

        /** Initializes a base station with its position */
        public BaseStation(double x, double y)
        {
            this.x = x;
            this.y = y;
        }

        /** Returns the signal strength or null if it has expired */
        public Double getStrength()
        {
            // Expire strength if enough time has passed
            if (expiryTime != null && !expiryTime.subtract(getDirector().getModelTime()).isPositive())
                expireStrength();

            return strength;
        }

        /** Calculates the squared distance to the base station from the result of getStrength */
        public Double getDistanceSq()
        {
            Double strength = getStrength();

            if (strength != null)
                return ONE_OVER_4_PI / strength;

            return null;
        }

        /** Sets the signal strength of a base station */
        public void setStrength(double value)
        {
            strength = value;
            expiryTime = getDirector().getModelTime().add(SIGNAL_EXPIRY_TIME);
        }

        /** Forcefully expire the signal strength */
        public void expireStrength()
        {
            strength = null;
            expiryTime = null;
        }

        /** Calculates the arbitrary squared distance between a point and this base stations */
        public double getDistanceSqArbitrary(double x, double y)
        {
            return (this.x - x) * (this.x - x) + (this.y - y) * (this.y - y);
        }
    }
}
