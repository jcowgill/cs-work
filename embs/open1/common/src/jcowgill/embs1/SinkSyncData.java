package jcowgill.embs1;

/**
 * Stores the synchronization data known about a particular sink
 */
public class SinkSyncData
{
	/** Maximum value of N */
	public static final int MAX_N = 10;

	/** Lower bound for Δt */
	public static final long DELTA_T_LOWER = 250;

	/** Upper bound for Δt */
	public static final long DELTA_T_UPPER = 1500;

	/** Period at the start of the rx phase to not send packets */
	private static final long RX_PHASE_LEADIN = 20;

	/** Period at the end of the rx phase to not send packets */
	private static final long RX_PHASE_LEADOUT = 50;

	/** Subtract this from the time we think an interesting beacon will appear */
	private static final long INTERESTING_BEACON_OFFSET = 500;

	/** The best value of n we have so far */
	private int bestN;

	/**
	 * The best value of Δt (the timestep) in milliseconds
	 *
	 * If this variable is 0, we know absolutely nothing about Δt.
	 */
	private long bestDeltaT;

	/** If true, we are very sure the values are correct */
	private boolean goodN, goodT;

	/**
	 * The value of n in the previous beacon received
	 *
	 * If this variable is 0, then we have never received any beacons.
	 */
	private int prevN;

	/** The absolute time of the previous beacon received in milliseconds */
	private long prevT;

	/** Initializes a SinkSyncData object */
	public SinkSyncData()
	{
		reset();
	}

	/** Resets the internal state of this object */
	public void reset()
	{
		bestN = 1;
		bestDeltaT = 0;
		goodN = false;
		goodT = false;
		prevN = 0;
		prevT = 0;
	}

	/** Returns true if a good value of n has been calculated */
	public boolean hasGoodN()
	{
		return goodN;
	}

	/** Returns true if a good value of Δt has been calculated */
	public boolean hasGoodDeltaT()
	{
		return goodT;
	}

	/**
	 * Called when a beacon is received to update the estimates for N and Δt
	 *
	 * @param absoluteTime the absolute time the beacon was received in milliseconds
	 * @param n the value of n in the beacon received
	 */
	public void receiveBeacon(long absoluteTime, int n)
	{
		// bestN must be at least the value of n we just received
		if (n > bestN)
		{
			bestN = n;

			// If n == MAX_N, we can be sure this is the correct value
			if (n >= MAX_N)
				goodN = true;
		}

		// We can only do Δt calculations if this isn't the first beacon
		//  Also ignore if deltaT < 0 (possible time wraparound)
		int deltaN = prevN - n;
		long deltaT = absoluteTime - prevT;

		if (prevN != 0 && deltaT > 0)
		{
			// Try and work out if this beacon is from the same sync iteration
			if (deltaN > 0 && deltaT < (11 + bestN) * DELTA_T_LOWER)
			{
				// Yes this beacon is definitely from the same iteration
				bestDeltaT = clampDeltaT(deltaT / deltaN);
				goodT = true;
			}
			else if (!goodT)
			{
				// Above is the only way we can be sure we have the right Δt value
				//  Since these are all estimates, don't overwrite a good Δt value

				if (deltaN > 0 && deltaT < deltaN * DELTA_T_UPPER)
				{
					// The beacon is likely to be from the same iteration (but we're not sure)
					bestDeltaT = clampDeltaT(deltaT / deltaN);
				}
				else
				{
					// Beacons have definitely wrapped at least once
					//  We guess at one iteration if it looks sane
					long guess = roundingIntegerDivision(deltaT, (11 + bestN - deltaN));

					if (isValidDeltaT(guess))
						bestDeltaT = guess;
				}
			}

			// If we know the value of Δt and we have two beacons from different
			//  sync iterations, we can infer the value of n.
			if (goodT)
			{
				int deltaBeacons = (int) (roundingIntegerDivision(deltaT, bestDeltaT) - deltaN);

				// Test if beacons are from different sync iterations
				if (11 + bestN <= deltaBeacons && deltaBeacons <= 11 + MAX_N)
				{
					bestN = deltaBeacons - 11;
					goodN = true;
				}
			}
		}

		// Update previous beacon values
		prevN = n;
		prevT = absoluteTime;
	}

	/** Returns the length of one iteration of the protocol or 0 if we don't know */
	public long getIterationLength()
	{
		if (bestDeltaT == 0 || !goodN)
			return 0;

		return (11 + bestN) * bestDeltaT;
	}

	/**
	 * Calculates the start of the reception phase
	 *
	 * This is the same as {@link #calcReceptionPhase(long, boolean)} but allows
	 * future sync iterations only if we have a good value of n.
	 *
	 * @param time the current absolute time in milliseconds
	 * @return the start of the next reception phase in absolute milliseconds
	 * @see #calcReceptionPhase(long, boolean)
	 */
	public long calcReceptionPhase(long time)
	{
		return calcReceptionPhase(time, goodN);
	}

	/**
	 * Calculates the start of the reception phase
	 *
	 * This method uses the current knowledge of the sink (guesses of n, Δt,
	 * times of previous beacons) to calculate the start of either the reception phase
	 * the sink is currently in, or the next reception phase.
	 *
	 * It returns 0 if there is not enough information at all to calculate it. It may
	 * return a result in the past if we are in a reception phase that's already started.
	 *
	 * Other functions give the caller an indication of the "quality" of the result
	 * returned by this method.
	 *
	 * If allowFutureSync is true, the method can return times calculated from future
	 * sync iterations. This will obviously be inaccurate if we have a bad value of n.
	 *
	 * @param time the current absolute time in milliseconds
	 * @param allowFutureSync if true, allow times from future sync iterations to be returned
	 * @return the start of the next reception phase in absolute milliseconds
	 */
	public long calcReceptionPhase(long time, boolean allowFutureSync)
	{
		long result = 0;

		// We can only calculate if we know some value for Δt
		if (prevN != 0 && bestDeltaT != 0)
		{
			// Calculate the time of the phase immediately after the previous beacon
			long immediatePhase = prevT + prevN * bestDeltaT + RX_PHASE_LEADIN;
			long phaseLength = bestDeltaT - (RX_PHASE_LEADIN + RX_PHASE_LEADOUT);
			long iterationLength = (11 + bestN) * bestDeltaT;

			if (immediatePhase + phaseLength > time)
			{
				// We are either in a reception phase now, or the next phase is at the end
				//  of this iteration
				result = immediatePhase;
			}
			else if (allowFutureSync)
			{
				// The time was in the past so try and predict the time in the future sync iteration
				result = immediatePhase + nextMultiple(iterationLength, (time - immediatePhase - phaseLength));
			}
		}

		return result;
	}

	/**
	 * Calculates the earliest time of the next interesting beacon
	 *
	 * This method is intended to prevent pointless use of channels if we
	 * know there will not be a useful beacon in the timeframe.
	 *
	 * @param absoluteTime the current absolute time
	 * @return the absolute time of the beacon
	 */
	public long nextInterestingBeacon(long absoluteTime)
	{
		// This is only used in the calculation of n. If we know n, no beacons
		// are interesting. If we don't know Δt then we have no idea when the
		// beacons are.

		if (goodN && goodT)
			return 0;

		if (!goodT)
			return absoluteTime;

		// Try to calculate roughly when the first beacon will arrive
		long firstBeacon = prevT + (11 + prevN) * bestDeltaT - INTERESTING_BEACON_OFFSET;

		if (absoluteTime >= firstBeacon)
			return absoluteTime;

		return firstBeacon;
	}

	/**
	 * Clamps a value of deltaT so it is within the valid range
	 *
	 * @param deltaT value to clamp
	 * @return clamped value
	 */
	private static long clampDeltaT(long deltaT)
	{
		if (deltaT > DELTA_T_UPPER)
			return DELTA_T_UPPER;

		if (deltaT < DELTA_T_LOWER)
			return DELTA_T_LOWER;

		return deltaT;
	}

	/**
	 * Tests if a deltaT value is within the valid range
	 *
	 * @param deltaT value to test
	 * @return true if it is in range
	 */
	private static boolean isValidDeltaT(long deltaT)
	{
		return deltaT >= DELTA_T_LOWER && deltaT <= DELTA_T_UPPER;
	}

	/**
	 * Divides a by b, but rounds the result instead of flooring it
	 *
	 * @param a the dividend
	 * @param b the divisor
	 * @return the rounded division result
	 */
	private static long roundingIntegerDivision(long a, long b)
	{
		return (a + b / 2) / b;
	}

	/**
	 * Finds the next multiple of 'multiplier' greater than or equal to n
	 *
	 * @param multiplier value to multiply by integers to get a value near n
	 * @param n value the multiple must be greater than
	 * @return the next multiple
	 */
	private static long nextMultiple(long multiplier, long n)
	{
		return (n + multiplier - 1) / multiplier * multiplier;
	}
}
