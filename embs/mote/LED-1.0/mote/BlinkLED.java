package mote;

import com.ibm.saguaro.logger.Logger;
import com.ibm.saguaro.system.LED;
import com.ibm.saguaro.system.Mote;
import com.ibm.saguaro.system.Time;
import com.ibm.saguaro.system.Timer;
import com.ibm.saguaro.system.TimerEvent;
import com.ibm.saguaro.system.csr;

public class BlinkLED
{
	// timer interval in ticks
	private static final long INTERVAL = Time.toTickSpan(Time.SECONDS, 2);

	// timer
	private static Timer timer;

	static
	{
		// instantiante a new timer
		timer = new Timer();

		// set the callback for the timer
		// i.e. when the timer ticks, it should call the "fire()"
		// method of this class

		timer.setCallback(new TimerEvent(null){
				public void invoke(byte param, long time){
					BlinkLED.fire(param, time);
				}
			});

		// set a new alarm in 2 seconds from now
		timer.setAlarmBySpan(INTERVAL);
	}

	 /**
	 * Callback for the timer
	 */
	private static void fire (byte param, long time) {

		// Check and toggle the state of the first LED of the mote (index 0)
		LED.setState((byte) 0, (byte) (1 - LED.getState((byte) 0)));

		// Setup a new timer alarm
		timer.setAlarmBySpan(INTERVAL);

		// Log something
		Logger.appendString(csr.s2b("LED Blinked"));
		Logger.flush(Mote.INFO);
	}
}
