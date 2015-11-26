package embs;

import com.ibm.saguaro.system.*;
import com.ibm.saguaro.logger.*;

/**
 * Mote sink
 *
 * Yellow LED = On during the reception phase
 * Green LED  = Blinked when a good packet is received
 * Red LED    = Blinked when a bad packet is received
 */
public class Sink {

	/* LED Colours */
	private static final byte LED_YELLOW = (byte) 0;
	private static final byte LED_GREEN = (byte) 1;
	private static final byte LED_RED = (byte) 2;

	/* LED States */
	private static final byte LED_OFF = (byte) 0;
	private static final byte LED_ON = (byte) 1;

	private static final int PAN_ID_OFFSET = 0x11;

	private final Timer tsend = new Timer();
	private final Timer tstart = new Timer();
	private final Timer tendPeriod = new Timer();
	private final Radio radio = new Radio();

	private final byte[] xmit = new byte[12];
	private final long wait;

	// settings for sink A
	private final int n;
	private final byte channel;

	// Mutable state
	private int nc;
	private boolean inReceptionPhase;
	private long nextEventTime;

	public Sink() {
		// Default settings
		this(0, 8, 600);
	}

	public Sink(int inChannel, int inN, int t) {
		// Store arguments
		this.channel = (byte) inChannel;
		this.n = inN;

		// Open the default radio
		radio.open(Radio.DID, null, 0, 0);

		// Set channel
		radio.setChannel(channel);

		// Set the PAN ID and the short address
		int panid = PAN_ID_OFFSET + channel;
		radio.setPanId(panid, false);
		radio.setShortAddr(panid);

		// Prepare beacon frame with source and destination addressing
		xmit[0] = Radio.FCF_BEACON;
		xmit[1] = Radio.FCA_SRC_SADDR|Radio.FCA_DST_SADDR;
		Util.set16le(xmit, 3, panid); // destination PAN address
		Util.set16le(xmit, 5, 0xFFFF); // broadcast address
		Util.set16le(xmit, 7, panid); // own PAN address
		Util.set16le(xmit, 9, panid); // own short address

		xmit[11] = (byte)n;

		// register delegate for received frames
		radio.setRxHandler(new DevCallback(this){
				@Override
				public int invoke (int flags, byte[] data, int len, int info, long time) {
					return ((Sink) obj).onReceive(flags, data, len, info, time);
				}
			});

		// Setup a periodic timer callback for beacon transmissions
		tsend.setCallback(new TimerEvent(this){
				@Override
				public void invoke(byte param, long time){
					((Sink) obj).periodicSend(param, time);
				}
			});

		// Setup a periodic timer callback to restart the protocol
		tstart.setCallback(new TimerEvent(this){
				@Override
				public void invoke(byte param, long time){
					((Sink) obj).restart(param, time);
				}
			});

		// Setup a periodic timer callback to end the reception period
		tendPeriod.setCallback(new TimerEvent(this){
				@Override
				public void invoke(byte param, long time){
					((Sink) obj).endReceptionPeriod(param, time);
				}
			});

		// Convert the periodic delay from ms to platform ticks
		wait = Time.toTickSpan(Time.MILLISECS, t);

		// Start listening
		radio.startRx(Device.ASAP | Device.RX4EVER, 0, 0);

		// Start the protocol now
		nextEventTime = Time.currentTicks();
		restart((byte) 0, 0);
	}

	// Called when a frame is received or at the end of a reception period
	int onReceive (int flags, byte[] data, int len, int info, long time) {
		if (data == null) {
			// Restart receiver
			radio.startRx(Device.ASAP | Device.RX4EVER, 0, 0);
		}
		else
		{
			// blink relevant led
			if (inReceptionPhase)
				toggleLED(LED_GREEN);
			else
				toggleLED(LED_RED);

			Logger.appendByte(data[11]);
			Logger.flush(Mote.WARN);
		}

		return 0;
	}

	// Called on a timer alarm
	void periodicSend(byte param, long time) {
		if(nc>0){
			// transmit a beacon
			radio.transmit(Device.ASAP|Radio.TXMODE_POWER_MAX, xmit, 0, 12, 0);
			// program new alarm
			nextEventTime += wait;
			tsend.setAlarmTime(nextEventTime);

			nc--;
			xmit[11]--;
		}
		else{
			// Start reception phase and timer
			inReceptionPhase = true;
			LED.setState(LED_YELLOW, LED_ON);

			nextEventTime += wait;
			tendPeriod.setAlarmTime(nextEventTime);
		}
	}

	// Called on a timer alarm, starts the protocol
	void restart(byte param, long time) {
		nc=n;
		xmit[11]=(byte)n;
		periodicSend((byte) 0, 0);
	}

	// Called on a timer alarm, ends the reception period
	void endReceptionPeriod(byte param, long time) {
		inReceptionPhase = false;
		LED.setState(LED_YELLOW, LED_OFF);

		// Set alarm to restart the protocol
		nextEventTime += 10 * wait;
		tstart.setAlarmTime(nextEventTime);
	}

	/** Toggles the given LED */
	private static void toggleLED(byte led)
	{
		LED.setState(led, (byte) (1 - LED.getState(led)));
	}
}
