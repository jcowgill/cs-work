package mote;

import com.ibm.saguaro.logger.Logger;
import com.ibm.saguaro.system.*;

public class RadioReceiver {
	/** LED Colours */
	private static final byte LED_GREEN = (byte) 1;
	private static final byte LED_RED = (byte) 2;

	/** Time the receiver is on for */
	private static final long RXON_DELAY = Time.toTickSpan(Time.SECONDS, 15);

	/** Time the receiver is off for */
	private static final long RXOFF_DELAY = Time.toTickSpan(Time.SECONDS, 5);

	/** Periodic Timer */
	private static final Timer wakeupTimer = new Timer();

	/** Radio reference */
	private static Radio radio = new Radio();

	static {
		// Open the default radio
		radio.open(Radio.DID, null, 0, 0);

		// Set the PAN ID to 0x42 and the short address to 0x30
		radio.setPanId(0x42, true);
		radio.setShortAddr(0x30);

		// Setup the radio handler on channel 1
		radio.setChannel((byte) 1);
		radio.setRxHandler(new DevCallback(null) {
			public int invoke(int flags, byte[] data, int len, int info, long time) {
				return receivePacket(flags, data, len, info, time);
			}
		});

		// Re-enable the radio when the timer expires
		wakeupTimer.setCallback(new TimerEvent(null) {
			public void invoke(byte param, long time) {
				wakeupTimerFire(param, time);
			}
		});

		// Immediately fire the wakeup timer to enable the radio
		wakeupTimer.setAlarmBySpan(0);
	}

	private static int receivePacket(int flags, byte[] data, int len, int info, long time) {
		// If the radio has stopped, enable wakeup timer
		if (data == null) {
			LED.setState(LED_GREEN, (byte) 0);
			wakeupTimer.setAlarmBySpan(RXOFF_DELAY);
			return 0;
		}

		// Blink red led
		LED.setState(LED_RED, (byte) (1 - LED.getState(LED_RED)));

		// Log packet received
		if (len == 8) {
			Logger.appendString(csr.s2b("good packet received, payload = "));
			Logger.appendByte(data[7]);
		} else {
			Logger.appendString(csr.s2b("bad packet received"));
		}

		Logger.flush(Mote.INFO);
		return 0;
	}

	private static void wakeupTimerFire(byte param, long time) {
		// Start radio and turn on green LED
		radio.startRx(Device.ASAP, 0, Time.currentTicks() + RXON_DELAY);
		LED.setState(LED_GREEN, (byte) 1);
	}
}
