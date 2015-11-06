package mote;

import com.ibm.saguaro.system.Device;
import com.ibm.saguaro.system.LED;
import com.ibm.saguaro.system.Radio;
import com.ibm.saguaro.system.Time;
import com.ibm.saguaro.system.Timer;
import com.ibm.saguaro.system.TimerEvent;
import com.ibm.saguaro.system.Util;

public class RadioSender {
	/** LED Colours */
	private static final byte LED_YELLOW = (byte) 0;

	/** Transmit Delay */
	private static final long XMIT_DELAY = Time.toTickSpan(Time.SECONDS, 2);

	/** Transmit Buffer */
	private static final byte[] xmit = new byte[8];

	/** Periodic Sending Timer */
	private static final Timer sendTimer = new Timer();

	/** Radio reference */
	private static Radio radio = new Radio();

	static {
		// Open the default radio
		radio.open(Radio.DID, null, 0, 0);

		// Set the PAN ID to 0x42 and the short address to 0x31
		radio.setPanId(0x42, true);
		radio.setShortAddr(0x31);

		// Set radio channel
		radio.setChannel((byte) 1);

		// Prepare beacon frame with source addressing
		xmit[0] = Radio.FCF_BEACON;
		xmit[1] = Radio.FCA_SRC_SADDR;
		Util.set16le(xmit, 3, 0x42);
		Util.set16le(xmit, 5, 0x31);

		// Setup timer to send the frames
		sendTimer.setCallback(new TimerEvent(null) {
			public void invoke(byte param, long time) {
				sendTimerFire(param, time);
			}
		});

		// Start the timer
		sendTimer.setAlarmBySpan(XMIT_DELAY);
	}

	private static void sendTimerFire(byte param, long time) {
		// Blink yellow LED
		LED.setState(LED_YELLOW, (byte) (1 - LED.getState(LED_YELLOW)));

		// Send packet
		radio.transmit(Device.ASAP | Radio.TXMODE_CCA, xmit, 0, 8, 0);
		xmit[7]++;

		// Restart timer
		sendTimer.setAlarmBySpan(XMIT_DELAY);
	}
}
