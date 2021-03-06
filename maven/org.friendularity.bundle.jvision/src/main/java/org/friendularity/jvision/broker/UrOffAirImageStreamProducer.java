/*
*  Copyright 2013 by The Friendularity Project (www.friendularity.org).
*
*  Licensed under the Apache License, Version 2.0 (the "License");
*  you may not use this file except in compliance with the License.
*  You may obtain a copy of the License at
*
*       http://www.apache.org/licenses/LICENSE-2.0
*
*  Unless required by applicable law or agreed to in writing, software
*  distributed under the License is distributed on an "AS IS" BASIS,
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*  See the License for the specific language governing permissions and
*  limitations under the License.
*/
package org.friendularity.jvision.broker;

import org.slf4j.LoggerFactory;

import java.awt.*;
import java.awt.image.BufferedImage;

/**
 * @author Annie
 */
class UrOffAirImageStreamProducer implements ImageStreamProducer, Runnable {

	private static UrOffAirImageStreamProducer defaultProducer = null;

	private SimpleImageStreamProducer sisp = null;
	private BufferedImage img;
	private static final int IMAGE_WIDTH = 640;
	private static final int IMAGE_HEIGHT = 480;
	private static final int BIG_DOT_R = 150;
	private static final int SMALL_DOT_R = 110;
	private static Font myFont;

	private UrOffAirImageStreamProducer() {
		img = new BufferedImage(IMAGE_WIDTH, IMAGE_HEIGHT, BufferedImage.TYPE_3BYTE_BGR);

		sisp = new SimpleImageStreamProducer("Err...");

		Thread t = new Thread(this, "UrOffAirImageStreamProducer");
		t.setDaemon(true);
		t.start();
	}


	static UrOffAirImageStreamProducer getDefaultUrOffAirImageStreamProducer() {
		if (defaultProducer == null)
			defaultProducer = new UrOffAirImageStreamProducer();

		return defaultProducer;
	}

	@Override
	public String getSourceName() {
		return sisp.getSourceName();
	}

	@Override
	public void addConsumer(ImageStreamConsumer c) {
		sisp.addConsumer(c);
	}

	@Override
	public void removeAllConsumers() {
		sisp.removeAllConsumers();
	}

	@Override
	public void run() {
		int i = 1;

		try {
			while (true) {
				Thread.sleep(100L);

				paintImage(i++, img.getGraphics());
				sisp.setConsumedImage(new ImageStreamImage(img));
				sisp.setConsumedMessage(Integer.toString(i));
			}
		} catch (InterruptedException ex) {
			LoggerFactory.getLogger(UrOffAirImageStreamProducer.class).error(ex.getMessage(), ex);
		}
	}

	private void paintImage(int i, Graphics gg) {
		Graphics2D g = (Graphics2D) gg;

		g.setColor(Color.white);
		g.fillRect(0, 0, IMAGE_WIDTH, IMAGE_HEIGHT);
		g.setColor(Color.gray);
		g.fillOval(IMAGE_WIDTH / 2 - BIG_DOT_R, IMAGE_HEIGHT / 2 - BIG_DOT_R, 2 * BIG_DOT_R, 2 * BIG_DOT_R);
		g.setColor(Color.gray.brighter());
		g.fillOval(IMAGE_WIDTH / 2 - SMALL_DOT_R, IMAGE_HEIGHT / 2 - SMALL_DOT_R, 2 * SMALL_DOT_R, 2 * SMALL_DOT_R);
		g.setColor(Color.black);
		g.drawOval(IMAGE_WIDTH / 2 - BIG_DOT_R, IMAGE_HEIGHT / 2 - BIG_DOT_R, 2 * BIG_DOT_R, 2 * BIG_DOT_R);
		g.drawOval(IMAGE_WIDTH / 2 - SMALL_DOT_R, IMAGE_HEIGHT / 2 - SMALL_DOT_R, 2 * SMALL_DOT_R, 2 * SMALL_DOT_R);
		g.drawLine(IMAGE_WIDTH / 2,
				IMAGE_HEIGHT / 2,
				(int) (IMAGE_WIDTH / 2 + IMAGE_WIDTH * Math.sin((i % 10) * Math.PI * 2.0f)),
				(int) (IMAGE_HEIGHT / 2 + IMAGE_WIDTH * Math.cos((i % 10) * Math.PI * 2.0f)));
		String s = Integer.toString(i);
		if (myFont == null) {
			myFont = new Font("Arial", Font.BOLD, BIG_DOT_R);
		}
		g.setFont(myFont);
		g.drawString(s, IMAGE_WIDTH / 2 - g.getFontMetrics().stringWidth(s) / 2, IMAGE_HEIGHT / 2 -
				(g.getFontMetrics().getAscent() + g.getFontMetrics().getDescent()) / 2);
	}

	@Override
	public void removeConsumer(ImageStreamConsumer c) {
		sisp.removeConsumer(c);
	}

}
