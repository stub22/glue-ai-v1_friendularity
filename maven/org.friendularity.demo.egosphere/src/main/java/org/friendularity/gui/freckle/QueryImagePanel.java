/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.gui.freckle;

import org.friendularity.app.jmxwrap.SignalStation;
import java.awt.Graphics;
import java.awt.Image;
import org.cogchar.sight.api.obs.PortableImage;

/**
 *
 * @author Stu Baurmann
 */
public class QueryImagePanel extends javax.swing.JPanel {

	private Image myImage;
	public QueryImagePanel() {
		setBackground(new java.awt.Color(0, 153, 51));
        setBorder(javax.swing.BorderFactory.createMatteBorder(5, 5, 5, 5, new java.awt.Color(0, 102, 0)));
        setMaximumSize(new java.awt.Dimension(320, 240));
        setMinimumSize(new java.awt.Dimension(320, 240));
        setPreferredSize(new java.awt.Dimension(320, 240));

		SignalStation ss = SignalStation.getSignalStation();
		ss.myDetectedImagePanel = this;
	}
	public void setImage(Image i) {
		myImage = i;
	}
	public void setPortableImage (PortableImage pimg) {
		Image i = pimg.fetchJavaImage(this);
		setImage(i);
	}
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		Image img = myImage;
		if (img != null) {
            try {
                g.drawImage(img,  0, 0, img.getWidth(null), img.getHeight(null),
                            0, 0, img.getWidth(null), img.getHeight(null), null);
				/*
				for (IAnnotatingObserver iao : m_annotaters) {
					iao.Annotate(g);
				}
				*/
            } catch (Exception e) {
				System.out.println("RawFrameProcessor caught exception: " + e);
				e.printStackTrace();
            }
		}
	}
}
