/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.test.camera.r50;

import java.awt.Graphics;
import java.awt.Image;
import javax.swing.JFrame;
import javax.swing.JPanel;
import org.jflux.api.core.Listener;
import org.mechio.api.vision.ImageEvent;
import org.mechio.impl.vision.PortableImageUtils;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */
public class ImageMonitor extends JPanel implements Listener<ImageEvent> {
    private Image myImage;
    
    public ImageMonitor() {
        myImage = null;
    }

    @Override
    public void handleEvent(ImageEvent t) {
        myImage = PortableImageUtils.unpackImage(t);
        repaint();
        
        try {
            ((JFrame)getTopLevelAncestor()).setSize(t.getWidth(), t.getHeight());
        } catch(Exception e) {
            
        }
    }
    
    @Override
    public void paint(Graphics g) {
        if(myImage != null){
            g.drawImage(myImage, 0, 0, getWidth(), getHeight(), null);
        }
    }
}
