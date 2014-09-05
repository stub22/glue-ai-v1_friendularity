/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bundle.macro.behavior;

import javax.swing.ImageIcon;

/**
 * ImageIcon collection interface.
 * 
 * @author eadsjr
 */
public interface ImageIconMap {
    
    /**
     * The ID representing all ImageIconMap instances.
     */
    public final static String PROP_IMAGE_ICON_MAP_ID = "ImageIconMapID";
    
    public void putIconImage(String imageIconName, ImageIcon imageIcon);

    public ImageIcon getImageIcon(String imageIconName);

    public void removeImageIcon(String imageIconName);
}