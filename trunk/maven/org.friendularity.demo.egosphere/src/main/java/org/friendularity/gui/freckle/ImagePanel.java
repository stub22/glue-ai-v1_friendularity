package org.friendularity.gui.freckle;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;

import javax.swing.JComponent;
import javax.swing.TransferHandler;
import javax.swing.event.MouseInputListener;
import org.cogchar.sight.vision.OpenCVImage;
import org.cogchar.sight.vision.PortableImage;

public class ImagePanel extends JComponent //java.awt.Canvas 
    implements MouseInputListener {

    private BufferedImage m_image;
    private Rectangle[] m_boxes;
    private MouseEvent firstMouseEvent = null;

    public ImagePanel() {
        addMouseMotionListener(this);
        addMouseListener(this);
    }

    public void setImage(BufferedImage image)
    {
        m_image = image;
        m_boxes = null;
    }
    
    public BufferedImage getImage() {
    	return m_image;
    }

    public void setBoxes(Rectangle[] boxes)
    {
        m_boxes = boxes;
    }
    
    public Rectangle[] getBoxes() {
    	return m_boxes;
    }
    
    public void update(Graphics g) {
        paint(g);
    }
    
    protected void paintComponent(Graphics graphics) {
    	Graphics g = graphics.create();

		g.setColor(Color.LIGHT_GRAY);
		g.fillRect(0,0,getHeight(),getWidth());

    	if ( m_image != null ) {
    		g.drawImage(m_image,  0, m_image.getHeight(null), m_image.getWidth(null), 0,
                    0, 0, m_image.getWidth(null), m_image.getHeight(null), null);
    		if ( m_boxes != null ) {
    			for ( int i = 0; i < m_boxes.length; i++ ) {
    				g.drawRect(m_boxes[i].x, m_boxes[i].y, m_boxes[i].width, m_boxes[i].height);
    			}
    		}
    	}
    	
    	g.dispose();
    }
	
	public OpenCVImage getQueryOpenCVImage() {
		BufferedImage queryBI  = getImage();
		// Is
		boolean flipVertical = true;
		PortableImage pimg = new PortableImage(queryBI, flipVertical);
		OpenCVImage ocvi = pimg.fetchOpenCVImage();
		return ocvi;
	}

    // allow drag and drop
    public void mousePressed(MouseEvent e) {
        //Don't bother to drag if there is no image.
        if (m_boxes == null) return;

        firstMouseEvent = e;
        e.consume();
    }

    public void mouseDragged(MouseEvent e) {
        //Don't bother to drag if the component displays no image.
        if (m_boxes == null) {
        	m_dragImage = m_image;
        	return;
        }

        if (firstMouseEvent != null) {
            e.consume();

            int dx = Math.abs(e.getX() - firstMouseEvent.getX());
            int dy = Math.abs(e.getY() - firstMouseEvent.getY());
            //Arbitrarily define a 5-pixel shift as the
            //official beginning of a drag.
            if (dx > 5 || dy > 5) {
                //This is a drag, not a click.
                JComponent c = (JComponent)e.getSource();
                TransferHandler handler = c.getTransferHandler();
                //Tell the transfer handler to initiate the drag.

                for (Rectangle box : m_boxes) {
                    if ( ( e.getX() > box.x && e.getX() < (box.x + box.width) )
                    		&& ( e.getY() > box.y && e.getY() < (box.y + box.height) ) )
                    // The coordinate origin for this function is bottom left	
                    m_dragImage = m_image.getSubimage(box.x, m_image.getHeight()- box.y - box.height, box.width, box.height);
                }

                handler.exportAsDrag(c, firstMouseEvent, TransferHandler.COPY);
                firstMouseEvent = null;
            }
        }
    }

    public void mouseReleased(MouseEvent e) {
        firstMouseEvent = null;
    }

    public void mouseMoved(MouseEvent e) { }
    public void mouseClicked(MouseEvent e) { }
    public void mouseEntered(MouseEvent e) { }
    public void mouseExited(MouseEvent e) { }

    public Image dragImage() {
        return ServerVizUI.toBufferedImage(m_dragImage, true);
    }
    private Image m_dragImage;
}
