/*
 * FaceTrackObserver.java
 * 
 * Created on Jul 30, 2007, 12:24:17 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.friendularity.sight.vision;

import org.cogchar.zzz.oldboot.ThreadAwareObject;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author josh
 */
public class FaceTrackObserver extends ThreadAwareObject
  implements ITrackObserver, IAnnotatingObserver
{
	private int[][] m_tracks;

    public FaceTrackObserver() {
		super();
    }

    public void ProcessFrame(int[][] trackData) {
		blessCurrentThread();
        m_tracks = trackData;
    }
    public List<List<Point>>	getTrackingHistoryListList() {
		ArrayList	trackPointLists = new ArrayList<List<Point>>();
        if ( m_tracks != null ) {
			// For each face
            for (int i = 0; i < m_tracks.length; i++) {
				int[] points = m_tracks[i];
				ArrayList<Point> trackHistory = new ArrayList<Point>();
				// For each x,y point in the track history, which runs oldest to newest
                for (int j = 0; j < points.length; j+=2) {
					Point p = new Point(points[j], points[j+1]);
					trackHistory.add(p);
				}
				trackPointLists.add(trackHistory);
            }
        }
		return trackPointLists;
	}
    public void Annotate(Graphics g)
    {
        if ( m_tracks != null )
        {
            int[][] tmp = m_tracks;
            g.setColor(Color.white);
			// For each face
            for (int i = 0; i < tmp.length; i++)
            {
				// For each x,y point in the track history, which runs oldest to newest
                for (int j = 0; j < tmp[i].length; j+=2)
                {
                    g.drawOval(tmp[i][j], tmp[i][j+1], 2*j, 2*j);
                    if ( j > 0 )
                    {
                        g.drawLine(tmp[i][j-2], tmp[i][j-1],
                                   tmp[i][j], tmp[i][j+1]);
                    }
                }
            }
        }
    }
    
    
}
