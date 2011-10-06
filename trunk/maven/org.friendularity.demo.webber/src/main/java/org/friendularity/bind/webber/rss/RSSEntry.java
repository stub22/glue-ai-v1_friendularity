/*
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.bind.webber.rss;

import org.friendularity.webber.utils.Utils;
import com.sun.syndication.feed.synd.SyndEntry;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 *
 * @author Matt Stevenson
 */
public class RSSEntry {
    private SyndEntry myEntry;
    private static DateFormat dateFormat=new SimpleDateFormat("MMMM d 'at' h:mm a");

    public RSSEntry(SyndEntry entry){
        if(entry == null){
            throw new IllegalArgumentException("RSSEntry cannot be initialize with a null entry.");
        }
        myEntry = entry;
    }

    public String getTitle(){
        return myEntry.getTitle();
    }

    public String getDetail(){
        String detail = myEntry.getDescription().getValue();
        detail = Utils.stripTags(detail);
        detail = Utils.plaintext(detail).trim();
        detail += Utils.silence(500);
        Date pub = myEntry.getPublishedDate();
        detail += "Published, " + dateFormat.format(pub);
        return detail;
    }
}
