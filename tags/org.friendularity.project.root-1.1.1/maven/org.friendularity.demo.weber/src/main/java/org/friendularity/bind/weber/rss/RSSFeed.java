/*
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.bind.weber.rss;

import com.sun.syndication.feed.synd.*;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.io.SyndFeedInput;
import com.sun.syndication.io.XmlReader;
import java.net.MalformedURLException;


import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/**
 *
 * @author Matt Stevenson
 */
public class RSSFeed {
    private static Logger theLogger = Logger.getLogger(RSSFeed.class.getName());
    private URL myUrl;
    private String myName;
    private List<RSSEntry>  myEntries;

    public String getName(){
        return myName;
    }

    public RSSEntry get(int i){
        return myEntries.get(i);
    }

    public RSSFeed(String name, String url) throws IllegalArgumentException {
        try{
            myName = name;
            myUrl = new URL(url);
        }catch(MalformedURLException ex){
            throw new IllegalArgumentException("RSSFeed initialized with bad url.", ex);
        }
    }

    public void reloadFeed(){
        myEntries = new ArrayList();
        SyndFeedInput input = new SyndFeedInput();
        SyndFeed sf;
        try{
            sf = input.build(new XmlReader(myUrl));
        }catch(Exception ex){
            theLogger.warning("There was an error loading the feed at: " + myUrl);
            ex.printStackTrace();
            return;
        }
        int count = 0;
        for(SyndEntry se : (List<SyndEntry>)sf.getEntries()){
            if(se == null) continue;
            myEntries.add(new RSSEntry(se));
            count++;
            if(count == 20){
                break;
            }
        }
    }

    public int count(){
        return Math.min(myEntries.size(), 20);
    }
}
