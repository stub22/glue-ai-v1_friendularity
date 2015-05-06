/*
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.bind.weber.rss;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;

/**
 *
 * @author Matt Stevenson
 */
public class FeedRepository {
    private RSSConfig myConfig;
    private List<RSSFeed> myFeeds;

    public FeedRepository(RSSConfig conf){
        myConfig = conf;
        loadFeeds();
    }

    public void loadFeeds(){
        myFeeds = new ArrayList();
        LinkedHashMap<String,String> urls = myConfig.getFeedURLMap();
        for(Entry<String,String> e : urls.entrySet()){
            myFeeds.add(new RSSFeed(e.getKey(), e.getValue()));
        }
    }

    public RSSFeed get(int i){
        return myFeeds.get(i);
    }

    public int size(){
        return myFeeds.size();
    }

    public List<RSSFeed> all(){
        return myFeeds;
    }
}