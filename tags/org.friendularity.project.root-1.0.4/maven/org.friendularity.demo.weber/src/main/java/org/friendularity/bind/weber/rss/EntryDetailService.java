/*
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.bind.weber.rss;

import org.friendularity.weber.services.GenRespWithConf;

/**
 *
 * @author Matt Stevenson
 */
public class EntryDetailService extends EntryService {

    public EntryDetailService(FeedSelector parent){
        super(parent);
    }
    
    @Override public String getServiceName(){
        return "RSS_ENTRY_DETAILS";
    }

    @Override public GenRespWithConf getEntryResponse(int i){
        String d = "" + (i+1) + ", " + myFeed.get(i).getDetail();
        return new GenRespWithConf(d, 10);
    }
}
