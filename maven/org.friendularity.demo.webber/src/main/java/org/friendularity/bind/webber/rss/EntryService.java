/*
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.bind.webber.rss;

import org.friendularity.bind.webber.jmx.JMXInterface;
import org.friendularity.webber.utils.Utils;
import org.friendularity.webber.services.GenRespWithConf;
import org.friendularity.webber.services.INexusService;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Matt Stevenson
 */
public class EntryService implements INexusService{
    protected static int silence = 600; //msec
    protected RSSFeed myFeed;
    private FeedSelector myParent;

    public EntryService(FeedSelector parent){
        myParent = parent;
    }

    public RSSFeed getFeed(){
        return myFeed;
    }

    public void setFeed(RSSFeed feed){
        myFeed = feed;
    }

    public GenRespWithConf getResponse(String input) {
        if(input.equalsIgnoreCase("quit")){
            JMXInterface.sendThoughtToConvoid("T_CLEAR_EXPECTATIONS", 1.0);
        }else if(input.equalsIgnoreCase("feeds")){
            return myParent.getAllEntries();
        }else if(input.equalsIgnoreCase("all")){
            return getAllEntries();
        }
        try{
            int i = Integer.parseInt(input) - 1;
            return getEntryResponse(i);
        }catch(NumberFormatException ex){
            return null;
        }
    }

    public GenRespWithConf getAllEntries() {
        String resp = "";
        for(int i=0; i<myFeed.count(); i++){
            String ent = getEntryResponse(i).getResponse();
            ent = Utils.plaintext(ent);
            resp += ent + Utils.silence(silence);
        }
        JMXInterface.sendThoughtToConvoid("T_EXPECT-RSS_ENTRY_DETAILS", 1000.0);
        JMXInterface.sendThoughtToConvoid("T_HANDLE_UNEXPECTED", 1000.0);
        return new GenRespWithConf(resp, 10);
    }

    public GenRespWithConf getEntryResponse(int i) {
        String ent = "" + (i+1) + ", " + myFeed.get(i).getTitle();
        return new GenRespWithConf(ent, 10);
    }

    public String getServiceName() {
        return "RSS_ENTRY";
    }

    public List<INexusService> getChildServices() {
        return new ArrayList();
    }

    public boolean ignoreBatchRequest() {
        return true;
    }

}
