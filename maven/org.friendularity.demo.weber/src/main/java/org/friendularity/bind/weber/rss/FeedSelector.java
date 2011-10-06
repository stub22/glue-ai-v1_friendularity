/*
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.bind.weber.rss;

import org.friendularity.bind.weber.jmx.JMXInterface;
import org.friendularity.weber.services.GenRespWithConf;
import org.friendularity.weber.services.INexusService;
import org.friendularity.weber.utils.Utils;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Matt Stevenson
 */
public class FeedSelector implements INexusService {
    private static int silence = 600;
    FeedRepository myRepo;
    List<INexusService> myFeedServices;
    EntryService myTitleService;
    EntryDetailService myDetailService;

    public FeedSelector(RSSConfig conf){
        myRepo = new FeedRepository(conf);
        myFeedServices = new ArrayList();
        myTitleService = new EntryService(this);
        myDetailService = new EntryDetailService(this);
        myFeedServices.add(myTitleService);
        myFeedServices.add(myDetailService);
    }
    
    public GenRespWithConf getResponse(String input) {
        if(input.equalsIgnoreCase("quit")){
            JMXInterface.sendThoughtToConvoid("T_CLEAR_EXPECTATIONS", 1.0);
        }
        if(input.equalsIgnoreCase("all")){
            return getAllEntries();
        }
        try{
            //The number we get in 1-based, our array is 0-based. So subtract 1.
            int i = Integer.parseInt(input) - 1;
            return getResponse(i);
        }catch(NumberFormatException ex){
            return null;
        }
    }

    public GenRespWithConf getAllEntries() {
        JMXInterface.sendThoughtToConvoid("T_EXPECT-RSS_FEED", 1000.0);
        JMXInterface.sendThoughtToConvoid("T_HANDLE_UNEXPECTED", 1000.0);
        String resp = "";
        for(int i=0; i<myRepo.size(); i++){
            String name = "" + (i+1) + ", " + myRepo.get(i).getName();
            resp += name + Utils.silence(silence);
        }
        return new GenRespWithConf(resp, 10);
    }

    public GenRespWithConf getResponse(int i) {
        RSSFeed selected = myRepo.get(i);
        selected.reloadFeed();
        setFeed(selected);
        JMXInterface.sendThoughtToConvoid("T_EXPECT-RSS_ENTRY_DETAILS", 1000.0);
        JMXInterface.sendThoughtToConvoid("T_HANDLE_UNEXPECTED", 1000.0);
        GenRespWithConf resp = myTitleService.getAllEntries();
        String r = "Responses for " + selected.getName() + Utils.silence(silence)
                + resp.getResponse();
        resp.setResponse(r);
        return resp;
    }

    private void setFeed(RSSFeed feed){
        for(INexusService s : myFeedServices){
            ((EntryService)s).setFeed(feed);
        }
    }

    public String getServiceName() {
        return "RSS_FEED";
    }

    public List<INexusService> getChildServices() {
        return myFeedServices;
    }

    public boolean ignoreBatchRequest() {
        return true;
    }
}
