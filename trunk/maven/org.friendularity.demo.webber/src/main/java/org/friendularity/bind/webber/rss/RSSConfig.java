/*
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.bind.webber.rss;

import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;

/**
 *
 * @author Matt Stevenson
 */
public class RSSConfig {
    private static Logger theLogger = Logger.getLogger(RSSConfig.class.getName());
    private static RSSConfig theConfig;
    private LinkedHashMap<String, String> myFeedURLMap;
    private String myConfigPath;

    public RSSConfig(String path) throws IOException {
        myConfigPath = path;
        loadConfig();
    }

    public void loadConfig() throws IOException {
        Properties prop = new Properties();
        prop.load(new FileReader(myConfigPath));
        myFeedURLMap = new LinkedHashMap<String, String>();
        for(Entry e : prop.entrySet()){
            myFeedURLMap.put((String)e.getKey(),(String)e.getValue());
        }
    }

    public LinkedHashMap<String,String> getFeedURLMap(){
        return myFeedURLMap;
    }

    public Set<String> getFeedNames(){
        return myFeedURLMap.keySet();
    }

    public Collection<String> getURLs(){
        return myFeedURLMap.values();
    }

    public static void init(String path){
        try{
            theConfig = new RSSConfig(path);
        }catch(IOException ex){
            theLogger.warning("Error initializing config.  Cannot find file: " + path);
        }
    }

    public static RSSConfig getConfig(){
        return theConfig;
    }
}
