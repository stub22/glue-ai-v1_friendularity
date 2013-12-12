/*
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.dictation.main;

import java.io.FileReader;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;
import org.xmlpull.v1.XmlPullParserFactory;

/**
 *
 * @author Matt Stevenson
 */
public class VocabularyLoader {
   private static Logger	theLogger = Logger.getLogger(VocabularyManager.class.getName());

    public static Vocabulary LoadVocab(String path) {
        try{
            XmlPullParserFactory factory = XmlPullParserFactory.newInstance();
            XmlPullParser xpp = factory.newPullParser();
            xpp.setInput(new FileReader(path));
            return parseDocument(xpp);
        }catch(Throwable t){
			theLogger.log(Level.SEVERE, "Problem reading vocabulary from: " + path, t);
        }
        return null;
	}

    private static Vocabulary parseDocument(XmlPullParser xpp) throws IOException, XmlPullParserException{
        Vocabulary vocab = new Vocabulary();
        int eventType = xpp.getEventType();
        while (eventType != XmlPullParser.END_DOCUMENT) {
            if(eventType == XmlPullParser.START_DOCUMENT) {
            } else if(eventType == XmlPullParser.END_DOCUMENT) {
            } else if(eventType == XmlPullParser.START_TAG) {
                parseStartTag(xpp, vocab);
            } else if(eventType == XmlPullParser.END_TAG) {
            } else if(eventType == XmlPullParser.TEXT) {
            }
            eventType = xpp.next();
        }
        return vocab;
    }

    private static void parseStartTag(XmlPullParser xpp, Vocabulary v){
        String name = xpp.getName();
        if(name.equals("Vocab")){
            parseVocabAttrs(xpp, v);
        }else if(name.equals("Token")){
            parseToken(xpp, v);
        }
    }

    private static void parseVocabAttrs(XmlPullParser xpp, Vocabulary v){
        int attrs = xpp.getAttributeCount();
        for(int i=0; i<attrs; i++){
            String name = xpp.getAttributeName(i);
            String val = xpp.getAttributeValue(i);
            parseVocabAttr(name, val, v);
        }
    }

    private static void parseVocabAttr(String name, String val, Vocabulary v){
        if(name.equals("name")){
            v.setName(val);
        }else if(name.equals("type")){
            v.setType(val);
        }else if(name.equals("destination")){
            v.setDestination(val);
        }else if(name.equals("condition")){
            v.setMiscCondition(val);
        }
    }

    private static void parseToken(XmlPullParser xpp, Vocabulary v){
        int attrs = xpp.getAttributeCount();
        String text = null, meaning = null;
        for(int i=0; i<attrs; i++){
            String name = xpp.getAttributeName(i);
            String val = xpp.getAttributeValue(i);
            if(name.equalsIgnoreCase("text")){
                text = val;
            }else if(name.equalsIgnoreCase("meaning")){
                meaning = val;
            }
        }
        if(text != null && meaning != null){
            v.addToken(text, meaning);
        }
    }
}
