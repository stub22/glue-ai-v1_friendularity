/*
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.dictation.main;

/**
 *
 * @author Matt Stevenson
 */
public class ContextMatchSet {
    private static MatchSet matches = new MatchSet();

    public static void addMatch(Match match){
        matches.add(match);
    }

    public static void removeMeaning(String meaning){
        for(Match m : matches){
            if(m.Meaning.equals(meaning)){
                matches.remove(m);
                return;
            }
        }
    }

    public static MatchSet getMatches(){
        return matches;
    }

    public static void clear(){
        matches.clear();
    }
}
