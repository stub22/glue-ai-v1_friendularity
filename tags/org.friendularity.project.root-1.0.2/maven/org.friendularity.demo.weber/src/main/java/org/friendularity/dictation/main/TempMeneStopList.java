/*
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.dictation.main;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Matt Stevenson
 */
public class TempMeneStopList {
    private static List<String> myIgnoreList;
    public static boolean validateInput(String input){
        ensureInitialized();
        if(myIgnoreList.contains(input)){
            return false;
        }
        input = input.trim();
        String cpy = input.replaceAll(" he", "");
        int count = (input.length() - cpy.length())/3;
        if(count >= 3){
            return false;
        }
        String cp2 = cpy.replaceAll(" will", "");
        int c2 = (cpy.length() - cp2.length())/5;
        if(c2 >= 3){
            return false;
        }
        int tot = count + c2;
        if(tot >= 4){
            return false;
        }
        double ratio = (double)cp2.length()/(double)input.length();
        if(ratio < 0.33){
            return false;
        }
        return true;
    }

    private static void ensureInitialized(){
        if(myIgnoreList != null){
            return;
        }
        String[] ignoreList = {
            "be no",
            "you",
            "and",
            "will will",
            "is your",
            "will or",
            "and it is",
            "weblog",
            "what you like",
            "will",
            "are",
            "i a lot",
            "i do have a",
            "is what the",
            "will",
            "will",
            "he",
            "now",
            "will have",
            "will",
            "are you",
            "will",
            "will",
            "a will",
            "well and he and he",
            "will",
            "as a",
            "will",
            "and all i he is",
            "will",
            "he",
            "will you do",
            "and",
            "he",
            "and",
            "will",
            "he",
            "will",
            "you are",
            "will",
            "you are",
            "are you",
            "and will",
            "you will he ",
            "will you will",
            "all",
            "will will will",
            "will",
            "you",
            "do all",
            "what will ye",
            "her in the",
            "you will",
            "you",
            "he will will will",
            "you as a",
            "well are in",
            "will",
            "you will",
            "we will",
            "or will",
            "her",
            "as well i'll yeah will",
            "we will i'll",
            "your",
            "where i'll he he he he he he and",
            "he",
            "will you are",
            "you are",
            "you will you he",
            "will you",
            "will is",
            "you will you",
            "you will",
            "our and yeah yeah are",
            "will he",
            "he",
            "i'll where he",
            "is or will",
            "you will will you are there he",
            "you and",
            "is are he will",
            "you he will",
            "are you are your",
            "i'll",
            "he",
            "will will will live",
            "you",
            "her are",
            "well",
            "as well"
        };
        myIgnoreList = new ArrayList<String>();
        for(String s : ignoreList){
            myIgnoreList.add(s);
        }
    }
}
