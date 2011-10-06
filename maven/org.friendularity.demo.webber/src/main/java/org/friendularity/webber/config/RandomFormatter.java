/*
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.webber.config;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 *
 * @author Matt Stevenson
 */
public class RandomFormatter implements IStringFormatter {
    private String          myReplacement;
    private List<String>    myTokens;
    private Random          theRandomizer;

    public RandomFormatter(){
        theRandomizer = new Random();
        myReplacement = "";
        myTokens = new ArrayList<String>();
    }

    public String format(String input) {
        String rep = "";
        if(!myTokens.isEmpty()){
            rep = myTokens.get(theRandomizer.nextInt(myTokens.size()));
        }
        return input.replaceAll(myReplacement, rep);
    }

    public static RandomFormatter loadFromFile(String path){
        RandomFormatter qf = new RandomFormatter();
        try{
            FileReader fin = new FileReader(path);
            BufferedReader din = new BufferedReader(fin);
            String line = din.readLine();
            if(line == null){
                fin.close();
                return qf;
            }
            qf.setReplacementString(line);
            while((line = din.readLine()) != null){
                qf.addToken(line);
            }
            fin.close();
        }catch(Throwable t){}
        return qf;
    }

    public void setReplacementString(String rep){
        myReplacement = rep;
    }

    public String getReplacementString(){
        return myReplacement;
    }

    public void addToken(String token){
        if(token == null || token.isEmpty())
            return;
        token = token.toLowerCase().trim();
        if(!myTokens.contains(token))
            myTokens.add(token);
    }

    public List<String> getTokens(){
        return myTokens;
    }
}
