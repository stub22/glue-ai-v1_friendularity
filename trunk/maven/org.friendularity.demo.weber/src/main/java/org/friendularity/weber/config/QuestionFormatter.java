/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.weber.config;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author matt
 */
public class QuestionFormatter implements IStringFormatter {
    private String myReplacement;
    private List<String> myTokens;

    public QuestionFormatter(){
        myReplacement = "";
        myTokens = new ArrayList<String>();
    }

    public String format(String input) {
        int lastIndex = 0;
        for(String t : myTokens){
            int i = input.lastIndexOf(t);
            int j = i + t.length();
            if(i >= 0 && j > lastIndex){
                lastIndex = j;
            }
        }
        if(lastIndex == 0)
            return input;
        
        return myReplacement + input.substring(lastIndex);
    }
    
    public static QuestionFormatter loadFromFile(String path){
        QuestionFormatter qf = new QuestionFormatter();
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
