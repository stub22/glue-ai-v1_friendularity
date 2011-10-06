/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.weber.config;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 *
 * @author matt
 */
public class ReplaceFormatter implements IStringFormatter{
    private LinkedList<String[]> myReplacements;

    public ReplaceFormatter(){
        myReplacements = new LinkedList<String[]>();
    }

    public String format(String input){
        Iterator i = myReplacements.iterator();
        while (i.hasNext()) {
            String[] pair = (String[])i.next();
            input = input.replace(pair[0], pair[1]);
        }
        return input;
    }

    public static ReplaceFormatter loadFromFile(String path){
        ReplaceFormatter rf = new ReplaceFormatter();
        try{
            FileReader fin = new FileReader(path);
            BufferedReader din = new BufferedReader(fin);
            String line = "";
            while((line = din.readLine()) != null){
                if(line.length() < 2)
                    continue;

                int i = line.indexOf(",");
                String rep_key = line.substring(0,i);
                String rep_val = i+1 == line.length() ? "" : line.substring(i+1);
                rf.addReplacement(rep_key, rep_val);
            }
            fin.close();
        }catch(Throwable t){}
        return rf;
    }

    public void addReplacement(String find, String replace){
        if(find == null || find.isEmpty())
            return;

        if(replace == null)
            replace = "";

        String[] replacementPair = {find, replace};
        myReplacements.add(replacementPair);
    }

    public List<String[]> getReplacements(){
        return myReplacements;
    }
}
