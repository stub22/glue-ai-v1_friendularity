/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.main;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;


/**
 *
 * @author Matt Stevenson
 */
public class Vocabulary {
	private Map<String, String>	myTokenMeaningMap;
	private String						myName;
	private String						myDestination;
	private String						myMiscCondition;
	private String						myInput;
	private String						myType;
	private MatchSet					myMatches;
    private List<String>                mySortedMeanings;
    private Map<String, List<String>>   mySortedTokens;

	public Vocabulary(){
		myTokenMeaningMap = new HashMap<String, String>();
		myInput = "";
	}

    public void addToken(String token, String meaning){
        myTokenMeaningMap.put(token.toLowerCase(), meaning);
    }

    public String getTokenMeaning(String token){
        return myTokenMeaningMap.get(token.toLowerCase());
    }

	public Map<String, String> getTokenMeaningMap(){
		return myTokenMeaningMap;
	}

    public int getTokenCount(){
        return myTokenMeaningMap.size();
    }

	public void clearInput(){
		myInput = "";
		myMatches = new MatchSet();
	}
	public void appendInput(String input){
		input = input.toLowerCase();
		myInput += input;
		myMatches = getTokenMatchSet(myInput);
	}
	public void setInput(String input){
		input = input.toLowerCase();
		myInput = input;
		myMatches = getTokenMatchSet(myInput);
	}
	public String getInput(){
		return myInput;
	}
	public MatchSet getMatches(){
		return myMatches;
	}
    public void setName(String name){
        myName = name;
    }
	public String getName(){
		return myName;
	}
    public void setDestination(String dest){
        myDestination = dest;
    }
	public String getDestination(){
		return myDestination;
	}
    public void setMiscCondition(String misc){
        myMiscCondition = misc;
    }
	public String getMiscCondition(){
        if(myMiscCondition == null) {
            myMiscCondition = "";
		}
		return myMiscCondition;
	}
    public void setType(String type){
        myType = type;
    }
	public String getType(){
		return myType;
	}

	public Match getBestMatch(){
		if(myMatches.size() == 0)
			return null;
		return myMatches.get(0);
	}

	public MatchSet getTokenMatchSet(String input){
		input = input.toLowerCase();
		MatchSet matches = new MatchSet();
        String[] variations = {"", "s", "er", "ing", "ers", "ings"};
		for(String token : myTokenMeaningMap.keySet()){
            for(String v : variations){
                String tv = token + v;
                if(input.equals(tv) || input.startsWith(tv + " ") ||
                        input.endsWith(" " + tv) || input.contains(" " + tv + " ")){
                    matches.add(new Match(token, myTokenMeaningMap.get(token), (double)token.length()));
                }
            }
		}
		matches.sort();
        myMatches = matches;
		return matches;
	}

	public void consume(Vocabulary v){
		for(String token : v.myTokenMeaningMap.keySet()){
			if(!myTokenMeaningMap.containsKey(token))
				myTokenMeaningMap.put(token, v.myTokenMeaningMap.get(token));
		}
	}

    @Override
    public String toString(){
        return "<Vocabulary: " + myName + ", " + myType + ", " + myDestination + ", " + myMiscCondition + ">";
    }

    public String toHtml()
    {
        String html = "<div class=\"vocabulary\">";
        html+="<div class=\"vocabulary_header\">";
        html+="Vocabulary: " + this.getName() + ", " + this.getType() + ", " + this.getDestination() + ", " + this.getMiscCondition();
        html+="</div>";
        html+=this.getMatches().toHtml();
        html+="</div>";
        return html;
    }

    public String toXML(){
        String xml = "<Vocab name=\"" + myName + "\" type=\"" + myType + "\" destination=\""
                + myDestination + "\" condition=\"" + myMiscCondition + "\">\n";
        for(Entry<String, String> kv : myTokenMeaningMap.entrySet()){
            xml += "\t" + getTokenXML(kv.getKey(), kv.getValue()) + "\n";
        }
        xml += "</Vocab>";
        return xml;
    }

    private void sort(){
        mySortedMeanings = new ArrayList(myTokenMeaningMap.values());
        Collections.sort(mySortedMeanings);
        mySortedTokens = new HashMap();
        for(Entry<String, String>  e : myTokenMeaningMap.entrySet()){
            if(!mySortedTokens.containsKey(e.getValue())){
                mySortedTokens.put(e.getValue(), new ArrayList<String>());
            }
            mySortedTokens.get(e.getValue()).add(e.getKey());
        }
        for(String s : mySortedMeanings){
            Collections.sort(mySortedTokens.get(s));
        }
    }

    public Set<String> getTokens(){
        return myTokenMeaningMap.keySet();
    }

    private String getTokenXML(String t, String m){
        t = t.toLowerCase().trim().replaceAll("[^a-z .,?!;':]", "");
        m = m.toUpperCase().trim();
        return "<Token text=\"" + t + "\" meaning=\"" + m + "\"/>";
    }
}
