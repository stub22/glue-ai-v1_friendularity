/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.main;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.friendularity.dictation.jmx.JMXWrapper;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import javax.swing.JTextArea;

/**
 *
 * @author matt
 */
public class VocabularyManager {
    private static Logger	theLogger = Logger.getLogger(VocabularyManager.class.getName());
    public List<Vocabulary> myVocabs;
    private JTextArea myArea = null;

    private DictationMatchLogger myMatchLogger;

    public VocabularyManager(String logDirectory) {
        try {
            myMatchLogger=new DictationMatchLogger(logDirectory);
        }
        catch(IOException ex) {
            ex.printStackTrace();
        }
    }


    public static List<Vocabulary> loadVocabulariesFromFile(String path){
		File dir = new File(path);
		return recursivelyLoadVocabs(dir);
    }
    
    private static List<Vocabulary> recursivelyLoadVocabs(File directory){
        List<Vocabulary> vocabs = new ArrayList<Vocabulary>();
        if(directory == null || !directory.isDirectory()){
            return vocabs;
        }
        File[] files = directory.listFiles();
        for(File f : files){
            if(f.isFile()){
                vocabs.addAll(loadVocabFile(f.getAbsolutePath()));
            }else if(f.isDirectory() && !directory.getAbsolutePath().contains(f.getAbsolutePath())){
                vocabs.addAll(recursivelyLoadVocabs(f));
            }
        }
        return vocabs;
    }

    private static List<Vocabulary> loadVocabFile(String filePath){
        List<Vocabulary> vocabs = new ArrayList<Vocabulary>();
        if(!filePath.endsWith(".xml")){
            return vocabs;
        }
        try{
            Vocabulary v = VocabularyLoader.LoadVocab(filePath);
            if(v != null){
                vocabs.add(v);
            }
        }catch(Throwable t){
            theLogger.warning("Error loading vocabulary - " + filePath);
            t.printStackTrace();
        }
        return vocabs;
    }
	
    public void setTextArea(JTextArea area){
        myArea = area;
    }
    
    public static VocabularyManager loadManagerFromFile(String path, String logDirectory){
        VocabularyManager vm = new VocabularyManager(logDirectory);
        vm.addVocabularies(loadVocabulariesFromFile(path));
        return vm;
    }

    public void addVocabularies(List<Vocabulary> vocabs){
        if(myVocabs == null)
            myVocabs = new ArrayList<Vocabulary>();
        myVocabs.addAll(vocabs);
    }

    public List<Vocabulary> getVocabularies(){
        return myVocabs;
    }

    public void appendInput(String input){
		input = input.trim();
		//cheap hack
		//when dragon hears a number, if the last thing heard was another number it will concatonate the two
		//user say "one" for an option, then the user says "three"
		//the last input will be changed by dragon to "13"
		//So we are crippling the robot and it can only hear 0-9 unless the number is part of a longer string
		if(input.matches("^\\d+$")){
			input = input.substring(input.length()-1);
			int x = Integer.parseInt(input);
			switch(x){
				case 0 : input = "zero"; break;
				case 1 : input = "one"; break;
				case 2 : input = "two"; break;
				case 3 : input = "three"; break;
				case 4 : input = "four"; break;
				case 5 : input = "five"; break;
				case 6 : input = "six"; break;
				case 7 : input = "seven"; break;
				case 8 : input = "eight"; break;
				case 9 : input = "nine"; break;
			}
		}
        myMatchLogger.appendInput(input);
        for(Vocabulary v : myVocabs){
            v.appendInput(input);
        }
    }

    public void SendMatches(JMXWrapper myWrapper){
        MatchSet misc = new MatchSet();
        MatchSet charEng = new MatchSet();
        MatchSet charEngMisc = new MatchSet();
        MatchSet nexus = new MatchSet();
        MatchSet nexusMisc = new MatchSet();
        misc.addAll(ContextMatchSet.getMatches());
        SendMatches(misc, charEng, charEngMisc, nexus, nexusMisc, myWrapper);
    }

    public void SendMatches(MatchSet misc, MatchSet charEng, MatchSet charEngMisc, MatchSet nexus, MatchSet nexusMisc, JMXWrapper myWrapper){
        for(Vocabulary v : myVocabs){
            if(v.getDestination().equals("MISC")){
                misc.addAll(v.getMatches());
            }
        }
        for(Vocabulary v : myVocabs){
            if(!v.getMatches().isEmpty())  {
                myMatchLogger.addVocab(v);
            }
            if(v.getDestination().equals("MISC") || v.getBestMatch() == null){
                continue;
            }
			
            if(v.getType().equals("INFORMATIVE")){
                SendInformative(v.getName(), v.getBestMatch().Meaning, v.getDestination(), myWrapper);
                continue;
            }

            if(v.getMiscCondition().isEmpty()){
                if(v.getDestination().equals("CHARACTER_ENGINE")){
                    MatchSet m = v.getMatches();
                    charEng.addAll(m);
                } else if(v.getDestination().equals("MESSAGING_NEXUS")){
                    MatchSet m = v.getMatches();
                    nexus.addAll(m);
                }
            }else{
                if(v.getDestination().equals("CHARACTER_ENGINE")){
                    if(misc.containsMeaning(v.getMiscCondition())){
                        MatchSet m = v.getMatches();
                        m.prepend(v.getMiscCondition());
                        charEngMisc.addAll(m);
                    }
                }else if(v.getDestination().equals("MESSAGING_NEXUS")){
                    if(misc.containsMeaning(v.getMiscCondition())){
                        MatchSet m = v.getMatches();
                        m.prepend(v.getMiscCondition());
                        nexusMisc.addAll(m);
                    }
                }
            }
        }
        consolidateAndSendMatches(charEng, charEngMisc, nexus, nexusMisc, myWrapper);

        //sendBestResponse(charEng, charEngMisc, nexus, nexusMisc, myWrapper);
        myMatchLogger.appendToFile();
        clearInput();
        //ContextMatchSet.clear();
    }

    private void consolidateAndSendMatches(MatchSet charEng, MatchSet charEngMisc, 
            MatchSet nexus, MatchSet nexusMisc, JMXWrapper jmx){
        List<Match> matches = new ArrayList();
        String dest = "";
        if(!charEngMisc.isEmpty()){
            matches = getTopMatches(charEngMisc, matches, 5);
            matches = getTopMatches(charEng, matches, 5);
            dest = "CHARACTER_ENGINE";
        }else if(!nexusMisc.isEmpty()){
            matches = getTopMatches(nexusMisc, matches, 5);
            matches = getTopMatches(nexus, matches, 5);
            dest = "MESSAGING_NEXUS";
        }else if(!charEng.isEmpty()){
            matches = getTopMatches(charEng, matches, 5);
            dest = "CHARACTER_ENGINE";
        }else{
            matches = getTopMatches(nexus, matches, 5);
            dest = "MESSAGING_NEXUS";
        }
        matches = normalizeMatchScores(matches);
        sendBestMatches(matches, dest, jmx);
    }

    private List<Match> normalizeMatchScores(List<Match> rawMatches){
        List<Match> matches = new ArrayList<Match>();
        Double bestScore = -1000.0;
        for(Match m : rawMatches){
            if(m.MatchScore > bestScore){
                bestScore = m.MatchScore;
            }
        }
        for(Match m : rawMatches){
            Double score = (m.MatchScore/bestScore);
            Match nm = new Match(m.Token, m.Meaning, score);
            matches.add(nm);
        }
        return matches;
    }

    private List<Match> getTopMatches(MatchSet matches, List<Match> currentMatches, int matchLimit){
        matches.sort();
        int start = currentMatches.size();
        int limit = matchLimit - start;
        if(limit <= 0){
            return currentMatches;
        }
        Iterator mit = matches.iterator();
        while(mit.hasNext() && limit > 0){
            Match m = (Match)mit.next();
            boolean skip = false;
            for(int i=0; i<currentMatches.size(); i++){
                Match mm = currentMatches.get(i);
                if(mm.Meaning.equals(m.Meaning)){
                    skip = true;
                    if(m.MatchScore > mm.MatchScore){
                        currentMatches.remove(i);
                        currentMatches.add(i, m);
                    }
                    break;
                }
            }
            if(!skip){
                currentMatches.add(m);
            }
        }
        return currentMatches;
    }

    private void sendBestMatches(List<Match> matches, String destination, JMXWrapper wrapper){
        Map<String,Double> meanings = new HashMap<String, Double>();
        for(Match m : matches){
            meanings.put(m.Meaning, m.MatchScore);
            myMatchLogger.setBestMatch(m, destination);
        }
		String input = myVocabs.get(0).getInput();
		wrapper.sendHeard(input);
        if(!matches.isEmpty()){
            if(destination.equals("CHARACTER_ENGINE")){
                SendCharEngine(meanings, wrapper);
                SendNexus(myVocabs.get(0).getInput(), false, wrapper);
            }else if(destination.equals("MESSAGING_NEXUS")){
                String mstr = "";
                for(Match m : matches){
                    mstr += m.Meaning + " ";
                }
                SendNexus(mstr.trim(), true, wrapper);
            }
        }else{
            if(TempMeneStopList.validateInput(input)){
                SendNexus(myVocabs.get(0).getInput(), true, wrapper);
            }
        }
    }

    public void clearInput(){
        for(Vocabulary v : myVocabs){
            v.clearInput();
        }
    }

    public void SendInformative(String vocabName, String message, String destination, JMXWrapper myJMXWrapper){
        try{
            print("Sending Information: " + message + "\n\tfrom vocab: " + vocabName + "\n\tto: " + destination);
            if(destination.equals("CHARACTER_ENGINE")){
                    myJMXWrapper.sendInformationToConvoid(vocabName, message);
            }else if(destination.equals("MESSAGING_NEXUS")){
                    myJMXWrapper.sendToNexus(vocabName + " " + message, false);
            }
        }catch(Throwable t){
            t.printStackTrace();
        }
    }

    public void SendCharEngine(Map<String,Double> meanings, JMXWrapper myJMXWrapper){
        String matchStr = "";
        for(Entry<String,Double> e : meanings.entrySet()){
            matchStr += e.getKey() + "(" + e.getValue() + ")" + ", ";
        }
        print("Sending Response: " + matchStr.trim() + "\n\tto: CHARACTER_ENGINE");
        myJMXWrapper.sendResponseMeaningToConvoid(meanings);
    }

    public void SendNexus(String message, boolean forReal, JMXWrapper myJMXWrapper){
        String what = (forReal ? "Response" : "Information");
        print("Sending " + what + ": " + message + "\n\tto: MESSAGING_NEXUS");
        myJMXWrapper.sendToNexus(message, forReal);
    }

    private void print(String s){
        theLogger.info(s);
        if(myArea != null){
            myArea.append(s+"\n");
        }
    }
    
    
}