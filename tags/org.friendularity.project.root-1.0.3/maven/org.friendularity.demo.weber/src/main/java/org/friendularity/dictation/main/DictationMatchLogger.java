/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.main;
import java.io.IOException;
import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
/**
 *
 * @author Eamq
 */
public class DictationMatchLogger {
    private File dictationManagerLogFile;
    private FileWriter dictationManagerLogFileWriter;

    private String myInput="";
    private List<String> myVocabDivs=new ArrayList<String>();
    private List<String> myMeanings = new ArrayList<String>();
    private int myInputCount = 0;

    private static String theHtmlHead = "<head><title>Dictation Log</title>"
            + "<link rel=\"stylesheet\" type=\"text/css\" href=\"../conf/_common/logging/styles_and_scripts/DictationLogger.css\" />"
            + "<script type=\"text/javascript\" src=\"../conf/_common/logging/styles_and_scripts/prototype.js\"></script>"
            + "<script type=\"text/javascript\" src=\"../conf/_common/logging/styles_and_scripts/effects.js\"></script>"
            + "<script type=\"text/javascript\" src=\"../conf/_common/logging/styles_and_scripts/collapsepanel.js\"></script>"
            + "</head>";

    private static DateFormat fileTimeStampFormat=new SimpleDateFormat("yyyy-MM-dd_HH_mm_ss");
    DictationMatchLogger(String logDirectory) throws IOException {

        if(!logDirectory.endsWith("/")) {
            logDirectory=logDirectory+"/";
        }
        
        String logFileName="dictationLog-"+ fileTimeStampFormat.format(new Date()) +".html";
        File logFile=new File(logDirectory+logFileName);
        logFile.createNewFile();

        dictationManagerLogFile=logFile;
        dictationManagerLogFileWriter=new FileWriter(dictationManagerLogFile);

        try {
            dictationManagerLogFileWriter.append(theHtmlHead);
            dictationManagerLogFileWriter.flush();
        }
        catch(IOException ex) {
            ex.printStackTrace();
        }
    }

    private static DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    private String getDateTime(){
        Date date = new Date();
        return dateFormat.format(date);
    }
    
    public String makeHtmlInputHeader(String input){
        return "<div class=\"input_header\" onClick=\"CollapsePanel.toggle_panel(this, $('container_"
                + myInputCount + "'));\">" + getDateTime() + ": <b>" + input + "</b></div>";
    }

    public String makeHtmlContainer(){
        String html = "<div id=\"container_" + myInputCount + "\" class=\"container\">";
        for(String s : myMeanings){
            html += s;
        }
        for(String vocabDiv : myVocabDivs) {
            html+=vocabDiv;
        }
        return html + "</div>";
    }

    public String makeHtmlBestMatch(Match match, String destination)  {
        String html = "<div class=\"best_match\">";
        if(match!=null) {
            html += " Token: <b>" + match.Token+"</b>";
            html += " Meaning: <b>" + match.Meaning+"</b>";
            html += " Match Score: " + match.MatchScore;
            html += " Destination: " + destination;
        }
        else {
            html+="NO MATCH";
        }
        return html + "</div>";
    }

    public String makeHtmlVocab(Vocabulary vocab) {
        String html = "<div class=\"vocabulary\">";
        html+="<div class=\"vocabulary_header\">";
        html+="Vocabulary: " + vocab.getName() + ", " +  vocab.getType() + ", " + vocab.getDestination();
        if(!vocab.getMiscCondition().isEmpty()){
            html += ", " + vocab.getMiscCondition();
        }
        html+="</div>";
        html+=makeHtmlMatchSet(vocab.getMatches());
        return html + "</div>";
    }

    public String makeHtmlMatchSet(MatchSet matches) {
        String html = "<div class=\"matches\">";
        for(Match m : matches) {
            html += makeHtmlMatch(m);
        }
        return html + "</div>";
    }

    public String makeHtmlMatch(Match match){
        String html = "<div class=\"match\">";
        html += " Token: " + match.Token;
        html += " Meaning: " + match.Meaning;
        html += " Match Score: " + match.MatchScore;
        return html + "</div>";
    }

    public void appendInput(String input){
        myInput+=input;
    }

    public String makeHtmlInputBlock() {
        String html = "<div class=\"input_block\">";
        html += makeHtmlInputHeader(myInput);
        html += makeHtmlContainer();
        myInputCount++;
        return html + "</div>";
    }

    public void addVocab(Vocabulary v) {
        myVocabDivs.add(makeHtmlVocab(v));
    }

    public void setBestMatch(Match match, String destination) {
        myMeanings.add(makeHtmlBestMatch(match, destination));
    }

    public void clear()  {
        myInput="";
        myVocabDivs.clear();
        myMeanings.clear();
    }

    public void appendToFile() {
        if(dictationManagerLogFileWriter==null) {
            return;
        }

        try {
            dictationManagerLogFileWriter.append(makeHtmlInputBlock());
            dictationManagerLogFileWriter.flush();
        }
        catch(IOException ex) {
            ex.printStackTrace();
        }
        clear();
    }

    private boolean isCleanedUp = false;
    public synchronized void cleanup() {
        if (isCleanedUp) {return;}

        // flag as cleaned up
        isCleanedUp = true;
        try {
            dictationManagerLogFileWriter.close();
        }
        catch(IOException ex){
            ex.printStackTrace();
        }
    }

    @Override
    public void finalize() {
        cleanup();
        try {super.finalize();}
        catch(Throwable ex) {ex.printStackTrace();}
    }
}