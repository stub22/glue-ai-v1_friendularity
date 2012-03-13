/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.weber.utils;

/**
 *
 * @author Administrator
 */
import org.friendularity.weber.services.ServiceRequestThread;
import org.friendularity.weber.services.INexusService;
import org.friendularity.weber.services.BatchServiceRequest;
import org.friendularity.weber.services.GenRespWithConf;
import java.io.IOException;
import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

import java.lang.StringBuilder;
public class MeneLogger {
    private File meneLogFile;
    private FileWriter meneLogFileWriter;

    private int myBlockCount=0;

    public static String theHtmlHead =
              "<head>\n"
            + "  <title>Messaging Nexus Log</title>\n"
            + "  <link rel=\"stylesheet\" type=\"text/css\" href=\"../conf/_common/logging/styles_and_scripts/MessagingNexusLogger.css\" />\n"
            + "  <script type=\"text/javascript\" src=\"../conf/_common/logging/styles_and_scripts/prototype.js\"></script>\n"
            + "  <script type=\"text/javascript\" src=\"../conf/_common/logging/styles_and_scripts/effects.js\"></script>\n"
            + "  <script type=\"text/javascript\" src=\"../conf/_common/logging/styles_and_scripts/collapsepanel.js\"></script>\n"
            + "</head>";

    private List<String> myRequestDivs= new ArrayList<String>();

    private static DateFormat fileTimeStampFormat=new SimpleDateFormat("yyyy-MM-dd_HH_mm_ss");
    public MeneLogger(String logDirectory) throws IOException {
        if(logDirectory==null) {
            logDirectory="";
        }
        if(!logDirectory.endsWith("/")) {
            logDirectory=logDirectory+"/";
        }


        String logFileName="meneLog-"+ fileTimeStampFormat.format(new Date()) +".html";
        File logFile=new File(logDirectory+logFileName);
        logFile.createNewFile();

        meneLogFile=logFile;
        meneLogFileWriter=new FileWriter(meneLogFile);

        meneLogFileWriter.append(theHtmlHead);
        meneLogFileWriter.flush();
    }

    private String getDateTime(){
        DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
        Date date = new Date();
        return dateFormat.format(date);
    }

    public void appendToFile() {
        if(meneLogFileWriter==null) {
            return;
        }

        try {
            for(String str : myRequestDivs ) {
                meneLogFileWriter.append(str);
                meneLogFileWriter.flush();
            }
        }
        catch(IOException ex) {
            ex.printStackTrace();
        }
        myRequestDivs.clear();
    }

    public void addBatchServiceRequest(BatchServiceRequest bsr, boolean toConvoid){
        myRequestDivs.add(makeHtmlServiceRequestResultBlock(bsr,toConvoid));
    }

    private String makeHtmlServiceRequestResultBlock(BatchServiceRequest bsr, boolean toConvoid)  {
        StringBuilder html = new StringBuilder();
        html.append("<div class=\"input_block\">\n");
        html.append("  "+makeHtmlInputHeader(bsr, toConvoid)+"\n");
        html.append("  "+makeHtmlContainer(bsr));
        html.append("</div>\n");
        myBlockCount++;
        return html.toString();
    }

    public String makeHtmlInputHeader(BatchServiceRequest bsr, boolean toConvoid){
        String html="<div class=\"input_header\" onClick=\"CollapsePanel.toggle_panel(this, $('container_"
                + myBlockCount + "'));\">";
        html += getDateTime() + ": <b>" + bsr.getInputString() + "</b><br/>";
        html += "Time Taken: "+bsr.getElapsedTimeMillis()+"&nbsp;&nbsp;&nbsp;&nbsp;To Convoid:"+(toConvoid?"TRUE":"FALSE");
        html += "</div>";
        
        return html;
    }
    
    private String makeHtmlContainer(BatchServiceRequest bsr){
        String html = "<div id=\"container_" + myBlockCount + "\" class=\"container\">";

        List<GenRespWithConf> responses= bsr.getReceivedResponses();
        GenRespWithConf bestResponse=bsr.getBestResponse();
        html += makeHtmlResponseDiv( bestResponse, true );

        for(GenRespWithConf resp : responses) {
            if(resp!=bestResponse) {
                html += makeHtmlResponseDiv( resp, false );
            }
        }

        for(ServiceRequestThread srt : bsr.getIncompleteRequests() ) {
            html+=makeHtmlIncompleteRequestDiv(srt);
        }

        for(INexusService ignoredService : bsr.getIgnoredServices()) {
            html+=makeHtmlIgnoredServiceDiv(ignoredService);
        }
        return html + "</div>";
    }

    private String makeHtmlResponseDiv(GenRespWithConf resp, boolean isBestResponse) {
        String cssClass=isBestResponse?"best_response":"response";
        String html = "<div class=\""+cssClass+"\">";
        if(resp!=null){
            html+=resp.getServiceName()+":"+resp.getResponse();
            html+="<br />Confidence: "+resp.getConfidence();
            html+="<br />Elapsed Time: "+resp.getElapsedTimeMillis()+"ms";
        }
        else {
            html+="NO RESPONSE RECIEVED";
        }
        html += "</div>";
        return html;
    }

    private String makeHtmlIncompleteRequestDiv(ServiceRequestThread incompleteServiceRequest){
        String cssClass="incomplete_service_request";
        String html = "<div class=\""+cssClass+"\">";
        if(incompleteServiceRequest!=null){
            html+=" Incomplete Request: "+incompleteServiceRequest.getServiceName();
            html+=" Started At: "+incompleteServiceRequest.getStartTimeMillis();
        }
        else {
            html+="NO INCOMPLETE SERVICE REQUEST GIVEN";
        }
        html += "</div>";
        return html;
    }

    private String makeHtmlIgnoredServiceDiv(INexusService ignoredService) {
        String cssClass="ignored_service_request";
        String html = "<div class=\""+cssClass+"\">";
        if(ignoredService!=null){
            html+=" Ignored Batch Request: "+ignoredService.getServiceName();
        }
        else {
            html+="NO IGNORED SERVICE GIVEN";
        }
        html += "</div>";
        return html;
    }
    
    private boolean isCleanedUp = false;
    public synchronized void cleanup() {
        if (isCleanedUp) {return;}

        // flag as cleaned up
        isCleanedUp = true;
        try {
            meneLogFileWriter.close();
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
