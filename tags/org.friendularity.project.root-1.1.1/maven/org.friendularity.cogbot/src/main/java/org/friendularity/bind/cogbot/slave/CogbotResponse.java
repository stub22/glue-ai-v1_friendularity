/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bind.cogbot.slave;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.Arrays;
import java.util.HashSet;

//import com.hansonrobotics.mene.config.MeneConfig;
//import com.hansonrobotics.mene.utils.Utils;
import java.io.PrintWriter;
import java.util.Iterator;
import java.util.Properties;
import org.friendularity.bind.cogbot.service.CogbotService;
import org.friendularity.bind.cogbot.simulator.CogbotAvatar;

/**
 *
 * @author dmiles/matt
 */
public class CogbotResponse {

    //This bot is currently set to not accept user input.
    public static String NO_RESPONSE = "Cogbot has no response for that input";
    private String usernameInRequest;
    private String currentBotId;
    private String myResponse;
    private HashSet<String> animations = new HashSet<String>();
    private String initialRequest;
    private Properties myConfig;
    public static double scoreMult = 8;
    public static int scoreMax = 12;
    private double scoreCurrent = scoreMult;
    private Long myStartTimeMillis = null;
    final CogbotAvatar avatar;

    public Long getStartTimeMillis() {
        return myStartTimeMillis;
    }
    private Long myEndTimeMillis = null;
    private PrintWriter callback;

    public Long getEndTimeMillis() {
        return myEndTimeMillis;
    }

    public CogbotResponse(CogbotAvatar av, PrintWriter cogbot, Properties config,
            String input, String userName, String botId) {
        callback = cogbot;
        avatar = av;
        myConfig = config;
        usernameInRequest = userName;//av.coerceToUser(userName);
        currentBotId = botId;
        initialRequest = input;
        gatherResponses();
    }

    protected void gatherResponses() {
        try {
            String cogbotUrlLocal = avatar.getConfig().getChatUrl();
            if (cogbotUrlLocal != null) {
                URL url = getCogbotUrl(initialRequest, cogbotUrlLocal,
                        usernameInRequest, currentBotId);
                myStartTimeMillis = System.currentTimeMillis();
                BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()));
                reader.skip(0); //ensures open
                parseHTML(reader);
            }
        } catch (IOException e) {
            log(e);
        } finally {
            myEndTimeMillis = System.currentTimeMillis();
            done();
        }
    }

    protected URL getCogbotUrl(String input, String baseUrl, String userName,
            String botId) {
        userName = URLEncode(userName);
        botId = URLEncode(botId);
        URL url = null;
        baseUrl += "entry=" + URLEncode(input);
        try {
            if (userName != null && !userName.isEmpty()) {
                baseUrl += "&ident=" + userName;
            }
            if (botId != null && !botId.isEmpty()) {
                baseUrl += "&logId=" + botId;
            }
            url = new URL(baseUrl);
            callback.println("Sending to: " + url.toString());
        } catch (Exception e) {
            log(e);
        }
        return url;
    }

    private String URLEncode(String text) {
        try {
            if (text==null) return "";
            return java.net.URLEncoder.encode(text, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            log(e);
            return text;
        }
    }

    private void parseHTML(BufferedReader in) throws IOException {
        String temp = "";
        String response = "";
        String animation = "";
        animations.clear();
        //
        boolean accept = false;
        boolean meta = false;
        try {
            in.skip(0); //ensures open
            next_line:
            while ((temp = in.readLine()) != null) {
                more_temp:
                do {
                    String temp2 = null;
                    temp = temp.trim();
                    if (temp.length() == 0) {
                        continue next_line;
                    }
                    String s = "<!-- Begin Meta !-->";
                    if (temp.startsWith(s)) {
                        meta = true;
                        temp = temp.replace(s, "");
                        if (temp.length() > 0) {
                            continue more_temp;
                        }
                    }
                    s = "<!-- End Meta !-->";
                    if (temp.startsWith(s)) {
                        meta = false;
                        temp = temp.replace(s, "");
                        if (temp.length() > 0) {
                            continue more_temp;
                        }
                    }

                    s = "<!-- Begin Response !-->";
                    if (temp.startsWith(s)) {
                        accept = true;
                        temp = temp.replace(s, "");
                        if (temp.length() > 0) {
                            continue more_temp;
                        }
                    }
                    s = "<!-- End Response !-->";
                    if (temp.startsWith(s)) {
                        accept = false;
                        temp = temp.replace(s, "");
                        if (temp.length() > 0) {
                            continue more_temp;
                        }
                    }

                    int tagged = temp.indexOf("<!-");
                    if (tagged > -1) {
                        temp2 = temp.substring(tagged);
                        temp = temp.substring(0, tagged);
                    }

                    if (meta) {
                        addAnimation(temp);
                        temp = "";
                    } else if (accept) {
                        response += temp;
                        temp = "";
                    } else {
                        continue next_line;
                    }
                    if (temp2 != null) {
                        temp = temp2;
                    }
                    temp = temp.trim();
                } while (true);
            }

            response = response.trim();
            setResponse(response);
        } catch (Throwable x) {
            log(x);
            animation = "ERROR";
            response ="ERROR "+ x.toString();
            setResponse(response);
        }
    }

    private void addAnimation(String temp) {
        if (temp == null) {
            return;
        }
        temp = temp.trim();
        if (temp.length() == 0) {
            return;
        }
        animations.add(temp);
    }

    private boolean hansonAnim(String temp) {
        for (char c : temp.toCharArray()) {
            if (Character.isDigit(c)) {
                return true;
            }
        }
        return false;
    }

    public String getAnimation() {
        if (animations.size() < 1) {
            return "";
        }
        Iterator<String> iter = animations.iterator();
        String anim = iter.next();
        anim = "<bookmark mark=\"anim:" + anim + "\" />";
        return anim;
        /*
        String str = Arrays.toString(animations.toArray());
        if (str.startsWith("[,")) str = "["+str.substring(2);
        return str;*/
    }

    public void setAnimation(String animation) {
        animations = new HashSet<String>();
        if (animation != null) {
            if (animation.startsWith(";")) {
                animation.substring(1);
            }
            if (animation.startsWith("[")) {
                animation.substring(1);
            }
            if (animation.endsWith("]")) {
                animation.substring(animation.length() - 1);
            }
            animations.addAll(Arrays.asList(animation.split(";")));
        }
    }

    public String getUserName() {
        return usernameInRequest;
    }

    public void setUserName(String id) {
        usernameInRequest = id;
    }

    public String getBotId() {
        return currentBotId;
    }

    public void setBotId(String logId) {
        currentBotId = logId;
    }

    public int getScore(int scoreMin, int scoreMax) {
        return (int) (scoreCurrent < scoreMin ? scoreMin : scoreCurrent > scoreMax ? scoreMax : scoreCurrent);
    }

    public String getResponse() {
        if (myResponse == null || myResponse.length() == 0) {
            return NO_RESPONSE;
        }
        return myResponse;
    }

    public void setResponse(String response) {
        if (response == null) {
            scoreCurrent = 2;
            response = "";//NO_RESPONSE;
        } else {
            int lastIndex = response.lastIndexOf("mene value=");

            while (lastIndex > 1) {
                String number = response.substring(lastIndex).replace("mene value=", "");
                response = response.substring(0, lastIndex - 1);
                try {
                    scoreCurrent = Double.valueOf(number) * scoreMult;
                } catch (NumberFormatException e) {
                }
                
                lastIndex = response.lastIndexOf("mene value=");
            }
            final String found = "certainty value=\"";
            while (true) {
                int skip = found.length();
                final int f = response.indexOf(found);
                if (f == -1) {
                    break;
                }
                String numbers = response.substring(f).replace(found, "");
                String found2 = "\"";
                int f2 = numbers.indexOf(found2);
                numbers = numbers.substring(0, f2-1);
                skip = numbers.length() + found2.length();
                try {
                    if (numbers.startsWith("+")) {
                        scoreCurrent += Integer.valueOf(numbers.substring(1));
                    } else if (numbers.startsWith("-")) {
                        scoreCurrent -= Integer.valueOf(numbers.substring(1));
                    } else {
                        scoreCurrent = Integer.valueOf(numbers);
                    }
                } catch (NumberFormatException e) {
                    scoreCurrent++;
                }
                response = response.substring(0, f) + " " + response.substring(f + skip);
                response = response.replace("#$", " ").replace("WHATISYOURFAV", "WHAT IS YOUR FAVORITE").replace("WHOISYOURFAV", "WHO IS YOUR FAVORITE").replace(" _", " ");
            }
        }
        myResponse = response;
    }

    public String getInitialRequest() {
        return initialRequest;
    }

    private void log(Throwable e) {
        callback.println(e);
    }

    private void done() {
    }

}

