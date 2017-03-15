/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bind.cogbot.service;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.friendularity.bind.cogbot.cogsim.CogSimConf;
import org.friendularity.bind.cogbot.simulator.CogbotAvatar;
import org.friendularity.bind.cogbot.slave.CogbotResponse;

/**
 *
 * @author Administrator
 */
public class CogbotService {

    final public static String cogbot_url_local = "cogbot_url_local";
    final public static String cogbot_binary_folder = "cogbot_binary_folder";
    final public static String cogbot_config_folder = "cogbot_config_folder";
    final public static String cogbot_port_number = "cogbot_port_number";
    final public static String cogsim_enable =  "cogsim_enable";
    
    transient static PrintWriter stdoutput = new PrintWriter(System.err);
    private static Logger theLogger = Logger.getLogger(CogbotService.class.getName());
    transient static CogbotService singleton = new CogbotService();
    public static boolean disableJMX = false;
    final Properties config = new Properties();
    final CogSimConf simConf = new CogSimConf(config);
    transient CogbotAvatar singleAvatar;
    transient boolean isCogbotAvailable = false;
    transient boolean isCogbotLocalChanging = false;
    transient boolean killCogbotLocalOnShutdown = false;
    transient Thread shutDownHook = null;
    transient Process localProcess;
    transient Thread localProcessThread = null;
    static boolean COGBOT_LOCAL_CHECKED = false;
    static boolean COGBOT_EC2_CHECKED = false;
    static boolean COGBOT_LOCAL_CHECKED_AFTER_START = false;

    public static void main(String args) {
    }
    private String portNum;

    public Properties getProperties() {
        return config;
    }

    private static CogbotService getSingleInstance() {
        return singleton;
    }

    public static CogbotAvatar getDefaultAvatar(Properties object) {
        return getInstance(object).getAvatar();
    }

    private CogbotService() {
    }

    private static void echo(String string, Object object) {
        if (object instanceof Throwable) {
            Throwable e = (Throwable) object;
            e.printStackTrace(stdoutput);
            echo(string + e);
            return;
        }
        try {
            echo(string + object);
        } catch (Exception e) {
        }
    }

    private static void echo(String msg) {
        stdoutput.println(msg);
        stdoutput.flush();
    }

    synchronized void startLocalProcess() {
        if (isCogbotLocalChanging) {
            return;
        }
        if (isCogbotAvailable) {
            return;
        }
        shutDownHook = new Thread(new Runnable() {

            public void run() {
                CogbotService.this.killLocalProcessNow();
            }
        });
        Runtime.getRuntime().addShutdownHook(shutDownHook);
        localProcessThread = new Thread(new Runnable() {

            public void run() {
                CogbotService.this.startLocalProcessNow();
            }
        });
        localProcessThread.start();
    }

    synchronized boolean isRunning() {
        if (isCogbotAvailable) {
            return true;
        }
        ensureAvailable();
        return isCogbotAvailable;
    }

    public static synchronized CogbotService getInstance(Properties myProperties) {
        CogbotService service = CogbotService.getSingleInstance();
        service.addConfig(myProperties);
        service.ensureAvailable();
        return service;
    }
    Thread ensureAvail = null;
    final Object starupShutdownLock = new Object();

    void ensureAvailable() {
        synchronized (starupShutdownLock) {
            if (ensureAvail != null) {
                return;
            }
            ensureAvail = new Thread(new Runnable() {

                public void run() {
                    ensureAvailable0();
                    synchronized (starupShutdownLock) {
                        ensureAvail = null;
                    }
                }
            });
            ensureAvail.start();
        }
    }

    synchronized void ensureAvailable0() {
        if (isCogbotAvailable) {
            return;
        }
        if (!COGBOT_LOCAL_CHECKED) {
            COGBOT_LOCAL_CHECKED = true;
            if (cogbotPing("127.0.0.1")) {
                simConf.setIp("127.0.0.1");
                isCogbotAvailable = true;
                return;
            }
        }
        if (!COGBOT_EC2_CHECKED) {
            String ip = config.getProperty(cogbot_url_local, "binabot.gotdns.org");
            COGBOT_EC2_CHECKED = true;
            if (cogbotPing(ip)) {
                simConf.setIp(ip);
                isCogbotAvailable = true;
                return;
            }
        }
        startLocalProcess();

        if (!COGBOT_LOCAL_CHECKED_AFTER_START) {
            COGBOT_LOCAL_CHECKED_AFTER_START = true;
            if (cogbotPing("127.0.0.1")) {
                simConf.setIp("127.0.0.1");
                isCogbotAvailable = true;
                return;
            }
        }

        echo("NO COGBOT FOUND ANYWHERE? - install it to $hanson-root/cogbot/ ");
        echo("  or change cogbot_url_local in the $hanson-root/config/mene/config.properties");
        COGBOT_LOCAL_CHECKED = false;
        COGBOT_EC2_CHECKED = false;
        COGBOT_LOCAL_CHECKED_AFTER_START = false;
    }


    public CogbotResponse getCogbotResponse(CogbotAvatar av, String input, String userName, String botId) {
        return new CogbotResponse(av, stdoutput, config, input, userName, botId);
    }

    public CogbotResponse getCogbotResponse(CogbotAvatar av, PrintWriter servicePw, Properties myProperties, String input, String userName, String botName) {
        if (!isRunning()) {
        }
        return new CogbotResponse(av, servicePw, myProperties, input,  userName, botName);
    }

    public CogbotResponse getCogbotResponse(CogbotAvatar cogbotAvatar, PrintWriter servicePw, Properties myProperties, String input, String theBotId) {
        return getCogbotResponse(cogbotAvatar, servicePw, myProperties, input,null,theBotId);
    }


    public void addConfig(Properties props) {
        if (props == null) {
            return;
        }
        config.putAll(props);
    }

    public void setOutput(PrintWriter servicePw) {
        stdoutput = servicePw;
    }

    public boolean isOnline() {
        return true;
    }

    public void log(Level l, String string, Throwable t) {
        theLogger.log(l, string, t);
    }

    static public Logger getLogger() {
        return theLogger;
    }

    private synchronized CogbotAvatar getAvatar() {
        if (singleAvatar == null) {
            singleAvatar = new CogbotAvatar(this);
            singleAvatar.readProperties(config);
        }
        return singleAvatar;
    }

    public PrintWriter getLogPrintWriter() {
        return stdoutput;
    }

    private boolean cogbotPing(String string) {
        portNum = config.getProperty(cogbot_port_number, "5580");
        try {
            URL url = new URL("http://" + string + ":" + portNum + "/ping");
            try {
                url.openConnection().getInputStream().close();
                echo("COGBOT FOUND AT " + string);
                return true;
            } catch (IOException ex) {
                echo("NO COGBOT FOUND AT " + string);
                return false;
            }
        } catch (MalformedURLException ex) {
            theLogger.getLogger(CogbotService.class.getName()).log(Level.SEVERE, null, ex);
            return false;
        }

    }

    public CogSimConf getConf() {
        return simConf;
    }

    synchronized void startLocalProcessNow() {
        try {
            if (isCogbotLocalChanging) {
                return;
            }
            isCogbotLocalChanging = true;
            if (isCogbotAvailable) {
                return;
            }
            isCogbotAvailable = true;
            echo("// CWD = ", new java.io.File(".").getCanonicalPath());
            String dirString = config.getProperty(cogbot_binary_folder, "C:\\_hanson\\_deploy\\distro_20a\\cogbot");
            File dir = new File(dirString);
            if (!dir.exists()) {
                echo("!Exists dir = " + dir.getAbsolutePath());
                failLocalProcessNow();
                return;
            }
            String cmd = "ABuildStartup.exe"; //cmd.exe /c
            String exec = dir + "\\" + cmd;
            localProcess = Runtime.getRuntime().exec(exec, null, dir);
            isCogbotLocalChanging = false;
            isCogbotAvailable = true;
            simConf.setIp("127.0.0.1");
            String line;

            BufferedReader input =
                    new BufferedReader(new InputStreamReader(localProcess.getInputStream()));
            while ((line = input.readLine()) != null) {
                System.out.println(line);
            }
            localProcess.waitFor();
            input.close();
            localProcess.destroy();
            isCogbotAvailable = false;
        } catch (Exception ex) {
            //ex.printStackTrace(stdoutput);
            stdoutput.println("ERROR Starting process " + ex.getMessage());
            Throwable why = ex.getCause();
            if (why != null) {
                stdoutput.println("BECAUSE " + why.getMessage());
            }
            Logger.getLogger(CogbotService.class.getName()).log(Level.SEVERE, null, ex);
            failLocalProcessNow();
        }
    }

    synchronized void killLocalProcessNow() {
        try {
            if (!killCogbotLocalOnShutdown) {
                return;
            }
            // so we dont change something
            isCogbotLocalChanging = true;
            isCogbotAvailable = false;
            if (localProcess == null) {
                return;
            }
            localProcess.destroy();
            if (localProcessThread != null) {
                localProcessThread.destroy();
            }
        } catch (Exception ex) {
            ex.printStackTrace(stdoutput);
            isCogbotAvailable = false;
            Logger.getLogger(CogbotService.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private void failLocalProcessNow() {
            try {
                localProcess.destroy();
            } catch (Exception e) {
            }
            isCogbotLocalChanging = false;
            isCogbotAvailable = false;
            localProcessThread = null;
            if (shutDownHook != null) {
                Runtime.getRuntime().removeShutdownHook(shutDownHook);
                shutDownHook = null;
            }
    }
}
