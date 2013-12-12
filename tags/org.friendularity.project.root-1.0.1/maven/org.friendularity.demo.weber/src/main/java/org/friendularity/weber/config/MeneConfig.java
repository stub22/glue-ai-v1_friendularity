/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.weber.config;

import org.friendularity.bind.weber.jmx.JMXInterface;
import org.friendularity.bind.weber.rss.RSSConfig;
import java.io.FileInputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author matt
 */
public class MeneConfig {
	private static Logger theLogger = Logger.getLogger(MeneConfig.class.getName());
	private Map<String, IStringFormatter> formatters;
	private String reset_phrase;
	private String elbot_url_local;
	private String elbot_url_remote;
	private String cogbot_url_local;
	private String serviceURL = "service:jmx:rmi:///jndi/rmi://localhost:7227/jmxrmi";
    private String myLogDirectory=null;
    private String batch_timeout;
    
    public IStringFormatter getFormatter(String key){
        if(!formatters.containsKey(key)){
            return new EmptyFormatter();
        }
        return formatters.get(key);
    }

    public void load_configuration(String propsPath){
        loadConfigPropsAndVars(propsPath, false);
    }
	public static Properties readPropertiesFile(String propsPath) {
		Properties props = new Properties();
		FileInputStream propsFile = null;
		try {
			propsFile = new FileInputStream(propsPath);
			props.load(propsFile);
		} catch (Throwable t) {
			theLogger.log(Level.SEVERE, "Can't read propsFile: " + propsPath, t);
		} finally {
			if (propsFile != null) {
				try {
					propsFile.close();
				} catch (Throwable t) {
					theLogger.log(Level.SEVERE, "Can't close propsFile " + propsFile, t);
				}
			}
		}
		return props;
	}
	public void loadConfigPropsAndVars(String propsPath, boolean force) {
		if (formatters != null && !force) {
			return;
		} 
		Properties props = readPropertiesFile(propsPath);
		load_configuration(props);
	}

	public void load_configuration(Properties config) {
		try {
			reset_phrase = config.getProperty("reset_phrase");
			elbot_url_local = config.getProperty("elbot_url_local");
			elbot_url_remote = config.getProperty("elbot_url_remote");
			cogbot_url_local = config.getProperty("cogbot_url_local");
			serviceURL = config.getProperty("character_engine_jmx_url");
			JMXInterface.setURL(serviceURL);
			myLogDirectory = config.getProperty("log_directory");
            batch_timeout = config.getProperty("batch_timeout");
			formatters = new HashMap<String, IStringFormatter>();
			String[] categories = { "input", "id", "logId", "animation", "str",
					"animation_list", "states", "ask_replace" };
			for (String key : categories) {
				String path = config.getProperty(key + "_path");
				formatters.put(key, ReplaceFormatter.loadFromFile(path));
			}
			String[] question_cats = { "question" };
			for (String key : question_cats) {
				String path = config.getProperty(key + "_path");
				formatters.put(key, QuestionFormatter.loadFromFile(path));
			}
			String[] random_cats = { "partner" };
			for (String key : random_cats) {
				String path = config.getProperty(key + "_path");
				formatters.put(key, RandomFormatter.loadFromFile(path));
			}
            RSSConfig.init(config.getProperty("rss_feeds_path"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

    public String getElbotUrlLocal() {
        return elbot_url_local;
    }

    public String getCogbotUrlLocal() {
        return cogbot_url_local;
    }

    public String getElbotUrlRemote() {
        return elbot_url_remote;
    }

    public String getResetPhrase() {
        return reset_phrase;
    }

    public String getServiceURL() {
        return serviceURL;
    }

    public String getLogDirectory(){
        return myLogDirectory;
    }

    public String getBatchTimeout(){
        return batch_timeout;
    }
}
