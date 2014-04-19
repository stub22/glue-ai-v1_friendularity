/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bind.weber.cogsim;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;

/**
 * @author Stu Baurmann
 */
public class OldTestCogSimInline {

	private static Logger theLogger = Logger.getLogger(OldTestCogSimInline.class.getName());
	static public String urlRoot = "http://binabot.gotdns.org:5580/";
	static public String saidURL = urlRoot + "posterboard/onchat-said";
	static public String heardURL = urlRoot + "posterboard/onchat-heard";
	static public String actionURL = urlRoot + "postaction";

	public static void postMessageToCogbot(HttpClient client,
				String cmd, String args) throws Throwable {
		theLogger.info("Posting to URL: " + actionURL);
		HttpPost postReq = new HttpPost(actionURL);
	/* This is for GET params and for params processed by HttpClient
		HttpParams params = new BasicHttpParams();
		params.setParameter("cmd", cmd);
		params.setParameter("args", args);
		params.setParameter("submit", "Do it CogBot!");
		theLogger.info("Post Params: " + params);
		postReq.setParams(params);
	 */
        List <NameValuePair> nvps = new ArrayList <NameValuePair>();
        nvps.add(new BasicNameValuePair("cmd", cmd));
        nvps.add(new BasicNameValuePair("args", args));
		UrlEncodedFormEntity formEntity = new UrlEncodedFormEntity(nvps, HTTP.UTF_8);
		postReq.setEntity(formEntity);
		Header[] allHeaders = postReq.getAllHeaders();
		theLogger.info("POST header count: " + allHeaders.length);
		for (Header h : allHeaders) {
			theLogger.info("Header: " + h);
		}
		theLogger.info("Request method: " + postReq.getMethod());
		theLogger.info("Request line: " + postReq.getRequestLine());
		HttpResponse response = client.execute(postReq);
		String summary = "Posted [cmd=" + cmd + ", args=" + args + "]";
		dumpResponseInfo(response, summary);
	}

	public static String getLastThingWeSaid(HttpClient client) throws Throwable {
		String thingWeSaid = null;
		HttpGet getReq = new HttpGet(saidURL);
		HttpResponse response = client.execute(getReq);
		dumpResponseInfo(response, "get last thing said");
		return null;
	}
	public static String getLastThingWeHeard(HttpClient client) throws Throwable {
		String thingWeHeard = null;
		HttpGet getReq = new HttpGet(heardURL);
		HttpResponse response = client.execute(getReq);
		dumpResponseInfo(response, "get last thing heard");
		return thingWeHeard;
	}
	public static void dumpResponseInfo (HttpResponse response, String rqSummary)
				throws Throwable {
		theLogger.info("RequestSummary: " + rqSummary);
		if (response != null) {
			theLogger.info("Response status line: " + response.getStatusLine());
			HttpEntity resEntity = response.getEntity();
			if (resEntity != null) {
				theLogger.info("Got response entity: " + resEntity);
				theLogger.info("Response content length: " + resEntity.getContentLength());
				theLogger.info("Chunked?: " + resEntity.isChunked());
				theLogger.info(EntityUtils.toString(resEntity));
				resEntity.consumeContent();
			} else {
				theLogger.warning("No entity attached to response to request: " + rqSummary);
			}
		} else {
			theLogger.warning("Got null response to request: " + rqSummary);
		}
	}
	public static void main(String args[]) {
		try {
			HttpClient client = new DefaultHttpClient();
			String message = "mene sez the time is: " + System.currentTimeMillis();
			theLogger.info("Saying: [" + message + "]");
			postMessageToCogbot(client, "say", message);		
			client = new DefaultHttpClient();
			String lastSaid = getLastThingWeSaid(client);
			theLogger.info("Last thing we said: " + lastSaid);
			String lastHeard = getLastThingWeHeard(client);
			theLogger.info("Last thing we heard: " + lastHeard);
		} catch (Throwable t) {
			t.printStackTrace();
		}
	}

	/*
	HttpHost target = new HttpHost("www.apache.org", 80, "http");

	// general setup
	SchemeRegistry supportedSchemes = new SchemeRegistry();

	// Register the "http" protocol scheme, it is required
	// by the default operator to look up socket factories.
	supportedSchemes.register(new Scheme("http",
	PlainSocketFactory.getSocketFactory(), 80));

	// prepare parameters
	HttpParams params = new BasicHttpParams();
	HttpProtocolParams.setVersion(params, HttpVersion.HTTP_1_1);
	HttpProtocolParams.setContentCharset(params, "UTF-8");
	HttpProtocolParams.setUseExpectContinue(params, true);

	ClientConnectionManager connMgr = new ThreadSafeClientConnManager(params,
	supportedSchemes);
	DefaultHttpClient httpclient = new DefaultHttpClient(connMgr, params);

	HttpGet req = new HttpGet("/");

	System.out.println("executing request to " + target);

	HttpResponse rsp = httpclient.execute(target, req);
	HttpEntity entity = rsp.getEntity();

	System.out.println("----------------------------------------");
	System.out.println(rsp.getStatusLine());
	Header[] headers = rsp.getAllHeaders();
	for (int i = 0; i < headers.length; i++) {
	System.out.println(headers[i]);
	}
	System.out.println("----------------------------------------");

	if (entity != null) {
	System.out.println(EntityUtils.toString(entity));
	}

	// When HttpClient instance is no longer needed,
	// shut down the connection manager to ensure
	// immediate deallocation of all system resources
	httpclient.getConnectionManager().shutdown();
	 */
}
