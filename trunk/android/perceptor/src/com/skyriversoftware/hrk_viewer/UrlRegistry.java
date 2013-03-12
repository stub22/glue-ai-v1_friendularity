package com.skyriversoftware.hrk_viewer;

public class UrlRegistry {
	// If we want defaults in the production Proctor, we should set these in XML - actually, why the heck aren't they now?
	// Well for one, it's easier to switch back and forth like this for now!
	private static String mainUrl = "http://lima.nodeset.com:8080/test_lifter";
	//private static String mainUrl = "http://192.168.1.55:8080/test_lifter";
	private static String pushyUrl = "http://lima.nodeset.com:8080/test_lifter"; // Currently unused for viewer
	private static String speechPath = "/speech";

	
	static void setMainUrl(String url) {
		mainUrl = url;
	}
	static String getMainUrl() {
		return mainUrl;
	}
	
	static void setPushyUrl(String url) {
		pushyUrl = url;
	}
	static String getPushyUrl() {
		return pushyUrl;
	}
	
	static void setSpeechPath(String path) {
		speechPath = path;
	}
	static String getSpeechUrl() {
		return mainUrl + speechPath;
	}
	
	static String getSpeechPath() {
		return speechPath;
	}
}
