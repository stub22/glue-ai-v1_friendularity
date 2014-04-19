package org.friendularity.jvision.gui;

import java.io.File;

/*
 * Tacky method of making it easier to move things about
 * 
 * presumably does something more sophisticated someday
 * 
 */
public class FileLocations {
	
	public enum CascadeType { 
		FRONTAL_CASCADE,
	    PROFILE_CASCADE,
		GLASSES_CASCADE,
		BANANA_CASCADE
	};
	
	public static final String imageBase() {
    return "src\\main\\resources\\opencv\\testimages";
	}
	
	public static final String cascadeTempfileName(CascadeType ct)
	{
		// data struct when this gets big enough
		switch(ct){
			case	FRONTAL_CASCADE: return ".lbpcascade_frontalface.xml";
		    case	PROFILE_CASCADE: return ".lbpcascade_profileface.xml";
			case	GLASSES_CASCADE: return ".haarcascade_eye.xml";
		    case	BANANA_CASCADE: return ".banaacascade.xml";
			default: throw new IllegalArgumentException("wrong cascade type?");
		}
	}
	
	public static final String cascadeResourceLocation(CascadeType ct)
	{
		// data struct when this gets big enough
		switch(ct){
			case	FRONTAL_CASCADE: return "/opencv/lbpcascades/lbpcascade_frontalface.xml";
		    case	PROFILE_CASCADE: return "/opencv/lbpcascades/lbpcascade_profileface.xml";
		    case	GLASSES_CASCADE: return "/opencv/haarcascades/haarcascade_eye.xml";
			case	BANANA_CASCADE:  return "/opencv/haarcascades/banana_classifier.xml";
			default: throw new IllegalArgumentException("wrong cascade type?");
		}
	}
	
	
}
