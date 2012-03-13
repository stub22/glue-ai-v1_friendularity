/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.weber.utils;

import org.friendularity.weber.services.GenRespWithConf;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author matt
 */
public class Utils {
    
    public static <T> List<T> list(T ...items){
		List<T> myList = new ArrayList<T>();
		for(Object o : items){
			myList.add((T)o);
		}
		return myList;
	}
    public static GenRespWithConf getBest(List<GenRespWithConf> askResponses){
        GenRespWithConf best = null;
        for(GenRespWithConf re : askResponses){
            if(best == null || re.getConfidence() > best.getConfidence()){
                best = re;
            }
        }
        if(best == null){
            best = new GenRespWithConf("", -1);
        }
        return best;
    }

	public static void println(String s){
		System.out.println(s);
	}

    public static String plaintext(String s){
        return s.replaceAll("[^a-zA-Z0-9 .,:?$@!']+", " ").replaceAll("\\s+", " ");
    }

    public static String stripTags(String s){
        return s.replaceAll("<[^>]*>", "");
    }

    public static String silence(int msec){
        return "<silence msec=\"" + msec + "\"/> ";
    }
}
