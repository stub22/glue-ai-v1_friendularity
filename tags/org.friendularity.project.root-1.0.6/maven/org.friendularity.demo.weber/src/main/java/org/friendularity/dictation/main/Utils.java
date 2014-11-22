/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.main;

import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

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
	public static <T> T[] array(T ...items){
		return items;
	}

	public static Properties loadProperties(String path){
		Properties props = new Properties();
		try{
			FileReader fr = new FileReader(new File(path));
			props.load(fr);
		}catch(Throwable ex){
			ex.printStackTrace();
		}
		return props;
	}
}
