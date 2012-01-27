/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.freckle;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;
import com.thoughtworks.xstream.io.xml.Dom4JWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.cogchar.xml.convoid.behavior.BehaviorDataSaver;
import org.dom4j.Document;
import org.dom4j.DocumentHelper;

/**
 *
 * @author Stu Baurmann
 */
public class FreckleFile {
	private static Logger theLogger = Logger.getLogger(FreckleFile.class.getName());
	
	private		List<FreckleFace>		myFaces;
	
	public FreckleFile() {
		completeInit();
	}
	public void completeInit() {
		if (myFaces == null) {
			myFaces = new ArrayList<FreckleFace>();
		}
		for (FreckleFace ff: myFaces) {
			ff.completeInit();
		}
	}
	public void addFace(FreckleFace ff) {
		myFaces.add(ff);
	}
	public List<FreckleFace> getFaceList() {
		return myFaces;
	}
	public static FreckleFile load(String filename) throws Throwable {
		FreckleFile		ffile = null;
		XStream xstream = buildDom4jXStreamForRead();
		FileReader fread = new FileReader(filename);
		ffile = (FreckleFile) xstream.fromXML(fread);
		ffile.completeInit();
		return ffile;		
	}
	public void save(String filename) throws Throwable {
		Document ffDoc = writeToDom4JDoc();
		// theLogger.finer("ffdoc=" + ffDoc.asXML());
		FileWriter fw = new FileWriter(filename);
		BehaviorDataSaver.writeDocument(ffDoc, fw);
		fw.close();
	}
	public Document writeToDom4JDoc() {
		// dom4JDriver produces a Dom4JXmlWriter, which cannot write mixedContent.
		// dom4jDriver.setOutputFormat(dom4jOutputFormat);
		XStream xstream = new XStream();
		Document targetDoc = DocumentHelper.createDocument();
		initFreckleFileXStream(xstream);
		Dom4JWriter d4jWriter = new Dom4JWriter(targetDoc);
		xstream.marshal(this, d4jWriter);
		return targetDoc;
	}	
	public static XStream buildDom4jXStreamForRead() {
		Dom4JDriver dom4jDriver = new Dom4JDriver();
		XStream xstream = new XStream(dom4jDriver);
		initFreckleFileXStream(xstream);
		return xstream;
	}
	public static void initFreckleFileXStream(XStream xstream) {
		xstream.alias("FreckleFile", FreckleFile.class);
		xstream.alias("FreckleFace", FreckleFace.class);
		xstream.addImplicitCollection(FreckleFile.class, "myFaces", FreckleFace.class);
		xstream.useAttributeFor(FreckleFace.class, "freckleID");
		xstream.useAttributeFor(FreckleFace.class, "personName");
	}	
}
