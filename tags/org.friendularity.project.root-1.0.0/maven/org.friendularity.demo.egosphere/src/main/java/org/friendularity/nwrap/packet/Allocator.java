/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.nwrap.packet;

import java.nio.ByteBuffer;

import java.util.List;
import java.util.ArrayList;


/**
 *
 * @author humankind
 */
public class Allocator {
	public enum	 Style {
		FREED_BY_JAVA,
		FREED_BY_NATIVE
	}
	
	private	static	List<Style>		theStyleList;
	public static List<Style> getAllocatorStyleEnumList() {
		if (theStyleList == null) {
			theStyleList = new ArrayList<Style>();
			// Order here must be the same as order in the Enum decl, and 
			// as the order of integer constants on the C++ side.
			theStyleList.add(Style.FREED_BY_JAVA);
			theStyleList.add(Style.FREED_BY_NATIVE);
		}
		return theStyleList;
	}
}
