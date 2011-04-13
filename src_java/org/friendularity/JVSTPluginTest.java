/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity;

import jvst.wrapper.VSTPluginAdapter;
/**
 *
 * @author winston
 */
public class JVSTPluginTest extends VSTPluginAdapter {

	public JVSTPluginTest(long wrapperNum) {
	    super(wrapperNum);
	    log("Construktor JVSTPluginTest(" + wrapperNum + ") is loggink to you now!");
	}
	@Override public int getPlugCategory() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	@Override public int canDo(String string) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	@Override public boolean setBypass(boolean bln) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public boolean string2Parameter(int i, String string) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getProgramNameIndexed(int i, int i1) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getProductString() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getVendorString() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public void setParameter(int i, float f) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public float getParameter(int i) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public void processReplacing(float[][] floats, float[][] floats1, int i) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public int getProgram() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public void setProgram(int i) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public void setProgramName(String string) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getProgramName() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getParameterName(int i) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getParameterDisplay(int i) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public String getParameterLabel(int i) {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public int getNumPrograms() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	public int getNumParams() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

}
