/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.demo.owlbind;

import org.friendularity.gen.reacted.btest.OwnsOtherThings;
import org.ontoware.rdf2go.impl.jena.ModelFactoryImpl;
import org.ontoware.rdf2go.impl.jena.ModelImplJena;
import org.ontoware.rdf2go.model.Model;

/**
 * @author Stu B. <www.texpedient.com>
 * 
 * Have not yet figured out any tricks for ordered collections or sort-key
 * 
 * Working primitive types from XSD:  integer, long, double, float, boolean, DateTime (=java.util.Calendar)
 * 
 * Failing primitive types:  XMLLiteral, DateTimeStamp, Decimal, Rational, Real
 */

public class TestReactorWrappers {
	public static void main(String[] args) {
		System.out.println("Testing Reactor Wrappers");
		
		try {
			Model freshModel = new ModelFactoryImpl().createModel();
			// Model m = new ModelImplJena(null);
			boolean doWrites = false;
			OwnsOtherThings oot_01 = new OwnsOtherThings(freshModel, doWrites);
			System.out.println("oot=" + oot_01);
			System.out.println("model=" + freshModel);

		} catch (Throwable t) {
			t.printStackTrace();
		}
	}
}
