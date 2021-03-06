/* CVS $Id: $ */
package org.friendu.otest; 
import com.hp.hpl.jena.rdf.model.*;
 
/**
 * Vocabulary definitions from ../../../cogchar_trunk/src_resources_core/org/cogchar/onto/AnimMotivMapBlend.owl 
 * @author Auto-generated by schemagen on 07 May 2014 16:54 
 */
public class AnimSchema {
    /** <p>The RDF model that holds the vocabulary terms</p> */
    private static Model m_model = ModelFactory.createDefaultModel();
    
    /** <p>The namespace of the vocabulary as a string</p> */
    public static final String NS = "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#";
    
    /** <p>The namespace of the vocabulary as a string</p>
     *  @see #NS */
    public static String getURI() {return NS;}
    
    /** <p>The namespace of the vocabulary as a resource</p> */
    public static final Resource NAMESPACE = m_model.createResource( NS );
    
    public static final Property contains = m_model.createProperty( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#contains" );
    
    public static final Property isContainedBy = m_model.createProperty( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#isContainedBy" );
    
    public static final Resource BodyPart = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#BodyPart" );
    
    public static final Resource BodyRegion = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#BodyRegion" );
    
    public static final Resource Character = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#Character" );
    
    public static final Resource DirectedAngle1D = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#DirectedAngle1D" );
    
    public static final Resource Drive = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#Drive" );
    
    public static final Resource Gesture = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#Gesture" );
    
    public static final Resource NormalRange1D = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#NormalRange1D" );
    
    public static final Resource Point = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#Point" );
    
    public static final Resource Point1D = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#Point1D" );
    
    public static final Resource Range1D = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#Range1D" );
    
    public static final Resource RobotServoJoint = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#RobotServoJoint" );
    
    public static final Resource Signal = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#Signal" );
    
    public static final Resource Space = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#Space" );
    
    public static final Resource Thingo = m_model.createResource( "http://www.cogchar.org/ontologies/y2012m06/AnimMotivMapBlend.owl#Thingo" );
    
}
