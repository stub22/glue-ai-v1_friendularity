package org.friendularity.spec.connection;

import org.appdapter.bind.rdf.jena.assembly.KnownComponentImpl;



public class AnimationLibrarySpec extends KnownComponentImpl{
    
    private String libraryId;
    
    public AnimationLibrarySpec()
    {    
    }
    
    public String getId()
    {
        return libraryId;
    }
    
    public void setId(String id)
    {
        libraryId=id;
    }
    
    
    
    
}
