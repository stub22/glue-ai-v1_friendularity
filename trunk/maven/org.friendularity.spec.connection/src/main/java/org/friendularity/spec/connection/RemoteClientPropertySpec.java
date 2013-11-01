
package org.friendularity.spec.connection;

import org.appdapter.bind.rdf.jena.assembly.KnownComponentImpl;

/**
 *
 * @author Major Jacquote II <mjacquote@gmail.com>
 */


public class RemoteClientPropertySpec extends KnownComponentImpl {
    
    private String speechServiceId;
    private String remoteId;
    
    
    public RemoteClientPropertySpec(){}
    
    public void setSpeechServiceId(String speechServiceId)
    {
        this.speechServiceId=speechServiceId;
    }
    
    public void setRemoteId(String remoteId)
    {
        this.remoteId=remoteId;
    }
    
    public String getSpeechServiceId()
    {
        return speechServiceId;
    }
    
    public String getRemoteId()
    {
        return remoteId;
    }
}
