package org.friendularity.bundle.discovery;

import org.appdapter.bind.rdf.jena.assembly.KnownComponentImpl;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */

public class SerialNumberSpec extends KnownComponentImpl {
    private String mySerialNumber;
    private RobotType myRobotType;
    private RobotCharacter myCharacter;
    private Boolean myPhysical;
    
    public String getSerialNumber() {
        return mySerialNumber;
    }
    
    public void setSerialNumber(String serialNumber) {
        mySerialNumber = serialNumber;
    }
    
    public RobotType getRobotType() {
        return myRobotType;
    }
    
    public void setRobotType(RobotType robotType) {
        myRobotType = robotType;
    }
    
    public RobotCharacter getCharacter() {
        return myCharacter;
    }
    
    public void setCharacter(RobotCharacter character) {
        myCharacter = character;
    }
    
    public Boolean isPhysical() {
        return myPhysical;
    }
    
    public void setPhysical(Boolean physical) {
        myPhysical = physical;
    }
    
    public String getBroadcastId() {
        StringBuilder sb = new StringBuilder();
        sb.append(myCharacter == RobotCharacter.ZENO ? "ZENO" : "ALICE");
        sb.append("-");
        sb.append(myRobotType == RobotType.R25 ? "R25" : "R50");
        sb.append("-");
        sb.append(myPhysical ? "ROBOT" : "AVATAR");
        sb.append("-");
        sb.append(mySerialNumber);
        
        return sb.toString();   
    }
}
