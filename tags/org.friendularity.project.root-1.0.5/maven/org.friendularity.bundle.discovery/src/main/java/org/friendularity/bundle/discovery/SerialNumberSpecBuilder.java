package org.friendularity.bundle.discovery;

import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;
import com.hp.hpl.jena.rdf.model.Resource;
import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler;
import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader;
import org.appdapter.core.item.Item;
import org.appdapter.core.item.ItemFuncs;
import org.appdapter.core.name.Ident;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */


public class SerialNumberSpecBuilder
    extends CachingComponentAssembler<SerialNumberSpec> {
    public SerialNumberSpecBuilder(Resource builderConfRes) {
        super(builderConfRes);
    }
    
    private final static String theSerialNumberNumber =
            "http://www.friendularity.org/discovery#serialNumberNumber";
    private final static String theRobotType =
            "http://www.friendularity.org/discovery#robotType";
    private final static String theRobotCharacter =
            "http://www.friendularity.org/discovery#robotCharacter";
    private final static String thePhysicalRobot =
            "http://www.friendularity.org/discovery#physicalRobot";
    
    private final static String ROBOT_TYPE_INVALID_WARN =
            "Invalid robot type.";
    
    private final static String CHARACTER_INVALID_WARN =
            "Invalid robot character.";
    
    private final static String PHYSICAL_INVALID_WARN =
            "Invalid physical state.";
    
    @Override
    protected Class<SerialNumberSpec> decideComponentClass(
            Ident ident, Item item) {
        return SerialNumberSpec.class;
    }

    @Override
    protected void initExtendedFieldsAndLinks(
            SerialNumberSpec mkc, Item item, Assembler asmblr, Mode mode) {
        ItemAssemblyReader reader = getReader();
        
        mkc.setSerialNumber(reader.readConfigValString(
                item.getIdent(), theSerialNumberNumber, item, "000001"));
        
        Ident robotTypeIdent = ItemFuncs.getNeighborIdent(item, theRobotType);
        Item robotType =
                item.getSingleLinkedItem(
                        robotTypeIdent, Item.LinkDirection.FORWARD);
        
        if(robotType.getIdent().getLocalName().equals("r25")) {
            mkc.setRobotType(RobotType.R25);
        } else if(robotType.getIdent().getLocalName().equals("r50")) {
            mkc.setRobotType(RobotType.R50);
        } else {
            getLogger().warn(ROBOT_TYPE_INVALID_WARN);
            mkc.setRobotType(RobotType.R25);
        }
        
        Ident characterIdent =
                ItemFuncs.getNeighborIdent(item, theRobotCharacter);
        Item character =
                item.getSingleLinkedItem(
                        characterIdent, Item.LinkDirection.FORWARD);
        
        if(character.getIdent().getLocalName().equals("zeno")) {
            mkc.setCharacter(RobotCharacter.ZENO);
        } else if(character.getIdent().getLocalName().equals("alice")) {
            mkc.setCharacter(RobotCharacter.ALICE);
        } else {
            getLogger().warn(CHARACTER_INVALID_WARN);
            mkc.setCharacter(RobotCharacter.ZENO);
        }
        
        String physical =
                reader.readConfigValString(
                        item.getIdent(), thePhysicalRobot, item, "true");
        
        if(physical.equals("true")) {
            mkc.setPhysical(true);
        } else if(physical.equals("false")) {
            mkc.setPhysical(false);
        } else {
            getLogger().warn(PHYSICAL_INVALID_WARN);
            mkc.setPhysical(true);
        }
    }    
}
