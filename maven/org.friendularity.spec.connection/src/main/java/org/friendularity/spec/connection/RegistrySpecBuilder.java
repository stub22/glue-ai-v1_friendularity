package org.friendularity.spec.connection;

import org.appdapter.bind.rdf.jena.assembly.CachingComponentAssembler;
import org.appdapter.core.item.Item;
import org.appdapter.core.name.Ident;

import com.hp.hpl.jena.assembler.Assembler;
import com.hp.hpl.jena.assembler.Mode;

import com.hp.hpl.jena.assembler.assemblers.AssemblerBase;
import com.hp.hpl.jena.rdf.model.Resource;
import java.util.List;
//import java.util.logging.Level;
//import java.util.logging.Logger;
import org.appdapter.bind.rdf.jena.assembly.ItemAssemblyReader;

public class RegistrySpecBuilder extends CachingComponentAssembler<RegistrationSpec> {

    private final String spec = "http://www.friedularity.org/Connection#Object";
    private final String property = "http://www.friedularity.org/Connection#hasProperty";
    private final String registrySpecQN = "http://www.friedularity.org/Connection#registrationQN";
   // private final Logger theLogger=Logger.getLogger(RegistrationSpec.class.getName());

    public RegistrySpecBuilder(Resource builderConfRes) {
        super(builderConfRes);
    }

    @Override
    protected Class<RegistrationSpec> decideComponentClass(Ident ident, Item item) {
        return RegistrationSpec.class;
    }

    @Override
    protected void initExtendedFieldsAndLinks(RegistrationSpec registrationSpec, Item item, Assembler asmblr, Mode mode) {

        
        ItemAssemblyReader reader = getReader();
        List linkedSpecs =
                reader.findOrMakeLinkedObjects(
                item, spec, asmblr, mode, null);
        
        if(linkedSpecs.isEmpty())
        {
            throw new IllegalArgumentException("No spec objects found.");
            //theLogger.Log(Level.SEVERE,"No spec objects found.", null);
        }
        else
        {
           // System.out.println("Now Printing Object properties");
           // System.out.println(((ConnectionSpec)linkedSpecs.get(0)).getIpAddress());
            registrationSpec.setSpec((Object)linkedSpecs.get(0));
        }
        
        //registrationSpec.setSpec(reader.readConfigValString(item.getIdent(), spec, item, ""));
        //registrationSpec.addProperty("", "");
        registrationSpec.setQN(reader.readConfigValString(item.getIdent(), registrySpecQN, item, ""));
        //System.out.println(registrationSpec.getQN());
        List linkedProperties =
                reader.findOrMakeLinkedObjects(
                item, property, asmblr, mode, null);
      //  int i=0;
        //System.out.println(linkedProperties.size());
        for (Object prop : linkedProperties) {
            if (prop instanceof PropertySpec) {
                PropertySpec propertySpec = (PropertySpec) prop;
                //System.out.println("Reg Spec Loop. "+propertySpec.toString());
               // System.out.println(propertySpec.getName());
             //   System.out.println(propertySpec.getValue());
                registrationSpec.addProperty( propertySpec.getName(), propertySpec.getValue());
           //     System.out.println("After put method.");
            }
         //   i++;
       //     System.out.println("Loop iteration "+i);
        }
     //   System.out.println("Now leaving RegSpecBuilder");

   }
}