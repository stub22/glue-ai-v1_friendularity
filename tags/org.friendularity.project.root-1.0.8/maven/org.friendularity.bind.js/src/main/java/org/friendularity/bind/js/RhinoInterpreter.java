package org.friendularity.bind.js;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import org.jflux.api.common.rk.utils.TimeUtils;
import org.mechio.client.basic.MechIO;
import sun.org.mozilla.javascript.Context;
import sun.org.mozilla.javascript.ImporterTopLevel;
import sun.org.mozilla.javascript.Scriptable;

/**
 *
 * @author Amy Jessica Book <jgpallack@gmail.com>
 */
public class RhinoInterpreter implements Interpreter {
    private Context myContext;
    private Scriptable myScope;
    private ControlSystem myController;
    
    public RhinoInterpreter() {
        myContext = Context.enter();
        myScope = new ImporterTopLevel(myContext);
        myController = new ControlSystem();
        populate();
    }
    
    @Override
    public void interpret(final Reader jsSource) {
        myController.setExitingNow(false);
        
        Thread evalThread = new Thread(new Runnable() {
            @Override
            public void run() {
                Context myContext = Context.enter();
                try {
                    myContext.evaluateReader(
                            myScope, jsSource, "<script>", 1, null);
                } catch(IOException ex) {
                    System.out.println(ex.getMessage());
                    ex.printStackTrace();
                }

                Context.exit();
            }
        });
        
        evalThread.start();
    }
    
    @Override
    public void stop() {
        myController.setExitingNow(true);
        MechIO.disconnect();
    }
    
    private void populate() {
        try {
            myScope.put("controlSystem", myScope, myController);
            myContext.evaluateReader(
                    myScope, new FileReader("lib.js"), "lib.js", 1, null);
        } catch(IOException ex) {
            System.out.println(ex.getMessage());
            ex.printStackTrace();
        }
    }
    
    public static void main(String[] args) throws Exception {
        Interpreter interpreter = new RhinoInterpreter();
        
        if(args.length > 1) {
            interpreter.interpret(new FileReader(args[1]));
        } else {
            interpreter.interpret(new FileReader("mio.js"));
            TimeUtils.sleep(10000);
            interpreter.stop();
        }
    }
}