package org.friendularity.bind.js;

import java.awt.EventQueue;
import java.io.FileReader;
import java.io.Reader;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import org.jflux.api.common.rk.utils.TimeUtils;
import org.mechio.client.basic.MechIO;

/**
 *
 * @author Amy Jessica Book <jgpallack@gmail.com>
 */
public class ScriptEngineInterpreter implements Interpreter {
    private ScriptEngine myEngine;
    private ControlSystem myController;
    
    public ScriptEngineInterpreter() {
        ScriptEngineManager factory = new ScriptEngineManager();
        myEngine = factory.getEngineByName("JavaScript");
        myController = new ControlSystem();
        populate();
    }
    
    @Override
    public void interpret(final Reader jsSource) {
        myController.setExitingNow(false);
        
        EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                try {
                    myEngine.eval(jsSource);
                } catch(ScriptException ex) {
                    System.out.println(ex.getMessage());
                    ex.printStackTrace();
                }
            }
        });
    }
    
    @Override
    public void stop() {
        myController.setExitingNow(true);
        MechIO.disconnect();
    }
    
    private void populate() {
        try {
            myEngine.put("controlSystem", myController);
            myEngine.eval(new FileReader("lib.js"));
        } catch(Exception ex) {
            System.out.println(ex.getMessage());
            ex.printStackTrace();
        }
    }
    
    public static void main(String[] args) throws Exception {
        Interpreter interpreter = new ScriptEngineInterpreter();
        
        if(args.length > 1) {
            interpreter.interpret(new FileReader(args[1]));
        } else {
            interpreter.interpret(new FileReader("mio.js"));
            TimeUtils.sleep(10000);
            interpreter.stop();
        }
    }
}