package org.friendularity.bind.js;

import java.io.Reader;

/**
 *
 * @author Amy Jessica Book <jgpallack@gmail.com>
 */
public interface Interpreter {
    public void interpret(Reader jsSource);
    public void stop();
}
