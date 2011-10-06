/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bind.cogbot.scripting;

import java.io.Serializable;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

public class SerialEventQueue implements Serializable {

    transient private final int nThreads;
    transient private final PoolWorker[] threads;
    transient private final LinkedList queue;
    transient Thread serialPing;
    transient boolean pingQueued = false;
    final String name;
    
    public SerialEventQueue(String name0) {
        this(name0, 1);
    }

    public void echo(String string) {
        System.err.println("----" + toString() + "----: " + string);
    }

    public SerialEventQueue(String name0,int nThreads) {
        this.name = name0;
        this.nThreads = nThreads;
        queue = new LinkedList();
        threads = new PoolWorker[nThreads];

        for (int i = 0; i < nThreads; i++) {
            threads[i] = new PoolWorker("PoolWorker "+ name + " #" + i);
            threads[i].start();
        }
        startPing();
    }

    public void invokeLater(Runnable r) {
        synchronized (queue) {
            queue.addLast(r);
            queue.notify();
        }
    }

    private void startPing() {
        serialPing = new Thread(new Runnable() {

            public void run() {
                try {
                    while (true) {
                        Thread.sleep(30000);
                        if (pingQueued) {
                            echo("Ping still QueueD! size=" + queue.size());
                            continue;
                        }
                        pingQueued = true;
                        SerialEventQueue.this.invokeLater(new Runnable() {
                            final long createdms = System.currentTimeMillis();
                            public void run() {
                                pingQueued = false;
                                long lag = System.currentTimeMillis() - createdms;
                                if (lag > 10) {
                                    echo("Lag was " + lag + "ms");
                                }
                            }
                        });
                    }

                } catch (InterruptedException ex) {
                    Logger.getLogger(SerialEventQueue.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }, "PoolPinger " + name);
        serialPing.start();
    }

    public void stop() {
       killThread(serialPing);
        for (int i = 0; i < nThreads; i++) {
            killThread(threads[i]);
            threads[i] = null;
        }
    }

    static void killThread(Thread t) {
        if (t == null) {
            return;
        }
        if (!t.isAlive()) {
            return;
        }
        try {
            t.stop();
        } catch (Throwable th) {
        }
        if (!t.isAlive()) {
            return;
        }
        try {
            t.interrupt();
        } catch (Throwable th) {
        }
        if (!t.isAlive()) {
            return;
        }
        try {
            t.destroy();
        } catch (Throwable th) {
        }
    }

    private class PoolWorker extends Thread {

        PoolWorker(String name) {
            super(name);
        }
        public void run() {
            Runnable r;
            while (true) {
                try {
                    synchronized (queue) {
                        while (queue.isEmpty()) {
                            try {
                                queue.wait();
                            } catch (InterruptedException ignored) {
                            }
                        }

                        r = (Runnable) queue.removeFirst();
                    }
                    // If we don't catch RuntimeException,
                    // the pool could leak threads
                    try {
                        r.run();
                    } catch (RuntimeException e) {
                        // You might want to log something here
                        e.printStackTrace();
                    } catch (Throwable e) {
                        // You might want to log something here
                        e.printStackTrace();
                    }
                } catch (Throwable te) {
                    // You might want to log something here
                    te.printStackTrace();
                }
            }
        }
    }
}
