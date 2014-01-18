package org.friendularity.bundle.demo.gmteach;

import java.awt.Component;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.swing.AbstractButton;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

import org.cogchar.svc.behav.control.ActionCallbackMap;
import org.friendularity.bundle.demo.gmteach.ListenerMap.LEntry;
import org.jflux.api.core.Listener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ext.osgi.common.MacroBundleActivatorBase.ListenableMap;

public class RunnablesPanel extends javax.swing.JPanel implements ActionCallbackMap, PropertyChangeListener, Listener<String> {
	public final class PanelWorker extends SwingWorker<Void, Runnable> {
		private final Vector<Runnable> todoList;

		//final JProgressBar progressBar = new JProgressBar(0, 100);

		private PanelWorker(Vector<Runnable> todoList) {
			this.todoList = todoList;
		}

		public Void doInBackground() {

			Runnable number;
			while (!isCancelled()) {
				number = (Runnable) todoList.remove(0);
				publish(number);
				setProgress(100 * (1 / (1 + todoList.size())));
			}
			return null;
		}

		// Can safely update the GUI from this method.
		protected void done() {

			try {
				// Retrieve the return value of doInBackground.
				get();

			} catch (Throwable e) {
				// This is thrown if we throw an exception
				// from doInBackground.
			}
		}

		@Override// Can safely update the GUI from this method.
		protected void process(List<Runnable> chunks) {
			for (Runnable r : chunks) {
				try {
					// Here we receive the values that we publish().
					// They may come grouped in chunks.
					//int mostRecentValue = chunks.get(chunks.size() - 1);

					//countLabel1.setText(Runnable.toString(mostRecentValue));
					r.run();
				} catch (Exception e) {
					getLogger().error("" + this, e);
				}
			}
		}
	}

	public final static String PROP_TRIGGER_PANEL_ID = "runnablesPanelID";

	protected Logger myLogger;

	protected Logger getLogger() {
		if (myLogger == null) {
			myLogger = LoggerFactory.getLogger(getClass());
		}
		return myLogger;
	}

	final protected Map<String, Component> myButtonMap = new HashMap<String, Component>();
	ListenableMap<String, Runnable> myRunnablesMap;

	@Override public void handleEvent(String input) {
		synchronized (myButtonMap) {
			childChanged(myButtonMap.get(input));
		}
	}

	/** Creates new form TriggerPanel */
	public RunnablesPanel() {
		initComponents();
	}

	public void addListenableMap(ListenableMap<String, Runnable> lm) {
		synchronized (myButtonMap) {
			myRunnablesMap = lm;
			synchronized (myRunnablesMap) {
				myRunnablesMap.addPropertyChangeListener(this);
				for (Object o : myRunnablesMap.entrySet()) {
					Map.Entry e = (Map.Entry) o;
					putRunnableButton((String) e.getKey(), (Runnable) e.getValue());
				}
			}
		}

	}

	private void putRunnableButton(String name, final Runnable listener) {
		final Component b = newJButton(name, new ActionListener() {
			@Override public void actionPerformed(ActionEvent e) {
				listener.run();
			}
		});
		synchronized (myButtonMap) {
			myButtonMap.put(name, b);
		}
		addButton(b);
	}

	protected Component newJButton(final String name, final ActionListener listener) {
		JButton button = new JButton(name) {
			@Override public boolean isEnabled() {
				return myRunnablesMap.get(name) != null;
			}

			@Override protected void fireActionPerformed(ActionEvent event) {
				try {
					setEnabled(false);
					super.fireActionPerformed(event);
				} finally {
					setEnabled(true);
					childChanged(this);
				}
			}

		};
		button.addActionListener(listener);
		return button;
	}

	@Override public void propertyChange(PropertyChangeEvent evt) {
		Object oldv = evt.getOldValue();
		if (!(oldv instanceof LEntry))
			return;
		Object src = evt.getSource();
		if (src == this)
			return;
		String pname = evt.getPropertyName();
		if (pname == null || !ListenableMap.PROP_PUT.equals(pname))
			return;
		LEntry oldE = (LEntry) oldv;
		String name = "" + oldE.getKey();
		Object newv = evt.getNewValue();
		if (newv == null) {
			removeActionCallback(name);
			return;
		}
		LEntry newE = (LEntry) newv;
		Object value = newE.getValue();
		putRunnableButton(name, (Runnable) value);
	}

	@Override public void putActionCallback(final String name, final ActionListener listener) {
		final Component b = newJButton(name, listener);
		myRunnablesMap.putNoFire(name, new Runnable() {
			@Override public void run() {
				listener.actionPerformed(new ActionEvent(this, -1, name));
			}
		});
		myButtonMap.put(name, b);
		addButton(b);
	}

	private void addButton(final Component b) {
		SwingUtilities.invokeLater(new Runnable() {
			@Override public void run() {
				add(b);
				changed();
			}
		});
	}

	public static void invokeAndWait(Runnable doRun) {
		if (EventQueue.isDispatchThread()) {
			doRun.run();
			return;
		}
		try {
			SwingUtilities.invokeAndWait(doRun);
		} catch (InterruptedException e) {
			e.printStackTrace();
			throw new RuntimeException(e);
		} catch (InvocationTargetException e) {
			e.printStackTrace();
			throw new RuntimeException(e);
		}

	}

	public void invokeInQueue(final Runnable doRun) {
		final Vector<Runnable> todoList = new Vector();
		todoList.add(doRun);
		PanelWorker worker = new PanelWorker(todoList);
		worker.execute();
	}

	public void changed() {
		for (String s : getActionKeys()) {
			Component c = getActionComponent(s);
			updateChildInfo(c);
		}
		invokeAndWait(new Runnable() {
			@Override public void run() {
				revalidate();
				repaint();
			}
		});
	}

	public void childChanged(Component child) {
		changed();
	}

	protected void updateChildInfo(Component c) {
		// TODO Auto-generated method stub

	}

	public Component getActionComponent(String actionCallbackName) {
		final Component c;
		synchronized (myButtonMap) {
			c = myButtonMap.get(actionCallbackName);
		}
		return c;
	}

	@Override public void removeActionCallback(String name) {
		if (myRunnablesMap != null) {
			synchronized (myRunnablesMap) {
				myRunnablesMap.removeNoFire(name);
			}
		}
		final Component b;
		synchronized (myButtonMap) {
			b = myButtonMap.remove(name);
		}
		if (b != null) {
			invokeAndWait(new Runnable() {
				@Override public void run() {
					remove(b);
				}
			});
		}
	}

	/**
	 * This method is called from within the constructor to initialize the form. WARNING: Do NOT modify this code. The content of this method is always regenerated by the Form Editor.
	 */
	@SuppressWarnings("unchecked") private void initComponents() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
	}

	@Override public ActionListener getActionCallback(String actionCallbackName) {
		ActionListener result = null;
		final Component c = getActionComponent(actionCallbackName);
		if (c instanceof AbstractButton) {
			AbstractButton b = (AbstractButton) c;
			ActionListener allAL[] = b.getActionListeners();
			if (allAL != null) {
				if (allAL.length == 1) {
					return allAL[0];
				}
			}
		}
		return null;
	}

	public void addActionListener(Listener<String> listener) {
	}

	public void removeActionListener(Listener<String> listener) {
	}

	public void addActionRemoveListener(Listener<String> listener) {
	}

	public void removeActionRemoveListener(Listener<String> listener) {
	}

	public Set<String> getActionKeys()
	{
		synchronized (myButtonMap) {
			return Collections.unmodifiableSet(myButtonMap.keySet());
		}
	}
}
