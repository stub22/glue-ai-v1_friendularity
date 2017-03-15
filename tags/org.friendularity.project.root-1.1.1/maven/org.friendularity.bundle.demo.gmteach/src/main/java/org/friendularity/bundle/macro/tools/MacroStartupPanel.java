package org.friendularity.bundle.macro.tools;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;

import org.friendularity.bundle.demo.gmteach.GMTeachApp;
import org.friendularity.bundle.demo.gmteach.GoalsPanel;
import org.jflux.api.core.Listener;

import ext.osgi.common.MacroBundleActivatorBase;
import ext.osgi.common.MacroBundleActivatorBase.MacroStartupSettings;

public class MacroStartupPanel extends GoalsPanel implements Listener<String> {
	public class RunnableComponent extends JButton {
		private final String name;
		ActionListener singleActionListener;
		private String beforeText;

		private RunnableComponent(String nam, String text, ActionListener al) {
			super(text);
			name = nam;
			setToolTipText(name);
			addActionListener(al);
		}

		@Override public void addActionListener(ActionListener l) {
			if (singleActionListener == l)
				return;
			if (singleActionListener == null)
				singleActionListener = l;
			super.addActionListener(l);
		}

		@Override public boolean isEnabled() {
			if (name == null)
				return true;
			return super.isEnabled() && !macroStartupSettings.isBegun(name);
		}

		@Override public boolean isVisible() {
			if (name == null)
				return true;
			return super.isVisible() && macroStartupSettings.isService(name);
		}

		@Override protected void fireActionPerformed(final ActionEvent event) {
			RunnableComponent.super.setEnabled(false);
			invokeInQueue(MacroBundleActivatorBase.asTodoItem(name, new Runnable() {
				@Override public void run() {
					try {
						RunnableComponent.super.fireActionPerformed(event);
					} finally {
						RunnableComponent.super.setEnabled(true);
					}
				}
			}));
			childChanged(RunnableComponent.this);
		}

		public boolean refresh() {
			final String now = getButtonText(name);
			String before = getText();
			if (before.equals(now))
				return false;
			invokeAndWait(new Runnable() {
				@Override public void run() {
					setText(now);
				}
			});
			return true;
		}

	}

	@Override protected void updateChildInfo(Component c) {
		if (c instanceof RunnableComponent) {
			((RunnableComponent) c).refresh();
		}
		super.updateChildInfo(c);
	}

	public String getButtonText(String name) {
		String mi = macroStartupSettings.getValueStatus(name);
		String pn = macroStartupSettings.getServiceName(name);
		String now = pn + "=" + mi;
		return now;
	}

	@Override public void handleEvent(String input) {
		Component component = myButtonMap.get(input);
		if (component != null) {
			if (component instanceof RunnableComponent) {
				RunnableComponent rc = (RunnableComponent) component;
				if (rc.refresh()) {
					childChanged(rc);
				}
				return;
			}
		}
		super.handleEvent(input);
	}

	MacroStartupSettings macroStartupSettings;

	public MacroStartupPanel(MacroStartupSettings ms) {
		this.macroStartupSettings = ms;
		addListenableMap(ms.actionCallbackMap);
		macroStartupSettings.registerServiceChanged(this);

	}

	@Override protected JButton newJButton(final String name, final ActionListener listener) {
		JButton button = new RunnableComponent(name, getButtonText(name), listener);
		button.addActionListener(listener);
		return button;
	}

	@Override public ActionListener getActionCallback(final String actionCallbackName) {
		ActionListener al = super.getActionCallback(actionCallbackName);
		if (al != null)
			return al;
		return new ActionListener() {
			@Override public void actionPerformed(ActionEvent e) {
				GMTeachApp.macroStartupSettings.runNow(actionCallbackName);
			}
		};

	}
}