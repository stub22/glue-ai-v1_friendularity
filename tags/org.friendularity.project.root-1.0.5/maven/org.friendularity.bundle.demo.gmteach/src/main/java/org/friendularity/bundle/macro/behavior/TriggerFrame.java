package org.friendularity.bundle.macro.behavior;

import java.awt.Component;

import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.UIManager;
import javax.swing.plaf.PanelUI;

import org.appdapter.core.log.Debuggable;
import org.osgi.framework.BundleContext;

/**
 * 
 * @author matt
 */

public class TriggerFrame extends javax.swing.JFrame {
	private final static String BEHAVIOR_PANEL_ID = "behaviorPanelID";
	private final static String ADMIN_PANEL_ID = "adminPanelID";

	private boolean myInitFlag;

	/**
	 * Creates new form TriggerFrame
	 */
	public TriggerFrame() {
		initComponents();
	}

	/**
	 * Called by the constructor methods to create the default <code>glassPane</code>. By default this method creates a new <code>JComponent</code> with visibility set to false.
	 * 
	 * @return the default <code>glassPane</code>
	 */
	protected Component createGlassPane() {
		JComponent c = new JPanel() {
			/**
			 * Resets the UI property with a value from the current look and feel.
			 * 
			 * @see JComponent#updateUI
			 */
			public void updateUI() {
				try {
					PanelUI pui = (PanelUI) UIManager.getUI(this);
					setUI(pui);
				} catch (Throwable t) {
					Debuggable.printStackTrace(t);
				}
			}

			/**
			 * Returns the look and feel (L&F) object that renders this component.
			 * 
			 * @return the PanelUI object that renders this component
			 * @since 1.4
			 */
			public PanelUI getUI() {
				return (PanelUI) ui;
			}
		};
		c.setName(this.getName() + ".glassPane");
		c.setVisible(false);
		((JPanel) c).setOpaque(false);
		return c;
	}

	/**
	 * This method is called from within the constructor to initialize the form. WARNING: Do NOT modify this code. The content of this method is always regenerated by the Form Editor.
	 */
	@SuppressWarnings("unchecked")
	// <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
	private void initComponents() {

		behavoirMasterPanel2 = new BehavoirMasterPanel();

		setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

		javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
		getContentPane().setLayout(layout);
		layout.setHorizontalGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING).addComponent(behavoirMasterPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, 475, Short.MAX_VALUE));
		layout.setVerticalGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING).addComponent(behavoirMasterPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, 334, Short.MAX_VALUE));

		pack();
	}// </editor-fold>//GEN-END:initComponents

	/**
	 * @param args
	 *            the command line arguments
	 */
	public static void main(String args[]) {
		/* Set the Nimbus look and feel */
		//<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
		/* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
		 * For details see http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html 
		 */
		try {
			for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
				if ("Nimbus".equals(info.getName())) {
					javax.swing.UIManager.setLookAndFeel(info.getClassName());
					break;
				}
			}
		} catch (ClassNotFoundException ex) {
			java.util.logging.Logger.getLogger(TriggerFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
		} catch (InstantiationException ex) {
			java.util.logging.Logger.getLogger(TriggerFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
		} catch (IllegalAccessException ex) {
			java.util.logging.Logger.getLogger(TriggerFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
		} catch (javax.swing.UnsupportedLookAndFeelException ex) {
			java.util.logging.Logger.getLogger(TriggerFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
		}
		//</editor-fold>

		/* Create and display the form */
		java.awt.EventQueue.invokeLater(new Runnable() {
			public void run() {
				new TriggerFrame().setVisible(true);
			}
		});
	}

	public synchronized void init(BundleContext context, String behaviorID, String adminID) {
		if (myInitFlag || context == null) {
			return;
		}
		behavoirMasterPanel2.init(context, behaviorID, adminID);
	}

	// Variables declaration - do not modify//GEN-BEGIN:variables
	private BehavoirMasterPanel behavoirMasterPanel2;
	// End of variables declaration//GEN-END:variables
}
