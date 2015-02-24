/*
 *  Copyright 2012 by The Cogchar Project (www.cogchar.org).
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.friendularity.model.test;

import com.jme3.system.AppSettings;
import java.io.File;
import javax.swing.filechooser.FileFilter;

/**
 *
 * @author Ryan Biggs <rbiggs@skyriversoftware.com>
 */


public class ModelPathDialog extends javax.swing.JDialog {

	static File lastDir;
	
	/**
	 * Creates new form ModelPathDialog
	 */
	public ModelPathDialog(java.awt.Frame parent, boolean modal) {
		super(parent, modal);
		initComponents();
		if (lastDir != null) {
			jFileChooser1.setCurrentDirectory(lastDir);
		}
	}

	/**
	 * This method is called from within the constructor to initialize the form. WARNING: Do NOT modify this code. The
	 * content of this method is always regenerated by the Form Editor.
	 */
	@SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jFileChooser1 = new javax.swing.JFileChooser();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jPanel1.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jLabel1.setFont(new java.awt.Font("Tahoma", 0, 30)); // NOI18N
        jLabel1.setText("Select the mesh.xml file of the model you wish to test:");

        jFileChooser1.setAcceptAllFileFilterUsed(false);
        jFileChooser1.setCurrentDirectory(new java.io.File("C:\\"));
            jFileChooser1.setFileFilter(new MeshXmlFilter());
            jFileChooser1.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    jFileChooser1ActionPerformed(evt);
                }
            });

            javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
            jPanel1.setLayout(jPanel1Layout);
            jPanel1Layout.setHorizontalGroup(
                jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel1Layout.createSequentialGroup()
                    .addComponent(jFileChooser1, javax.swing.GroupLayout.PREFERRED_SIZE, 721, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGap(0, 0, Short.MAX_VALUE))
                .addGroup(jPanel1Layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jLabel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addContainerGap())
            );
            jPanel1Layout.setVerticalGroup(
                jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel1Layout.createSequentialGroup()
                    .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jLabel1)
                    .addGap(18, 18, 18)
                    .addComponent(jFileChooser1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            );

            javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
            getContentPane().setLayout(layout);
            layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
            );
            layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
            );

            pack();
        }// </editor-fold>//GEN-END:initComponents

	private void jFileChooser1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jFileChooser1ActionPerformed
		// TODO add your handling code here:
		String command = evt.getActionCommand();
		
		if (command.equals(javax.swing.JFileChooser.APPROVE_SELECTION)) {
			File desiredFile = jFileChooser1.getSelectedFile();
			lastDir = desiredFile;
			final ModelViewer app = new ModelViewer(desiredFile);
			app.setShowSettings(false);
			AppSettings settings = new AppSettings(true);
			settings.setResolution(800, 600);
			settings.setBitsPerPixel(32);
			app.setSettings(settings);
			new Thread(new Runnable(){
				public void run(){
					app.start();
				}
				}).start();
			this.dispose();
		} else if (command.equals(javax.swing.JFileChooser.CANCEL_SELECTION)) {
			System.exit(0);
		}
	}//GEN-LAST:event_jFileChooser1ActionPerformed

	static class MeshXmlFilter extends FileFilter {
		
		final static String MESH_XML_SUFFIX = "mesh.xml";
		final static String DESCRIPTION = "mesh.xml files";
		
		@Override
		public boolean accept(File f) {
			if (f.isDirectory() || f.getName().endsWith(MESH_XML_SUFFIX)) {
				return true;
			} else {
				return false;
			}
		}
		
		@Override
		public String getDescription() {
			return DESCRIPTION;
		}
	}
	
	/**
	 * @param args the command line arguments
	 */
	public static void start(String args[]) {
		/*
		 * Set the Nimbus look and feel
		 */
		//<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
        /*
		 * If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel. For details see
		 * http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html
		 */
		try {
			for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
				if ("Nimbus".equals(info.getName())) {
					javax.swing.UIManager.setLookAndFeel(info.getClassName());
					break;
				}
			}
		} catch (ClassNotFoundException ex) {
			java.util.logging.Logger.getLogger(ModelPathDialog.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
		} catch (InstantiationException ex) {
			java.util.logging.Logger.getLogger(ModelPathDialog.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
		} catch (IllegalAccessException ex) {
			java.util.logging.Logger.getLogger(ModelPathDialog.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
		} catch (javax.swing.UnsupportedLookAndFeelException ex) {
			java.util.logging.Logger.getLogger(ModelPathDialog.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
		}
		//</editor-fold>

		/*
		 * Create and display the dialog
		 */
		java.awt.EventQueue.invokeLater(new Runnable() {

			public void run() {
				ModelPathDialog dialog = new ModelPathDialog(new javax.swing.JFrame(), true);
				
				dialog.addWindowListener(new java.awt.event.WindowAdapter() {

					@Override
					public void windowClosing(java.awt.event.WindowEvent e) {
						System.exit(0);
					}
				});
				dialog.setVisible(true);
			}
		});
	}
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JFileChooser jFileChooser1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    // End of variables declaration//GEN-END:variables
}
