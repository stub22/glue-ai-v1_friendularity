/*
 * EgosphereMonitorPanel.java
 *
 * Created on September 27, 2009, 5:59 PM
 */

package org.friendularity.gui.egosphere;

/**
 *
 * @author  Matthew Stevenson
 */
public class EgosphereMonitorPanel extends javax.swing.JPanel {

    /** Creates new form EgosphereMonitorPanel */
    public EgosphereMonitorPanel() {
        initComponents();
		if (!java.beans.Beans.isDesignTime()) {
			EgosphereMonitorImpl emi = new EgosphereMonitorImpl();
			emi.setDrawingPanel(myEgosphereSwingPanel);
			emi.setScalingPanel(scalingVisionPanel1);
			emi.setEgoBean(myEgosphereMonitorBean);
			emi.setGazeDetailTextArea(detailsTextArea);
		}
    }
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        bindingGroup = new org.jdesktop.beansbinding.BindingGroup();

        myEgosphereMonitorBean = new org.friendularity.gui.egosphere.EgosphereMonitorBean();
        myEgosphereSwingPanel = new org.friendularity.gui.egosphere.EgosphereSwingPanel();
        jToggleButton1 = new javax.swing.JToggleButton();
        scalingVisionPanel1 = new org.friendularity.gui.egosphere.ScalingVisionPanel();
        jLabel1 = new javax.swing.JLabel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jScrollPane1 = new javax.swing.JScrollPane();
        detailsTextArea = new javax.swing.JTextArea();

        myEgosphereMonitorBean.addPropertyChangeListener(new java.beans.PropertyChangeListener() {
            public void propertyChange(java.beans.PropertyChangeEvent evt) {
                myEgosphereMonitorBeanPropertyChange(evt);
            }
        });

        myEgosphereSwingPanel.setBackground(new java.awt.Color(153, 153, 153));
        myEgosphereSwingPanel.setBorder(javax.swing.BorderFactory.createMatteBorder(3, 3, 3, 3, new java.awt.Color(102, 102, 102)));

        javax.swing.GroupLayout myEgosphereSwingPanelLayout = new javax.swing.GroupLayout(myEgosphereSwingPanel);
        myEgosphereSwingPanel.setLayout(myEgosphereSwingPanelLayout);
        myEgosphereSwingPanelLayout.setHorizontalGroup(
            myEgosphereSwingPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 742, Short.MAX_VALUE)
        );
        myEgosphereSwingPanelLayout.setVerticalGroup(
            myEgosphereSwingPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 291, Short.MAX_VALUE)
        );

        jToggleButton1.setLabel("Start!");

        org.jdesktop.beansbinding.Binding binding = org.jdesktop.beansbinding.Bindings.createAutoBinding(org.jdesktop.beansbinding.AutoBinding.UpdateStrategy.READ_WRITE, myEgosphereMonitorBean, org.jdesktop.beansbinding.ELProperty.create("${displayVideo}"), jToggleButton1, org.jdesktop.beansbinding.BeanProperty.create("selected"));
        bindingGroup.addBinding(binding);

        jToggleButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jToggleButton1ActionPerformed(evt);
            }
        });

        scalingVisionPanel1.setBackground(new java.awt.Color(0, 153, 0));
        scalingVisionPanel1.setBorder(javax.swing.BorderFactory.createMatteBorder(5, 5, 5, 5, new java.awt.Color(0, 102, 0)));
        scalingVisionPanel1.setMaximumSize(new java.awt.Dimension(320, 240));
        scalingVisionPanel1.setMinimumSize(new java.awt.Dimension(320, 240));
        scalingVisionPanel1.setPreferredSize(new java.awt.Dimension(320, 240));

        javax.swing.GroupLayout scalingVisionPanel1Layout = new javax.swing.GroupLayout(scalingVisionPanel1);
        scalingVisionPanel1.setLayout(scalingVisionPanel1Layout);
        scalingVisionPanel1Layout.setHorizontalGroup(
            scalingVisionPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 310, Short.MAX_VALUE)
        );
        scalingVisionPanel1Layout.setVerticalGroup(
            scalingVisionPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 233, Short.MAX_VALUE)
        );

        jTextArea1.setColumns(20);
        jTextArea1.setEditable(false);
        jTextArea1.setFont(new java.awt.Font("Monospaced", 0, 10)); // NOI18N
        jTextArea1.setLineWrap(true);
        jTextArea1.setRows(5);
        jTextArea1.setText("All PersonTrackers\nare shown below at their \ncurrently estimated \nposition.  Each tracker\ncorresponds to either:\na) faceHypo only\nb) personCue only\nc) faceHypo + personCue\n\nFaces below are not to scale, they are fixed at 40x40 pixels.");
        jScrollPane2.setViewportView(jTextArea1);

        detailsTextArea.setColumns(20);
        detailsTextArea.setEditable(false);
        detailsTextArea.setFont(new java.awt.Font("Monospaced", 0, 10)); // NOI18N
        detailsTextArea.setLineWrap(true);
        detailsTextArea.setRows(5);
        jScrollPane1.setViewportView(detailsTextArea);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(myEgosphereSwingPanel, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(layout.createSequentialGroup()
                                .addGap(51, 51, 51)
                                .addComponent(jToggleButton1))
                            .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 156, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 244, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel1)
                        .addGap(18, 18, 18)
                        .addComponent(scalingVisionPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 243, Short.MAX_VALUE)
                    .addComponent(scalingVisionPanel1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 243, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(jToggleButton1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jLabel1)
                                .addGap(47, 47, 47))
                            .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 214, Short.MAX_VALUE))))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(myEgosphereSwingPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jToggleButton1.getAccessibleContext().setAccessibleName("start_video");

        bindingGroup.bind();
    }// </editor-fold>//GEN-END:initComponents

private void myEgosphereMonitorBeanPropertyChange(java.beans.PropertyChangeEvent evt) {//GEN-FIRST:event_myEgosphereMonitorBeanPropertyChange
// TODO add your handling code here:
}//GEN-LAST:event_myEgosphereMonitorBeanPropertyChange

private void jToggleButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jToggleButton1ActionPerformed
	// TODO add your handling code here:
}//GEN-LAST:event_jToggleButton1ActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTextArea detailsTextArea;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JToggleButton jToggleButton1;
    private org.friendularity.gui.egosphere.EgosphereMonitorBean myEgosphereMonitorBean;
    private org.friendularity.gui.egosphere.EgosphereSwingPanel myEgosphereSwingPanel;
    private org.friendularity.gui.egosphere.ScalingVisionPanel scalingVisionPanel1;
    private org.jdesktop.beansbinding.BindingGroup bindingGroup;
    // End of variables declaration//GEN-END:variables

}
