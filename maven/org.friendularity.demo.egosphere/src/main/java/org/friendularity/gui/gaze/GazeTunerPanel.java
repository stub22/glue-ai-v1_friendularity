/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * GazeTuningPanel.java
 *
 * Created on Apr 11, 2010, 5:09:31 PM
 */

package org.friendularity.gui.gaze;

import org.friendularity.gaze.util.GazeStrategyCue;
import org.friendularity.gaze.util.GazeStrategyCue.MotionStyle;
import org.friendularity.gaze.api.GazeJointStrategy;
import org.freckler.sight.impl.hypo.SightHypothesis;

/**
 *
 * @author humankind
 */
public class GazeTunerPanel extends javax.swing.JPanel {
	private		GazeTunerImpl		myGazeTunerImpl;

    /** Creates new form GazeTuningPanel */
    public GazeTunerPanel() {
        initComponents();
		if (!java.beans.Beans.isDesignTime()) {
			GazeTunerImpl gti = new GazeTunerImpl();
			// bmi.setConfigBean(myBlendingMonitorBean);
			myGazeTunerImpl = gti;
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

        combo_gazePlan = new javax.swing.JComboBox();
        txt_targetFaceNum = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        gazeTuningTabs = new javax.swing.JTabbedPane();
        jumpyTuningTab = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        txt_refresh = new javax.swing.JTextField();
        txt_slackX = new javax.swing.JTextField();
        txt_flatJump = new javax.swing.JTextField();
        txt_slackY = new javax.swing.JTextField();
        txt_jumpRatio = new javax.swing.JTextField();
        txt_brakePower = new javax.swing.JTextField();
        txt_brakeSlope = new javax.swing.JTextField();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        btn_updateGaze = new javax.swing.JButton();
        jLabel9 = new javax.swing.JLabel();
        txt_uncert = new javax.swing.JTextField();
        txt_age_uncert = new javax.swing.JTextField();
        jLabel10 = new javax.swing.JLabel();

        combo_gazePlan.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        combo_gazePlan.setEnabled(false);
        combo_gazePlan.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                combo_gazePlanActionPerformed(evt);
            }
        });

        txt_targetFaceNum.setEnabled(false);

        jLabel1.setText("gaze face number=");
        jLabel1.setEnabled(false);

        jumpyTuningTab.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        jumpyTuningTab.setEnabled(false);

        jLabel2.setText("Refresh");

        jLabel3.setText("slackX");

        jLabel4.setText("flatJump");

        jLabel5.setText("brakePower");

        jLabel6.setText("slackY");

        jLabel7.setText("jumpRatio");

        jLabel8.setText("breakSlope");

        btn_updateGaze.setText("Update Gaze");
        btn_updateGaze.setEnabled(false);
        btn_updateGaze.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btn_updateGazeActionPerformed(evt);
            }
        });

        jLabel9.setText("PosUncert");

        jLabel10.setText("AgeUncert");

        javax.swing.GroupLayout jumpyTuningTabLayout = new javax.swing.GroupLayout(jumpyTuningTab);
        jumpyTuningTab.setLayout(jumpyTuningTabLayout);
        jumpyTuningTabLayout.setHorizontalGroup(
            jumpyTuningTabLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jumpyTuningTabLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jumpyTuningTabLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel2)
                    .addComponent(jLabel9)
                    .addComponent(jLabel3)
                    .addComponent(jLabel4)
                    .addComponent(jLabel5))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 150, Short.MAX_VALUE)
                .addGroup(jumpyTuningTabLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(txt_slackX)
                    .addComponent(txt_uncert)
                    .addComponent(txt_flatJump)
                    .addComponent(txt_refresh)
                    .addComponent(txt_brakePower, javax.swing.GroupLayout.DEFAULT_SIZE, 52, Short.MAX_VALUE))
                .addGap(18, 18, 18)
                .addGroup(jumpyTuningTabLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(btn_updateGaze)
                    .addGroup(jumpyTuningTabLayout.createSequentialGroup()
                        .addGroup(jumpyTuningTabLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel6)
                            .addComponent(jLabel7)
                            .addComponent(jLabel8)
                            .addComponent(jLabel10))
                        .addGap(18, 18, 18)
                        .addGroup(jumpyTuningTabLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(txt_slackY, javax.swing.GroupLayout.DEFAULT_SIZE, 62, Short.MAX_VALUE)
                            .addComponent(txt_jumpRatio, javax.swing.GroupLayout.DEFAULT_SIZE, 62, Short.MAX_VALUE)
                            .addComponent(txt_age_uncert, javax.swing.GroupLayout.DEFAULT_SIZE, 62, Short.MAX_VALUE)
                            .addComponent(txt_brakeSlope, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 62, Short.MAX_VALUE))))
                .addContainerGap())
        );
        jumpyTuningTabLayout.setVerticalGroup(
            jumpyTuningTabLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jumpyTuningTabLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jumpyTuningTabLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel3)
                    .addComponent(txt_slackX, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel6)
                    .addComponent(txt_slackY, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jumpyTuningTabLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel4)
                    .addComponent(txt_flatJump, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel7)
                    .addComponent(txt_jumpRatio, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jumpyTuningTabLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(txt_brakePower, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel5)
                    .addComponent(jLabel8)
                    .addComponent(txt_brakeSlope, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jumpyTuningTabLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(txt_refresh, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(9, 9, 9)
                .addGroup(jumpyTuningTabLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel9)
                    .addComponent(txt_uncert, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel10)
                    .addComponent(txt_age_uncert, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 23, Short.MAX_VALUE)
                .addComponent(btn_updateGaze)
                .addContainerGap())
        );

        gazeTuningTabs.addTab("tab1", jumpyTuningTab);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(102, 102, 102)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(54, 54, 54)
                        .addComponent(jLabel1)
                        .addGap(18, 18, 18)
                        .addComponent(txt_targetFaceNum, javax.swing.GroupLayout.PREFERRED_SIZE, 110, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addContainerGap(81, Short.MAX_VALUE))
                    .addGroup(layout.createSequentialGroup()
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(combo_gazePlan, javax.swing.GroupLayout.PREFERRED_SIZE, 314, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(43, 43, 43))))
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(gazeTuningTabs, javax.swing.GroupLayout.DEFAULT_SIZE, 439, Short.MAX_VALUE)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(combo_gazePlan, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(txt_targetFaceNum, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addComponent(gazeTuningTabs, javax.swing.GroupLayout.DEFAULT_SIZE, 224, Short.MAX_VALUE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

	private void combo_gazePlanActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_combo_gazePlanActionPerformed
		try{
			GazeStrategyCue gsc = myGazeTunerImpl.getGaze((String)combo_gazePlan.getSelectedItem());
			txt_refresh.setText(gsc.getRefreshPeriodFrames().toString());
			txt_slackX.setText(gsc.getSlackHorizDeg().toString());
			txt_slackY.setText(gsc.getSlackVertDeg().toString());
			txt_flatJump.setText(gsc.getFlatJumpSize().toString());
			txt_jumpRatio.setText(gsc.getDistanceJumpRatio().toString());
			txt_brakePower.setText(gsc.getBrakePower().toString());
			txt_brakeSlope.setText(gsc.getBrakeSlope().toString());
			txt_uncert.setText(SightHypothesis.getFaceNoticeConfig().positionUncertaintyWeight.toString());
			txt_age_uncert.setText(SightHypothesis.getFaceNoticeConfig().ageUncertaintyWeight.toString());
			MotionStyle ms = gsc.getMotionStyle();
			int count = gazeTuningTabs.getTabCount();
			for(int i=1; i<count; i++){
				gazeTuningTabs.remove(i);
			}
			for(GazeJointStrategy gjs : gsc.getJointLinks()){
				gazeTuningTabs.addTab(((Integer)gjs.getLogicalJointID()).toString(),
					new RampyGazeTuningPanel(gjs, ms));
			}
		}catch(Exception ex){}
}//GEN-LAST:event_combo_gazePlanActionPerformed

	private void btn_updateGazeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btn_updateGazeActionPerformed
		try{
			String name = (String)combo_gazePlan.getSelectedItem();
			Integer refresh = Integer.parseInt(txt_refresh.getText());
			Double slackX = Double.parseDouble(txt_slackX.getText());
			Double slackY = Double.parseDouble(txt_slackY.getText());
			Double flatJump = Double.parseDouble(txt_flatJump.getText());
			Double jumpRatio = Double.parseDouble(txt_jumpRatio.getText());
			Double brakePower = Double.parseDouble(txt_brakePower.getText());
			Double brakeSlope = Double.parseDouble(txt_brakeSlope.getText());
			myGazeTunerImpl.changeGaze(name, refresh, slackX, slackY, flatJump,
				jumpRatio, brakePower, brakeSlope);
			SightHypothesis.getFaceNoticeConfig().positionUncertaintyWeight =
				Double.parseDouble(txt_uncert.getText());
			SightHypothesis.getFaceNoticeConfig().ageUncertaintyWeight =
				Double.parseDouble(txt_age_uncert.getText());
		}catch(Exception e){
			myGazeTunerImpl.logWarning("There was an error setting gaze variables. Values have not been changed.");
		}
}//GEN-LAST:event_btn_updateGazeActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btn_updateGaze;
    private javax.swing.JComboBox combo_gazePlan;
    private javax.swing.JTabbedPane gazeTuningTabs;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jumpyTuningTab;
    private javax.swing.JTextField txt_age_uncert;
    private javax.swing.JTextField txt_brakePower;
    private javax.swing.JTextField txt_brakeSlope;
    private javax.swing.JTextField txt_flatJump;
    private javax.swing.JTextField txt_jumpRatio;
    private javax.swing.JTextField txt_refresh;
    private javax.swing.JTextField txt_slackX;
    private javax.swing.JTextField txt_slackY;
    private javax.swing.JTextField txt_targetFaceNum;
    private javax.swing.JTextField txt_uncert;
    // End of variables declaration//GEN-END:variables

}
