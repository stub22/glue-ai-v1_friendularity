/**
 * Copyright 2009 Hanson Robotics Inc.
 * All Rights Reserved.
 */

package org.friendularity.gui.hypo;

import org.friendularity.app.face.FaceHypothesis;
import org.cogchar.animoid.config.FaceNoticeConfig;

/**
 * @author  Stu Baurmann
 */
public class HypoMonitorPanel extends javax.swing.JPanel {
    private HypoMonitorImpl		myImpl;
    /** Creates new form HypoMonitorPanel */
    public HypoMonitorPanel() {
        initComponents();
        if (!java.beans.Beans.isDesignTime()) {
			myImpl = new HypoMonitorImpl();
			hypoTable.setModel(myImpl.getHypoTableModel());
			distTable.setModel(myImpl.getHypoDistanceTableModel());
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

        statsSplitPane = new javax.swing.JSplitPane();
        hypoScrollPane = new javax.swing.JScrollPane();
        hypoTable = new javax.swing.JTable();
        popScrollPane = new javax.swing.JScrollPane();
        distTable = new javax.swing.JTable();
        jPanel3 = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        txt_azDiamSqCoef = new javax.swing.JTextField();
        txt_mergeThresh = new javax.swing.JTextField();
        jLabel4 = new javax.swing.JLabel();
        txt_degXsecCoef = new javax.swing.JTextField();
        jLabel3 = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        txt_secondsCoef = new javax.swing.JTextField();
        jLabel2 = new javax.swing.JLabel();
        jLabel10 = new javax.swing.JLabel();
        txt_elDiamSqCoef = new javax.swing.JTextField();
        but_saveCogDistParams = new javax.swing.JButton();
        jPanel2 = new javax.swing.JPanel();
        jLabel5 = new javax.swing.JLabel();
        txt_initStrength = new javax.swing.JTextField();
        txt_hiddenDecay = new javax.swing.JTextField();
        jLabel6 = new javax.swing.JLabel();
        txt_exposedDecay = new javax.swing.JTextField();
        jLabel7 = new javax.swing.JLabel();
        txt_survivalThresh = new javax.swing.JTextField();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        txt_obsRetainedInHypo = new javax.swing.JTextField();
        jLabel14 = new javax.swing.JLabel();
        txt_obsRetainedInFF = new javax.swing.JTextField();
        but_loadCogDistParams = new javax.swing.JButton();
        jPanel4 = new javax.swing.JPanel();
        jLabel11 = new javax.swing.JLabel();
        txt_overlapCoef = new javax.swing.JTextField();
        jLabel12 = new javax.swing.JLabel();
        txt_freckMatchCoef = new javax.swing.JTextField();
        jLabel13 = new javax.swing.JLabel();
        txt_freckMismatchCoef = new javax.swing.JTextField();
        but_startMon = new javax.swing.JButton();

        setPreferredSize(new java.awt.Dimension(600, 600));

        statsSplitPane.setDividerLocation(145);
        statsSplitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);

        hypoTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        hypoScrollPane.setViewportView(hypoTable);

        statsSplitPane.setLeftComponent(hypoScrollPane);

        distTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        popScrollPane.setViewportView(distTable);

        statsSplitPane.setRightComponent(popScrollPane);

        txt_azDiamSqCoef.setColumns(12);
        txt_azDiamSqCoef.setText("jTextField1");

        txt_mergeThresh.setColumns(12);
        txt_mergeThresh.setText("jTextField3");

        jLabel4.setText("mergeThresh");

        txt_degXsecCoef.setColumns(12);
        txt_degXsecCoef.setEditable(false);
        txt_degXsecCoef.setText("jTextField2");

        jLabel3.setText("unused");

        jLabel1.setText("azDiamSqCoef");

        txt_secondsCoef.setColumns(12);
        txt_secondsCoef.setText("jTextField1");

        jLabel2.setText("secondsCoef");

        jLabel10.setText("elDiamSqCoef");

        txt_elDiamSqCoef.setColumns(12);
        txt_elDiamSqCoef.setText("jTextField1");
        txt_elDiamSqCoef.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                txt_elDiamSqCoefActionPerformed(evt);
            }
        });

        org.jdesktop.layout.GroupLayout jPanel1Layout = new org.jdesktop.layout.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(jPanel1Layout.createSequentialGroup()
                .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(jPanel1Layout.createSequentialGroup()
                        .addContainerGap()
                        .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                            .add(jPanel1Layout.createSequentialGroup()
                                .add(jLabel3)
                                .addPreferredGap(org.jdesktop.layout.LayoutStyle.UNRELATED)
                                .add(txt_degXsecCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                            .add(jPanel1Layout.createSequentialGroup()
                                .add(jLabel4)
                                .addPreferredGap(org.jdesktop.layout.LayoutStyle.UNRELATED)
                                .add(txt_mergeThresh, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))))
                    .add(jPanel1Layout.createSequentialGroup()
                        .add(7, 7, 7)
                        .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.TRAILING)
                            .add(jLabel10)
                            .add(jLabel1)
                            .add(jLabel2))
                        .add(10, 10, 10)
                        .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                            .add(txt_secondsCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                            .add(txt_elDiamSqCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                            .add(txt_azDiamSqCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))))
                .addContainerGap(org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(jPanel1Layout.createSequentialGroup()
                .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(jLabel1)
                    .add(txt_azDiamSqCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(jLabel10)
                    .add(txt_elDiamSqCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(jLabel2)
                    .add(txt_secondsCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(txt_degXsecCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(jLabel3))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.TRAILING)
                    .add(jLabel4)
                    .add(txt_mergeThresh, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
        );

        but_saveCogDistParams.setText("commit");
        but_saveCogDistParams.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                but_saveCogDistParamsActionPerformed(evt);
            }
        });

        jLabel5.setText("initStrength");

        txt_initStrength.setColumns(9);
        txt_initStrength.setText("jTextField1");

        txt_hiddenDecay.setColumns(9);
        txt_hiddenDecay.setText("jTextField4");
        txt_hiddenDecay.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                txt_hiddenDecayActionPerformed(evt);
            }
        });

        jLabel6.setText("hiddenDecay");

        txt_exposedDecay.setColumns(9);
        txt_exposedDecay.setText("jTextField5");

        jLabel7.setText("exposedDecay");

        txt_survivalThresh.setColumns(9);
        txt_survivalThresh.setText("jTextField6");

        jLabel8.setText("survivalThresh");

        jLabel9.setText("obsRetainedInHypo");

        txt_obsRetainedInHypo.setColumns(9);
        txt_obsRetainedInHypo.setText("jTextField6");

        jLabel14.setText("obsRetainedInFF");

        txt_obsRetainedInFF.setColumns(9);
        txt_obsRetainedInFF.setText("jTextField6");
        txt_obsRetainedInFF.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                txt_obsRetainedInFFActionPerformed(evt);
            }
        });

        org.jdesktop.layout.GroupLayout jPanel2Layout = new org.jdesktop.layout.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .add(jPanel2Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(jPanel2Layout.createSequentialGroup()
                        .add(24, 24, 24)
                        .add(jPanel2Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.TRAILING)
                            .add(jLabel5)
                            .add(jLabel6)
                            .add(jLabel7)
                            .add(jLabel8))
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(jPanel2Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                            .add(txt_survivalThresh, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 92, Short.MAX_VALUE)
                            .add(txt_exposedDecay, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 92, Short.MAX_VALUE)
                            .add(txt_hiddenDecay, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 92, Short.MAX_VALUE)
                            .add(txt_initStrength, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 92, Short.MAX_VALUE)))
                    .add(org.jdesktop.layout.GroupLayout.TRAILING, jPanel2Layout.createSequentialGroup()
                        .add(jLabel9)
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(txt_obsRetainedInHypo, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 92, Short.MAX_VALUE))
                    .add(jPanel2Layout.createSequentialGroup()
                        .add(11, 11, 11)
                        .add(jLabel14)
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(txt_obsRetainedInFF, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 94, Short.MAX_VALUE)))
                .add(32, 32, 32))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .add(jPanel2Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(jLabel5)
                    .add(txt_initStrength, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jPanel2Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(txt_hiddenDecay, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(jLabel6))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jPanel2Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(txt_exposedDecay, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(jLabel7))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jPanel2Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(txt_survivalThresh, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(jLabel8))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jPanel2Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(txt_obsRetainedInHypo, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(jLabel9))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jPanel2Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(jLabel14)
                    .add(txt_obsRetainedInFF, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(19, Short.MAX_VALUE))
        );

        but_loadCogDistParams.setText("load");
        but_loadCogDistParams.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                but_loadCogDistParamsActionPerformed(evt);
            }
        });

        jLabel11.setText("overlapCoef");

        txt_overlapCoef.setColumns(8);
        txt_overlapCoef.setText("jTextField1");

        jLabel12.setText("freckMatchCoef");

        txt_freckMatchCoef.setColumns(8);
        txt_freckMatchCoef.setText("jTextField1");

        jLabel13.setText("freckMismatchCoef");

        txt_freckMismatchCoef.setColumns(8);
        txt_freckMismatchCoef.setText("jTextField1");

        org.jdesktop.layout.GroupLayout jPanel4Layout = new org.jdesktop.layout.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(jPanel4Layout.createSequentialGroup()
                .addContainerGap(org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .add(jPanel4Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(org.jdesktop.layout.GroupLayout.TRAILING, jPanel4Layout.createSequentialGroup()
                        .add(jLabel13, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 97, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(txt_freckMismatchCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                    .add(org.jdesktop.layout.GroupLayout.TRAILING, jPanel4Layout.createSequentialGroup()
                        .add(jPanel4Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.TRAILING)
                            .add(jLabel12, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 82, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                            .add(jLabel11, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 68, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(jPanel4Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.TRAILING)
                            .add(txt_freckMatchCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                            .add(txt_overlapCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))))
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .add(jPanel4Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(txt_overlapCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(jLabel11))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jPanel4Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(txt_freckMatchCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(jLabel12))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jPanel4Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(jLabel13)
                    .add(txt_freckMismatchCoef, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(32, Short.MAX_VALUE))
        );

        but_startMon.setText("start mon");
        but_startMon.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                but_startMonActionPerformed(evt);
            }
        });

        org.jdesktop.layout.GroupLayout jPanel3Layout = new org.jdesktop.layout.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(jPanel3Layout.createSequentialGroup()
                .add(jPanel2, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.UNRELATED)
                .add(jPanel3Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(jPanel3Layout.createSequentialGroup()
                        .add(10, 10, 10)
                        .add(but_loadCogDistParams)
                        .add(18, 18, 18)
                        .add(but_saveCogDistParams)
                        .add(103, 103, 103)
                        .add(but_startMon))
                    .add(jPanel3Layout.createSequentialGroup()
                        .add(jPanel1, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .add(jPanel4, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(jPanel3Layout.createSequentialGroup()
                .add(jPanel3Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(jPanel3Layout.createSequentialGroup()
                        .addContainerGap()
                        .add(jPanel1, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                    .add(jPanel3Layout.createSequentialGroup()
                        .add(20, 20, 20)
                        .add(jPanel4, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                .add(jPanel3Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(jPanel3Layout.createSequentialGroup()
                        .add(11, 11, 11)
                        .add(jPanel3Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                            .add(but_loadCogDistParams)
                            .add(but_saveCogDistParams)))
                    .add(jPanel3Layout.createSequentialGroup()
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(but_startMon)))
                .addContainerGap(org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            .add(jPanel2, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(layout.createSequentialGroup()
                        .add(statsSplitPane, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 628, Short.MAX_VALUE)
                        .add(10, 10, 10))
                    .add(jPanel3, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .add(jPanel3, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(statsSplitPane, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 298, Short.MAX_VALUE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

	private void but_saveCogDistParamsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_but_saveCogDistParamsActionPerformed
		FaceNoticeConfig fnc = FaceHypothesis.getFaceNoticeConfig();
		fnc.cogDistCoeffAzDiamSquared = Double.parseDouble(txt_azDiamSqCoef.getText());
		fnc.cogDistCoeffElDiamSquared = Double.parseDouble(txt_elDiamSqCoef.getText());
		fnc.cogDistCoeffSeconds = Double.parseDouble(txt_secondsCoef.getText());
		fnc.cogDistCoeffProduct = Double.parseDouble(txt_degXsecCoef.getText());

		fnc.cogDistCoeffTimestampOverlap = Double.parseDouble(txt_overlapCoef.getText());
		fnc.cogDistCoeffFreckleMatch = Double.parseDouble(txt_freckMatchCoef.getText());
		fnc.cogDistCoeffFreckleMismatch = Double.parseDouble(txt_freckMismatchCoef.getText());

		fnc.mergeThresholdCogDist = Double.parseDouble(txt_mergeThresh.getText());

		fnc.initialStrength  = Double.parseDouble(txt_initStrength.getText());
		fnc.hiddenDecayConstant = Double.parseDouble(txt_hiddenDecay.getText());
		fnc.exposedDecayConstant = Double.parseDouble(txt_exposedDecay.getText());
		fnc.survivalThreshold = Double.parseDouble(txt_survivalThresh.getText());

		fnc.obsRetainedInHypo = Integer.parseInt(txt_obsRetainedInHypo.getText());
		fnc.obsRetainedInFreckleFace = Integer.parseInt(txt_obsRetainedInFF.getText());

		FaceHypothesis.loadConfig(fnc);
	}//GEN-LAST:event_but_saveCogDistParamsActionPerformed

	private void but_loadCogDistParamsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_but_loadCogDistParamsActionPerformed
		FaceNoticeConfig fnc = FaceHypothesis.getFaceNoticeConfig();
		txt_azDiamSqCoef.setText("" + fnc.cogDistCoeffAzDiamSquared);
		txt_elDiamSqCoef.setText("" + fnc.cogDistCoeffElDiamSquared);
		txt_secondsCoef.setText("" + fnc.cogDistCoeffSeconds);
		txt_degXsecCoef.setText("" + fnc.cogDistCoeffProduct);

		txt_overlapCoef.setText("" + fnc.cogDistCoeffTimestampOverlap);
		txt_freckMatchCoef.setText("" + fnc.cogDistCoeffFreckleMatch);
		txt_freckMismatchCoef.setText("" + fnc.cogDistCoeffFreckleMismatch);

		txt_mergeThresh.setText("" + fnc.mergeThresholdCogDist);

		
		txt_initStrength.setText("" + fnc.initialStrength);
		txt_hiddenDecay.setText("" + fnc.hiddenDecayConstant);
		txt_exposedDecay.setText("" + fnc.exposedDecayConstant);

		txt_obsRetainedInHypo.setText("" + fnc.obsRetainedInHypo);
		txt_obsRetainedInFF.setText("" + fnc.obsRetainedInFreckleFace);

		txt_survivalThresh.setText("" + fnc.survivalThreshold);

	}//GEN-LAST:event_but_loadCogDistParamsActionPerformed

	private void but_startMonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_but_startMonActionPerformed
		myImpl.update(null, null);
	}//GEN-LAST:event_but_startMonActionPerformed

	private void txt_elDiamSqCoefActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_txt_elDiamSqCoefActionPerformed
		// TODO add your handling code here:
	}//GEN-LAST:event_txt_elDiamSqCoefActionPerformed

	private void txt_hiddenDecayActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_txt_hiddenDecayActionPerformed
		// TODO add your handling code here:
	}//GEN-LAST:event_txt_hiddenDecayActionPerformed

	private void txt_obsRetainedInFFActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_txt_obsRetainedInFFActionPerformed
		// TODO add your handling code here:
	}//GEN-LAST:event_txt_obsRetainedInFFActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton but_loadCogDistParams;
    private javax.swing.JButton but_saveCogDistParams;
    private javax.swing.JButton but_startMon;
    private javax.swing.JTable distTable;
    private javax.swing.JScrollPane hypoScrollPane;
    private javax.swing.JTable hypoTable;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JScrollPane popScrollPane;
    private javax.swing.JSplitPane statsSplitPane;
    private javax.swing.JTextField txt_azDiamSqCoef;
    private javax.swing.JTextField txt_degXsecCoef;
    private javax.swing.JTextField txt_elDiamSqCoef;
    private javax.swing.JTextField txt_exposedDecay;
    private javax.swing.JTextField txt_freckMatchCoef;
    private javax.swing.JTextField txt_freckMismatchCoef;
    private javax.swing.JTextField txt_hiddenDecay;
    private javax.swing.JTextField txt_initStrength;
    private javax.swing.JTextField txt_mergeThresh;
    private javax.swing.JTextField txt_obsRetainedInFF;
    private javax.swing.JTextField txt_obsRetainedInHypo;
    private javax.swing.JTextField txt_overlapCoef;
    private javax.swing.JTextField txt_secondsCoef;
    private javax.swing.JTextField txt_survivalThresh;
    // End of variables declaration//GEN-END:variables

}
