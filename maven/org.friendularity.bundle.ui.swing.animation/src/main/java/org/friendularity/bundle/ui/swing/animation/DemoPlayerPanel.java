/*
 * Copyright 2014 the Friendularity Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * DemoPlayerPanel.java
 *
 * Created on Jul 13, 2012, 2:50:38 AM
 */
package org.friendularity.bundle.ui.swing.animation;

/**
 *
 * @author Matthew Stevenson
 */
public class DemoPlayerPanel extends javax.swing.JPanel {

    /** Creates new form DemoPlayerPanel */
    public DemoPlayerPanel() {
        initComponents();
//        animationLibraryPanel1.setPlayerSource(playerClientPanel1);
//        animationLibraryPanel1.setPromptLibrary(animPromptPanel1.getValue());
//        animPromptPanel1.setPlayerSource(playerClientPanel1);
    }

	public AnimationLibraryPanel getAnimationLibraryPanel(){
		return animationLibraryPanel1;
	}

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        animationLibraryPanel1 = new org.friendularity.bundle.ui.swing.animation.AnimationLibraryPanel();
        playerClientPanel1 = new org.friendularity.bundle.ui.swing.animation.PlayerClientPanel();
        animPromptPanel1 = new org.friendularity.bundle.ui.swing.animation.AnimPromptPanel();
        jLabel1 = new javax.swing.JLabel();

        jLabel1.setText("AnimationPrompts");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(playerClientPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 547, Short.MAX_VALUE)
            .addComponent(animationLibraryPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 547, Short.MAX_VALUE)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jLabel1)
                .addContainerGap())
            .addComponent(animPromptPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 547, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(playerClientPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(animationLibraryPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, 395, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(animPromptPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private org.friendularity.bundle.ui.swing.animation.AnimPromptPanel animPromptPanel1;
    private org.friendularity.bundle.ui.swing.animation.AnimationLibraryPanel animationLibraryPanel1;
    private javax.swing.JLabel jLabel1;
    private org.friendularity.bundle.ui.swing.animation.PlayerClientPanel playerClientPanel1;
    // End of variables declaration//GEN-END:variables
}
