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
 * AnimationPanel.java
 *
 * Created on Jul 13, 2012, 1:25:18 AM
 */
package org.friendularity.bundle.ui.swing.animation;

import java.awt.Color;
import java.util.HashMap;
import java.util.Map;
import javax.swing.RepaintManager;
import org.jflux.api.core.Source;
import org.mechio.api.animation.Animation;
import org.mechio.api.animation.Channel;
import org.mechio.api.animation.editor.ChannelEditor;
import org.mechio.api.animation.messaging.RemoteAnimationPlayerClient;
import org.rwshop.swing.animation.config.PathProperties;
import org.rwshop.swing.common.scaling.CoordinateScalar;
import org.rwshop.swing.common.scaling.DefaultCoordinateScalar;

/**
 *
 * @author Matthew Stevenson
 */
public class AnimationPanel extends javax.swing.JPanel implements Comparable<AnimationPanel> {
    private Animation myAnimation;
    private Source<RemoteAnimationPlayerClient> myPlayerSource;
    
    /** Creates new form AnimationPanel */
    public AnimationPanel() {
        initComponents();
    }

    public void setAnimation(Animation anim){
        if(anim == null){
            return;
        }
        myAnimation = anim;
        lblAnim.setText(myAnimation.getVersion().display());
        Map<Integer,PathProperties> props = new HashMap();
        for(Channel chan : anim.getChannels()){
            int id = chan.getId();
            Color col = ChannelEditor.getChannelColor(id);
            PathProperties chanProps = new PathProperties(null, col, null);
            props.put(chan.getId(), chanProps);
        }
        CoordinateScalar scalar = new DefaultCoordinateScalar();
        pnlAnim.setScalar(scalar);
        pnlAnim.setAnimation(myAnimation, props);
        RepaintManager.currentManager(this).markCompletelyDirty(pnlAnim);
    }
    
    public Animation getAnimation(){
        return myAnimation;
    }

    public void setPlayerSource(Source<RemoteAnimationPlayerClient> player){
        myPlayerSource = player;
    }
    
    public String getAnimationName() {
        return myAnimation.getVersion().display();
    }
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        pnlAnim = new org.rwshop.swing.animation.AnimationRenderer();
        btnLoop = new javax.swing.JButton();
        btnStop = new javax.swing.JButton();
        btnPlay = new javax.swing.JButton();
        lblAnim = new javax.swing.JLabel();

        setMaximumSize(new java.awt.Dimension(3509999, 64));
        setMinimumSize(new java.awt.Dimension(0, 64));
        setPreferredSize(new java.awt.Dimension(350, 64));

        javax.swing.GroupLayout pnlAnimLayout = new javax.swing.GroupLayout(pnlAnim);
        pnlAnim.setLayout(pnlAnimLayout);
        pnlAnimLayout.setHorizontalGroup(
            pnlAnimLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 189, Short.MAX_VALUE)
        );
        pnlAnimLayout.setVerticalGroup(
            pnlAnimLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 30, Short.MAX_VALUE)
        );

        btnLoop.setText("Loop");
        btnLoop.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnLoopActionPerformed(evt);
            }
        });

        btnStop.setText("Stop");
        btnStop.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnStopActionPerformed(evt);
            }
        });

        btnPlay.setText("Play");
        btnPlay.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnPlayActionPerformed(evt);
            }
        });

        lblAnim.setText("Animation Name");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addComponent(pnlAnim, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnPlay)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnLoop)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnStop)
                .addContainerGap())
            .addComponent(lblAnim, javax.swing.GroupLayout.DEFAULT_SIZE, 350, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(lblAnim)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(btnLoop, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(btnStop, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(btnPlay, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(pnlAnim, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    private void btnPlayActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPlayActionPerformed
        if(myAnimation == null || myPlayerSource == null){
            return;
        }
        RemoteAnimationPlayerClient player = myPlayerSource.getValue();
        if(player == null){
            return;
        }
        player.playAnimation(myAnimation);
    }//GEN-LAST:event_btnPlayActionPerformed

    private void btnLoopActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnLoopActionPerformed
        if(myAnimation == null || myPlayerSource == null){
            return;
        }
        RemoteAnimationPlayerClient player = myPlayerSource.getValue();
        if(player == null){
            return;
        }
        player.loopAnimation(myAnimation);
    }//GEN-LAST:event_btnLoopActionPerformed

    private void btnStopActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnStopActionPerformed
        if(myAnimation == null || myPlayerSource == null){
            return;
        }
        RemoteAnimationPlayerClient player = myPlayerSource.getValue();
        if(player == null){
            return;
        }
        player.stopAnimation(myAnimation);
    }//GEN-LAST:event_btnStopActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnLoop;
    private javax.swing.JButton btnPlay;
    private javax.swing.JButton btnStop;
    private javax.swing.JLabel lblAnim;
    private org.rwshop.swing.animation.AnimationRenderer pnlAnim;
    // End of variables declaration//GEN-END:variables

    @Override
    public int compareTo(AnimationPanel o) {
        String name = getAnimationName();
        String otherName = o.getAnimationName();
        
        return name.compareTo(otherName);
    }
}
