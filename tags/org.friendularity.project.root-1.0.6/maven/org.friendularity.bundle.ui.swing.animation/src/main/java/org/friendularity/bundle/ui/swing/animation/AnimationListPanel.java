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
 * AnimationJobListPanel.java
 *
 * Created on Apr 28, 2011, 1:33:08 PM
 */

package org.friendularity.bundle.ui.swing.animation;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Pattern;
import javax.swing.BoxLayout;
import javax.swing.border.Border;
import javax.swing.border.MatteBorder;
import org.jflux.api.core.Source;
import org.mechio.api.animation.Animation;
import org.mechio.api.animation.messaging.RemoteAnimationPlayerClient;

/**
 *
 * @author Matthew Stevenson <www.friendularity.org>
 */
public class AnimationListPanel extends javax.swing.JPanel {
    private final static Border theBorder = new MatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY);
    private SortedSet<AnimationPanel> myAnimationJobPanels;
    private Map<Animation,AnimationPanel> myAnimationJobMap;
    private Source<RemoteAnimationPlayerClient> myPlayerSource;
    private PlayerSource myIndirectSource;
    private List<String> myFilters;
    private SortedSet<AnimationPanel> myFilteredAnimationJobPanels;
    
    /** Creates new form AnimationJobListPanel */
    public AnimationListPanel() {
        initComponents();
        myAnimationJobPanels = new TreeSet();
        myAnimationJobMap = new HashMap();
        myAnimationJobsPanel.setLayout(new BoxLayout(myAnimationJobsPanel,BoxLayout.Y_AXIS));
        myIndirectSource = new PlayerSource();
        
        jTextField1.addKeyListener(new java.awt.event.KeyAdapter() {
            @Override
            public void keyReleased(java.awt.event.KeyEvent evt) {
                jTextField1KeyReleased(evt);
            }
        });
    }
    
    public void setSource(Source<RemoteAnimationPlayerClient> playerSource){
        myPlayerSource = playerSource;
    }
    
    public void addAnimation(Animation anim){
        _addAnimation(anim);
    }
    
    public void addAnimations(List<Animation> anims){
        for(Animation anim : anims){
            _addAnimation(anim);
        }
    }
    
    private void _addAnimation(Animation anim){
        if(anim == null){
            return;
        }
        if(myAnimationJobMap.containsKey(anim)){
           return;
        }
        AnimationPanel panel = new AnimationPanel();
        panel.setBorder(theBorder);
        panel.setAnimation(anim);
        panel.setPlayerSource(myIndirectSource);
        myAnimationJobMap.put(anim, panel);
        myAnimationJobPanels.add(panel);
        
        filterCache();
    }
    
    public void clearAnimations(){
        RemoteAnimationPlayerClient player = myIndirectSource.getValue();
        myAnimationJobPanels.clear();
        myAnimationJobsPanel.removeAll();
        if(player != null){
            for(Animation anim : myAnimationJobMap.keySet()){
                player.stopAnimation(anim);
            }
        }
        myAnimationJobMap.clear();
        
        filterCache();
    }
    
    public void jTextField1KeyReleased(java.awt.event.KeyEvent evt) {
        setFilters(jTextField1.getText());
    }
    
    public void setFilters(String filterStr) {
        myFilters = new ArrayList<String>();

        String[] filters = filterStr.split(",");
        for(String s : filters) {
            s = s.trim();
            if(!s.isEmpty()) {
                myFilters.add(s);
            }
        }
        filterCache();
    }
    
    private void filterCache() {
        if(myFilters == null || myFilters.isEmpty()) {
            myFilteredAnimationJobPanels = myAnimationJobPanels;
            refresh();
            
            return;
        }

        myFilteredAnimationJobPanels = new TreeSet<AnimationPanel>();
        for(AnimationPanel thals : myAnimationJobPanels) {
            if(filterList(thals)) {
                myFilteredAnimationJobPanels.add(thals);
            }
        }

        refresh();
    }
    
    private boolean filterList(AnimationPanel thals){
        String s = thals.getAnimationName();
        
        for(String f : myFilters){
            Pattern p = Pattern.compile(
                    ".*" + f + ".*", Pattern.MULTILINE | Pattern.DOTALL);
            if(s != null && p.matcher(s).matches()){
                return true;
            }
        }
        
        return false;
    }
    
    private void refresh(){
        myAnimationJobsPanel.removeAll();
        
        for(AnimationPanel p: myFilteredAnimationJobPanels) {
            myAnimationJobsPanel.add(p);
        }
        
        myAnimationJobsPanel.revalidate();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        myAnimationJobsScrollPane = new javax.swing.JScrollPane();
        myAnimationJobsPanel = new javax.swing.JPanel();
        jTextField1 = new javax.swing.JTextField();

        myAnimationJobsScrollPane.setBorder(null);

        javax.swing.GroupLayout myAnimationJobsPanelLayout = new javax.swing.GroupLayout(myAnimationJobsPanel);
        myAnimationJobsPanel.setLayout(myAnimationJobsPanelLayout);
        myAnimationJobsPanelLayout.setHorizontalGroup(
            myAnimationJobsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 278, Short.MAX_VALUE)
        );
        myAnimationJobsPanelLayout.setVerticalGroup(
            myAnimationJobsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 320, Short.MAX_VALUE)
        );

        myAnimationJobsScrollPane.setViewportView(myAnimationJobsPanel);

        jTextField1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextField1ActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(myAnimationJobsScrollPane)
            .addComponent(jTextField1)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jTextField1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(myAnimationJobsScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 314, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    private void jTextField1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jTextField1ActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_jTextField1ActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTextField jTextField1;
    private javax.swing.JPanel myAnimationJobsPanel;
    private javax.swing.JScrollPane myAnimationJobsScrollPane;
    // End of variables declaration//GEN-END:variables
    
    class PlayerSource implements Source<RemoteAnimationPlayerClient> {
        @Override
        public RemoteAnimationPlayerClient getValue() {
            if(myPlayerSource == null){
                return null;
            }
            return myPlayerSource.getValue();
        }
    }
}
