/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * AdvancedPanel.java
 *
 * Created on Jan 6, 2010, 9:40:02 PM
 */

package org.friendularity.dictation.gui.advanced;

import org.friendularity.dictation.main.Vocabulary;
import org.friendularity.dictation.gui.main.DictationImpl;
//import com.hansonrobotics.dictation.gui.advanced.resources.ModifiedFlowLayout;
import org.friendularity.dictation.gui.advanced.resources.ModifiedFlowLayout;
import org.friendularity.dictation.jmx.JMXWrapper;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.ListSelectionModel;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileFilter;


/**
 *
 * @author Eamq
 */
public class AdvancedPanel extends javax.swing.JPanel {

	private DictationImpl myImpl;
	
	private boolean isCharEngEnabled	 = true;
	private boolean isMeneEnabled		 = true;
	private boolean isInformativeEnabled = true;
	private boolean isResponsiveEnabled  = true;

	private static List<Vocabulary> theVocabs    = new ArrayList<Vocabulary>();
    private List<Vocabulary> myVocabList         = new ArrayList<Vocabulary>();

	private List<String> myMeanings           = new ArrayList<String>();
	private List<String> myAddedMeanings      = new ArrayList<String>();
	private List<String> myRemovalList        = new ArrayList<String>();

	private Map<String, MeaningInfoCollection> myMeaningMap = new HashMap<String, MeaningInfoCollection>();

	private DefaultListModel myVocabListModel		 = new DefaultListModel();
	private DefaultListModel myMeaningListModel		 = new DefaultListModel();
	private DefaultListModel myAddedMeaningListModel = new DefaultListModel();

    private ListSelectionModel myVocabLSM;
	private ListSelectionModel myMeaningLSM;
	private ListSelectionModel myAddedMeaningLSM;

	private JMXWrapper myJMXWrapper = new JMXWrapper();

    /** Creates new form AdvancedPanel */
    public AdvancedPanel() {
        initComponents();
        // Default constructor not supported, use AdvancedPanel(DictationImpl impl)
    }

    // Use this constructor
	public AdvancedPanel(DictationImpl impl) {
        initComponents();
		initAdvancedPanel(impl);
    }

    public void initAdvancedPanel(DictationImpl impl){
        myImpl = impl;
		for(Vocabulary v : myImpl.getVocabManager().getVocabularies()){
			if(!v.getName().equals("MISC")){
				theVocabs.add(v);
                myVocabList.add(v);
				myVocabListModel.addElement(v.getName());
				for(String token : v.getTokenMeaningMap().keySet()){
					String meaning = v.getTokenMeaning(token);
					if(!v.getMiscCondition().equals("")){
						meaning = v.getMiscCondition() + " " + meaning;
					}
					if(!myMeaningMap.containsKey(meaning)){
						myMeaningMap.put(meaning, new MeaningInfoCollection(v.getName(), v.getDestination(), v.getType()));
						myMeaningMap.get(meaning).setButton(new JButton(meaning));
					}else {
						boolean matchFound = false;
						for(String[] info : myMeaningMap.get(meaning).getInfo()){
							if(info[0].equals(v.getName())){
								matchFound = true;
							}
						}
						if(!matchFound){
							myMeaningMap.get(meaning).addInfo(v.getName(), v.getDestination(), v.getType());
						}
					}
				}
			}
		}

        myVocabLSM = listVocabs.getSelectionModel();
		myVocabLSM.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				vocabLSMChanged(e);
			}
		});
		myMeaningLSM = listMeanings.getSelectionModel();
		myAddedMeaningLSM = listAddedMeanings.getSelectionModel();

        txtSearchVocabs.getDocument().addDocumentListener(new DocumentListener() {
            public void insertUpdate(DocumentEvent e) {
                txtSearchVocabsModified(e);
            }
            public void removeUpdate(DocumentEvent e) {
                txtSearchVocabsModified(e);
            }
            public void changedUpdate(DocumentEvent e) { }
        });

        txtSearchMeanings.getDocument().addDocumentListener(new DocumentListener() {
            public void insertUpdate(DocumentEvent e) {
                changeMeaningList();
            }
            public void removeUpdate(DocumentEvent e) {
                changeMeaningList();
            }
            public void changedUpdate(DocumentEvent e) { }
        });

		chkCharEngEnabled.setSelected(isCharEngEnabled);
		chkMeneEnabled.setSelected(isMeneEnabled);
		chkInformativeEnabled.setSelected(isInformativeEnabled);
		chkResponsiveEnabled.setSelected(isResponsiveEnabled);

        pnlButtonPanel.setLayout(new ModifiedFlowLayout(FlowLayout.LEADING, 10, 10));
        jScrollPane4.getVerticalScrollBar().setUnitIncrement(16);
    }

    public void resetAll(){
        myVocabLSM.clearSelection();      
        myMeaningLSM.clearSelection();
        myAddedMeaningLSM.clearSelection();
        txtSearchVocabs.setText("");
        txtSearchMeanings.setText("");
        txtSearchMeanings.setBackground(Color.WHITE);
        for(String meaning : myAddedMeanings){
            myRemovalList.add(meaning);
		}
		for(String meaning : myRemovalList){
			myAddedMeaningListModel.removeElement(meaning);
			myAddedMeanings.remove(meaning);
			pnlButtonPanel.remove(getButton(meaning));
		}
		myRemovalList.clear();
		listAddedMeanings.setModel(myAddedMeaningListModel);
		pnlButtonPanel.repaint();
		pnlButtonPanel.revalidate();
    }

    public void saveToFile(JFileChooser fc) throws IOException{
        int returnVal = fc.showSaveDialog(this);
        if(returnVal == JFileChooser.APPROVE_OPTION){
            File saveFile = fc.getSelectedFile();
            String fileName = saveFile.getAbsolutePath();
            Pattern pattern = Pattern.compile("(.*)\\.(.*)", Pattern.CASE_INSENSITIVE);
            Matcher matcher = pattern.matcher(fileName);
            if(!matcher.matches()){
                saveFile = new File(fileName + ".avo");
            }
            DataOutputStream out = new DataOutputStream(
                new BufferedOutputStream(new FileOutputStream(saveFile))
            );
            for(String meaning : myAddedMeanings){
                out.writeUTF(meaning);
            }
            out.flush();
            out.close();
        }
    }

    public void loadFromFile(JFileChooser fc) throws IOException{
        int returnVal = fc.showOpenDialog(this);
        if(returnVal == JFileChooser.APPROVE_OPTION){
            File openFile = fc.getSelectedFile();
            DataInputStream in = new DataInputStream(
                new BufferedInputStream(new FileInputStream(openFile))
            );
            resetAll();
            String data = in.readUTF();
            try {
                while (true){
                    myAddedMeanings.add(data);
                    myAddedMeaningListModel.addElement(data);
                    addButton(data);
                    data = in.readUTF();
                }
            } catch (EOFException e) {
                listAddedMeanings.setModel(myAddedMeaningListModel);
                pnlButtonPanel.repaint();
                pnlButtonPanel.revalidate();
            }
        }
    }

    public void txtSearchVocabsModified(DocumentEvent e){
        txtSearchVocabs.setBackground(Color.WHITE);
        myVocabList.clear();
        myVocabListModel.clear();
        String text = txtSearchVocabs.getText();
        text = text.replaceAll("\\s", "\\\\E[_\\\\s]\\\\Q");
        text = "(.*)\\Q" + text + "\\E(.*)";
        Pattern vocabPattern = Pattern.compile(text, Pattern.CASE_INSENSITIVE);
        int matchCount = 0;
        for (Vocabulary v : theVocabs) {
            String name = v.getName();
            Matcher vocabMatches = vocabPattern.matcher(name);
            if (vocabMatches.matches() && !name.equals("MISC")) {
                myVocabList.add(v);
                myVocabListModel.addElement(name);
                matchCount++;
            }
        }
        if (matchCount == 0) {
            txtSearchVocabs.setBackground(Color.PINK);
        }
        if(txtSearchMeanings.getText().equals("")){
            txtSearchMeanings.setBackground(Color.WHITE);
        }
        listVocabs.setModel(myVocabListModel);    
    }

	public JButton getButton(String meaning){
		return myMeaningMap.get(meaning).getButton();
	}

    public void addButton(String meaning){
        pnlButtonPanel.add(getButton(meaning)).addMouseListener(new MouseListener() {
			public void mouseClicked(MouseEvent e) {
				meaningButtonClicked(e);
			}
			public void mousePressed(MouseEvent e) { }
			public void mouseReleased(MouseEvent e) { }
			public void mouseEntered(MouseEvent e) { }
			public void mouseExited(MouseEvent e) {	}
		});
    }

	public void vocabLSMChanged(ListSelectionEvent e){
		myVocabLSM = (ListSelectionModel)e.getSource();
        changeMeaningList();
	}

    public void changeMeaningList(){
        txtSearchMeanings.setBackground(Color.WHITE);
        myMeaningListModel.clear();
        myMeanings.clear();
        String text = txtSearchMeanings.getText();
        text = text.replaceAll("\\s", "\\\\E[_\\\\s]\\\\Q");
        text = "(.*)\\Q" + text + "\\E(.*)";
        Pattern searchPattern = Pattern.compile(text, Pattern.CASE_INSENSITIVE);
        int minIndex = myVocabLSM.getMinSelectionIndex();
        int maxIndex = myVocabLSM.getMaxSelectionIndex();
        int matchCount = 0;
        for (int i=minIndex; i<=maxIndex; i++) {
            if(myVocabLSM.isSelectedIndex(i)){
                Vocabulary vocab = myVocabList.get(i);
                for(String token : vocab.getTokenMeaningMap().keySet()){
                    String meaning = vocab.getTokenMeaning(token);
                    if(!vocab.getMiscCondition().equals("")){
                        meaning = vocab.getMiscCondition() + " " + meaning;
                    }
                    Matcher searchMatches = searchPattern.matcher(meaning);
                    if(!myMeanings.contains(meaning) && searchMatches.matches()){
                     	myMeanings.add(meaning);
                        matchCount++;
                	}
              	}
            }
        }
        if (matchCount == 0) {
            txtSearchMeanings.setBackground(Color.PINK);
        }
        Collections.sort(myMeanings);
        for(String m : myMeanings){
            myMeaningListModel.addElement(m);
        }
        listMeanings.setModel(myMeaningListModel);
    }

	public void meaningButtonClicked(MouseEvent e){
		String meaning = ((JButton)e.getSource()).getText();
		for(String[] info : myMeaningMap.get(meaning).getInfo()){
			String vocab = info[0];
			String dest  = info[1];
			String type  = info[2];
			if(type.equals("INFORMATIVE") && isInformativeEnabled){
				if(isCharEngEnabled && dest.equals("CHARACTER_ENGINE")){
					myImpl.getVocabManager().SendInformative(vocab, meaning, dest, myJMXWrapper);
				}
				if(isMeneEnabled && dest.equals("MESSAGING_NEXUS")){
					myImpl.getVocabManager().SendInformative(vocab, meaning, dest, myJMXWrapper);
				}
			}
			if(type.equals("RESPONSIVE") && isResponsiveEnabled){
				if(isCharEngEnabled && dest.equals("CHARACTER_ENGINE")){
                    Map<String,Double> map = new HashMap();
                    map.put(meaning, 1.0);
					myImpl.getVocabManager().SendCharEngine(map, myJMXWrapper);
				}
				if(isMeneEnabled && dest.equals("MESSAGING_NEXUS")){
					myImpl.getVocabManager().SendNexus(meaning, true, myJMXWrapper);
				}
			}
		}
	}

    public void setFileFilter(JFileChooser fc) {
        fc.setFileFilter(new FileFilter() {

            @Override
            public boolean accept(File f) {
                if(f.isDirectory()){
                    return true;
                }
                Pattern pattern = Pattern.compile("(.*)\\.avo\\z", Pattern.CASE_INSENSITIVE);
                Matcher matcher = pattern.matcher(f.getName());
                if(matcher.matches()){
                    return true;
                } else {
                    return false;
                }
            }

            @Override
            public String getDescription() {
                return "*.avo";
            }
        });
    }

    public void addCustomMeaning(){
        String meaning = txtAddCustomMeaning.getText();
        meaning = meaning.toUpperCase().trim().replaceAll("\\s", "_");
        if(!meaning.equals("") && !myAddedMeanings.contains(meaning)){
            myAddedMeanings.add(meaning);
			myAddedMeaningListModel.addElement(meaning);
            myMeaningMap.put(meaning, new MeaningInfoCollection("CUSTOM", "CHARACTER_ENGINE", "RESPONSIVE"));
            myMeaningMap.get(meaning).setButton(new JButton(meaning));
			addButton(meaning);
		}
		listAddedMeanings.setModel(myAddedMeaningListModel);
        txtAddCustomMeaning.setText("");
		pnlButtonPanel.revalidate();
    }

    private List<Vocabulary> getSelectedVocabs(){
        List<Vocabulary> vocabs = new ArrayList();
        for(int i : listVocabs.getSelectedIndices()){
            vocabs.add(myVocabList.get(i));
        }
        return vocabs;
    }

    private List<String> buildDictionary(List<Vocabulary> vocabs){
        List<String> dict = new ArrayList();
        for(Vocabulary v : vocabs){
            dict.addAll(v.getTokens());
        }
        Collections.sort(dict);
        return dict;
    }

    private void saveDictionary(File file, List<String> dict) throws Throwable{
        file.createNewFile();
        FileWriter fw = new FileWriter(file);
        String str = "";
        for(String s : dict){
            str += s + "\n";
        }
        fw.write(str);
        fw.close();
    }

    private void generateDictionary() throws Throwable{
        JFileChooser fc = new JFileChooser();
        fc.setSelectedFile(new File("vocab_lines.txt"));
        int returnVal = fc.showSaveDialog(this);
        if(returnVal == JFileChooser.APPROVE_OPTION){
            File saveFile = fc.getSelectedFile();
            String fileName = saveFile.getAbsolutePath();
            Pattern pattern = Pattern.compile("(.*)\\.(.*)", Pattern.CASE_INSENSITIVE);
            Matcher matcher = pattern.matcher(fileName);
            if(!matcher.matches()){
                saveFile = new File(fileName + ".txt");
            }
            List<String> dict = buildDictionary(getSelectedVocabs());
            saveDictionary(saveFile, dict);
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

        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        jSplitPane2 = new javax.swing.JSplitPane();
        jSplitPane3 = new javax.swing.JSplitPane();
        jPanel4 = new javax.swing.JPanel();
        btnAddMeaning = new javax.swing.JButton();
        txtSearchMeanings = new javax.swing.JTextField();
        jScrollPane2 = new javax.swing.JScrollPane();
        listMeanings = new javax.swing.JList();
        jPanel5 = new javax.swing.JPanel();
        btnRemoveMeaning = new javax.swing.JButton();
        jScrollPane3 = new javax.swing.JScrollPane();
        listAddedMeanings = new javax.swing.JList();
        btnAddCustomMeaning = new javax.swing.JButton();
        txtAddCustomMeaning = new javax.swing.JTextField();
        jPanel3 = new javax.swing.JPanel();
        txtSearchVocabs = new javax.swing.JTextField();
        btnAddMeaning1 = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        listVocabs = new javax.swing.JList(myVocabListModel);
        jScrollPane4 = new javax.swing.JScrollPane();
        pnlButtonPanel = new javax.swing.JPanel();
        chkCharEngEnabled = new javax.swing.JCheckBox();
        chkMeneEnabled = new javax.swing.JCheckBox();
        chkInformativeEnabled = new javax.swing.JCheckBox();
        chkResponsiveEnabled = new javax.swing.JCheckBox();

        setName("Form"); // NOI18N

        jSplitPane1.setDividerLocation(250);
        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setName("jSplitPane1"); // NOI18N

        jPanel1.setName("jPanel1"); // NOI18N

        jSplitPane2.setDividerLocation(250);
        jSplitPane2.setName("jSplitPane2"); // NOI18N

        jSplitPane3.setDividerLocation(250);
        jSplitPane3.setName("jSplitPane3"); // NOI18N

        jPanel4.setName("jPanel4"); // NOI18N

        org.jdesktop.application.ResourceMap resourceMap = org.jdesktop.application.Application.getInstance(org.friendularity.dictation.gui.main.DictationGrabberApp.class).getContext().getResourceMap(AdvancedPanel.class);
        btnAddMeaning.setText(resourceMap.getString("btnAddMeaning.text")); // NOI18N
        btnAddMeaning.setName("btnAddMeaning"); // NOI18N
        btnAddMeaning.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnAddMeaningActionPerformed(evt);
            }
        });

        txtSearchMeanings.setColumns(81);
        txtSearchMeanings.setText(resourceMap.getString("txtSearchMeanings.text")); // NOI18N
        txtSearchMeanings.setName("txtSearchMeanings"); // NOI18N

        jScrollPane2.setName("jScrollPane2"); // NOI18N

        listMeanings.setName("listMeanings"); // NOI18N
        jScrollPane2.setViewportView(listMeanings);

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(btnAddMeaning, javax.swing.GroupLayout.DEFAULT_SIZE, 249, Short.MAX_VALUE)
            .addComponent(txtSearchMeanings, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 249, Short.MAX_VALUE)
            .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 249, Short.MAX_VALUE)
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel4Layout.createSequentialGroup()
                .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 187, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(txtSearchMeanings, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnAddMeaning)
                .addContainerGap())
        );

        jSplitPane3.setLeftComponent(jPanel4);

        jPanel5.setName("jPanel5"); // NOI18N

        btnRemoveMeaning.setText(resourceMap.getString("btnRemoveMeaning.text")); // NOI18N
        btnRemoveMeaning.setName("btnRemoveMeaning"); // NOI18N
        btnRemoveMeaning.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnRemoveMeaningActionPerformed(evt);
            }
        });

        jScrollPane3.setName("jScrollPane3"); // NOI18N

        listAddedMeanings.setName("listAddedMeanings"); // NOI18N
        jScrollPane3.setViewportView(listAddedMeanings);

        btnAddCustomMeaning.setText(resourceMap.getString("btnAddCustomMeaning.text")); // NOI18N
        btnAddCustomMeaning.setName("btnAddCustomMeaning"); // NOI18N
        btnAddCustomMeaning.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnAddCustomMeaningActionPerformed(evt);
            }
        });

        txtAddCustomMeaning.setText(resourceMap.getString("txtAddCustomMeaning.text")); // NOI18N
        txtAddCustomMeaning.setName("txtAddCustomMeaning"); // NOI18N
        txtAddCustomMeaning.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                txtAddCustomMeaningKeyPressed(evt);
            }
        });

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel5Layout.createSequentialGroup()
                .addComponent(txtAddCustomMeaning, javax.swing.GroupLayout.DEFAULT_SIZE, 178, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnAddCustomMeaning, javax.swing.GroupLayout.PREFERRED_SIZE, 53, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addComponent(btnRemoveMeaning, javax.swing.GroupLayout.DEFAULT_SIZE, 237, Short.MAX_VALUE)
            .addComponent(jScrollPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 237, Short.MAX_VALUE)
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnAddCustomMeaning)
                    .addComponent(txtAddCustomMeaning, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 184, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnRemoveMeaning)
                .addContainerGap())
        );

        jSplitPane3.setRightComponent(jPanel5);

        jSplitPane2.setRightComponent(jSplitPane3);

        jPanel3.setName("jPanel3"); // NOI18N

        txtSearchVocabs.setColumns(81);
        txtSearchVocabs.setText(resourceMap.getString("txtSearchVocabs.text")); // NOI18N
        txtSearchVocabs.setName("txtSearchVocabs"); // NOI18N

        btnAddMeaning1.setText(resourceMap.getString("btnAddMeaning1.text")); // NOI18N
        btnAddMeaning1.setName("btnAddMeaning1"); // NOI18N
        btnAddMeaning1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnAddMeaning1ActionPerformed(evt);
            }
        });

        jScrollPane1.setName("jScrollPane1"); // NOI18N

        listVocabs.setName("listVocabs"); // NOI18N
        jScrollPane1.setViewportView(listVocabs);

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 249, Short.MAX_VALUE)
            .addComponent(btnAddMeaning1, javax.swing.GroupLayout.DEFAULT_SIZE, 249, Short.MAX_VALUE)
            .addComponent(txtSearchVocabs, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 249, Short.MAX_VALUE)
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 189, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(txtSearchVocabs, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnAddMeaning1)
                .addContainerGap())
        );

        jSplitPane2.setLeftComponent(jPanel3);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 749, Short.MAX_VALUE)
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 257, Short.MAX_VALUE)
        );

        jSplitPane1.setTopComponent(jPanel1);

        jScrollPane4.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        jScrollPane4.setName("jScrollPane4"); // NOI18N
        jScrollPane4.setVerifyInputWhenFocusTarget(false);

        pnlButtonPanel.setName("pnlButtonPanel"); // NOI18N

        javax.swing.GroupLayout pnlButtonPanelLayout = new javax.swing.GroupLayout(pnlButtonPanel);
        pnlButtonPanel.setLayout(pnlButtonPanelLayout);
        pnlButtonPanelLayout.setHorizontalGroup(
            pnlButtonPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 747, Short.MAX_VALUE)
        );
        pnlButtonPanelLayout.setVerticalGroup(
            pnlButtonPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 239, Short.MAX_VALUE)
        );

        jScrollPane4.setViewportView(pnlButtonPanel);

        jSplitPane1.setRightComponent(jScrollPane4);

        chkCharEngEnabled.setText(resourceMap.getString("chkCharEngEnabled.text")); // NOI18N
        chkCharEngEnabled.setName("chkCharEngEnabled"); // NOI18N
        chkCharEngEnabled.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                chkCharEngEnabledMouseClicked(evt);
            }
        });

        chkMeneEnabled.setText(resourceMap.getString("chkMeneEnabled.text")); // NOI18N
        chkMeneEnabled.setName("chkMeneEnabled"); // NOI18N
        chkMeneEnabled.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                chkMeneEnabledMouseClicked(evt);
            }
        });

        chkInformativeEnabled.setText(resourceMap.getString("chkInformativeEnabled.text")); // NOI18N
        chkInformativeEnabled.setName("chkInformativeEnabled"); // NOI18N
        chkInformativeEnabled.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                chkInformativeEnabledMouseClicked(evt);
            }
        });

        chkResponsiveEnabled.setText(resourceMap.getString("chkResponsiveEnabled.text")); // NOI18N
        chkResponsiveEnabled.setName("chkResponsiveEnabled"); // NOI18N
        chkResponsiveEnabled.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                chkResponsiveEnabledMouseClicked(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 751, Short.MAX_VALUE)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addComponent(chkInformativeEnabled)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(chkResponsiveEnabled)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 163, Short.MAX_VALUE)
                .addComponent(chkCharEngEnabled)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(chkMeneEnabled))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 497, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(chkMeneEnabled)
                    .addComponent(chkCharEngEnabled)
                    .addComponent(chkInformativeEnabled)
                    .addComponent(chkResponsiveEnabled)))
        );
    }// </editor-fold>//GEN-END:initComponents

	private void chkCharEngEnabledMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_chkCharEngEnabledMouseClicked
		if(isCharEngEnabled){
			isCharEngEnabled = false;
			chkCharEngEnabled.setSelected(false);
		}else {
			isCharEngEnabled = true;
			chkCharEngEnabled.setSelected(true);
		}
	}//GEN-LAST:event_chkCharEngEnabledMouseClicked

	private void chkMeneEnabledMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_chkMeneEnabledMouseClicked
		if(isMeneEnabled){
			isMeneEnabled = false;
			chkMeneEnabled.setSelected(false);
		}else {
			isMeneEnabled = true;
			chkMeneEnabled.setSelected(true);
		}
	}//GEN-LAST:event_chkMeneEnabledMouseClicked

	private void chkInformativeEnabledMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_chkInformativeEnabledMouseClicked
		if(isInformativeEnabled){
			isInformativeEnabled = false;
			chkInformativeEnabled.setSelected(false);
		}else {
			isInformativeEnabled = true;
			chkInformativeEnabled.setSelected(true);
		}
	}//GEN-LAST:event_chkInformativeEnabledMouseClicked

	private void chkResponsiveEnabledMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_chkResponsiveEnabledMouseClicked
		if(isResponsiveEnabled){
			isResponsiveEnabled = false;
			chkResponsiveEnabled.setSelected(false);
		}else {
			isResponsiveEnabled = true;
			chkResponsiveEnabled.setSelected(true);
		}
	}//GEN-LAST:event_chkResponsiveEnabledMouseClicked

    private void btnAddCustomMeaningActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAddCustomMeaningActionPerformed
        addCustomMeaning();
    }//GEN-LAST:event_btnAddCustomMeaningActionPerformed

    private void txtAddCustomMeaningKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_txtAddCustomMeaningKeyPressed
        if(evt.getKeyCode()== KeyEvent.VK_ENTER){
            addCustomMeaning();
        }
    }//GEN-LAST:event_txtAddCustomMeaningKeyPressed

	private void btnAddMeaningActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAddMeaningActionPerformed
		int minIndex = myMeaningLSM.getMinSelectionIndex();
		int maxIndex = myMeaningLSM.getMaxSelectionIndex();
		for(int i=minIndex; i<=maxIndex; i++){
			String meaning = myMeanings.get(i);
			if(myMeaningLSM.isSelectedIndex(i) && !myAddedMeanings.contains(meaning)){
				myAddedMeanings.add(meaning);
				myAddedMeaningListModel.addElement(meaning);
				addButton(meaning);
			}
		}
		listAddedMeanings.setModel(myAddedMeaningListModel);
		pnlButtonPanel.revalidate();
	}//GEN-LAST:event_btnAddMeaningActionPerformed

	private void btnRemoveMeaningActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRemoveMeaningActionPerformed
		int minIndex = myAddedMeaningLSM.getMinSelectionIndex();
		int maxIndex = myAddedMeaningLSM.getMaxSelectionIndex();
		for(int i=minIndex; i<=maxIndex; i++){
			if(myAddedMeaningLSM.isSelectedIndex(i)){
				myRemovalList.add(myAddedMeanings.get(i));
			}
		}
		for(String meaning : myRemovalList){
			myAddedMeaningListModel.removeElement(meaning);
			myAddedMeanings.remove(meaning);
			pnlButtonPanel.remove(getButton(meaning));
		}
		myRemovalList.clear();
		listAddedMeanings.setModel(myAddedMeaningListModel);
		pnlButtonPanel.repaint();
		pnlButtonPanel.revalidate();
	}//GEN-LAST:event_btnRemoveMeaningActionPerformed

    private void btnAddMeaning1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAddMeaning1ActionPerformed
        try{
            generateDictionary();
        }catch(Throwable t){
            System.out.println("Failed to save dictionary");
        }
    }//GEN-LAST:event_btnAddMeaning1ActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnAddCustomMeaning;
    private javax.swing.JButton btnAddMeaning;
    private javax.swing.JButton btnAddMeaning1;
    private javax.swing.JButton btnRemoveMeaning;
    private javax.swing.JCheckBox chkCharEngEnabled;
    private javax.swing.JCheckBox chkInformativeEnabled;
    private javax.swing.JCheckBox chkMeneEnabled;
    private javax.swing.JCheckBox chkResponsiveEnabled;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JSplitPane jSplitPane2;
    private javax.swing.JSplitPane jSplitPane3;
    private javax.swing.JList listAddedMeanings;
    private javax.swing.JList listMeanings;
    private javax.swing.JList listVocabs;
    private javax.swing.JPanel pnlButtonPanel;
    private javax.swing.JTextField txtAddCustomMeaning;
    private javax.swing.JTextField txtSearchMeanings;
    private javax.swing.JTextField txtSearchVocabs;
    // End of variables declaration//GEN-END:variables
}