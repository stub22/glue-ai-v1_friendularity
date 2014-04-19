/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.unused;

import org.friendularity.dictation.main.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 *
 * @author matt
 */
public class TopicIdentifier {

    public static Map<Integer, String> DocPattern;
    public static Map<Integer, String> DocMeaning;
    public static Map<String, String> InvIndex;
    public static Map<Integer, Integer> DocTermCount;
    public static Map<String, Double> DocFreq;
    public static Map<Integer, Double> ScoreTable;

    public static int NumDocs;
    public static Map<String, String> WNTable;
    public static String nounSource = "index.noun";
    public static String verbSource = "index.verb";
    public static String adjSource = "index.adj";

    public static Integer useWN = 1;
    public static Integer useSoundex = 0;
    public static Double BaseWeight = 1.1;
    public static Double SoundexWeight = 1.0;
    public static Double WordNetWeight = 0.9;
    public static Double BestScoreCutoff = 0.3;
    public static Integer Debugflg = 1;

    public static Map<String, Integer> StopList = new HashMap<String, Integer>();
    public static Map<String, String> BeList = new HashMap<String, String>();

	public static void loadConfig(Properties props) throws Throwable{
		nounSource = props.getProperty("nounSource", TopicIdentifier.nounSource);
		verbSource = props.getProperty("verbSource", TopicIdentifier.verbSource);
        adjSource = props.getProperty("adjSource", TopicIdentifier.adjSource);
        useWN = Integer.parseInt(props.getProperty("useWN", TopicIdentifier.useWN.toString()));
        useSoundex = Integer.parseInt(props.getProperty("useSoundex", TopicIdentifier.useSoundex.toString()));
        BaseWeight = Double.parseDouble(props.getProperty("BaseWeight", TopicIdentifier.BaseWeight.toString()));
        WordNetWeight = Double.parseDouble(props.getProperty("WordNetWeight", TopicIdentifier.WordNetWeight.toString()));
        SoundexWeight = Double.parseDouble(props.getProperty("SoundexWeight", TopicIdentifier.SoundexWeight.toString()));
        BestScoreCutoff = Double.parseDouble(props.getProperty("BestScoreCutoff", TopicIdentifier.BestScoreCutoff.toString()));
		String meaningSource = props.getProperty("MEANINGXML");
		LoadKB(meaningSource);
	}

    public static void InitStopList()
	{
        StopList = new HashMap<String, Integer>();
        BeList = new HashMap<String, String>();
        StopList.put("a", 1);
        StopList.put("an", 1);
        StopList.put("and", 1);
        //StopList.put("are", 1);
        StopList.put("as", 1);
        StopList.put("at", 1);
        //StopList.put("be", 1);
        StopList.put("but", 1);
        StopList.put("by", 1);

        StopList.put("for", 1);
        StopList.put("if", 1);
        StopList.put("in", 1);
        StopList.put("into", 1);
        // StopList.put("is", 1);
        StopList.put("it", 1);

        StopList.put("no", 1);
        StopList.put("not", 1);
        StopList.put("of", 1);
        StopList.put("on", 1);
        StopList.put("or", 1);
        StopList.put("such", 1);

        StopList.put("that", 1);
        StopList.put("the", 1);
        StopList.put("their", 1);
        StopList.put("then", 1);
        StopList.put("there", 1);
        StopList.put("these", 1);

        StopList.put("they", 1);
        StopList.put("this", 1);
        StopList.put("to", 1);
        // StopList.put("was", 1);
        StopList.put("will", 1);
        StopList.put("with", 1);

        BeList.put("be", "is");
        BeList.put("is", "is");
        BeList.put("are", "is");
    }

    public static void ClearIndex() {
        DocMeaning = new HashMap<Integer, String>();
        DocPattern = new HashMap<Integer, String>();
        InvIndex = new HashMap<String, String>();
        DocTermCount = new HashMap<Integer, Integer>();
        DocFreq = new HashMap<String, Double>();
        //WNTable = new HashMap<String, String>();
        NumDocs = 0;
        InitStopList();
    }

    public static void AddToIndex(String Pattern, String Meaning) {
        String token;
        String token_snd;
        String token_wnkeys;
        String[] tokens_s = Pattern.toLowerCase().split(" ");
        Integer n = tokens_s.length;

        NumDocs += 1;
        DocPattern.put(NumDocs, Pattern);
        DocMeaning.put(NumDocs, Meaning.toUpperCase());

        Integer WNC = 0;
        Integer SNC = 0;

        for(int i = 0; i < n; i++){
            //For each word
            token = tokens_s[i].trim();

            String tokenSub = BeList.get(token);
            if(tokenSub != null){
				token = tokenSub;
			}
			Integer stop = StopList.get(token);
			if(stop == null)
				stop = 0;
            if(stop == 0){
                //increment the frequency of use in documents
                Double oldDF = DocFreq.get(token);
				if(oldDF == null)
					oldDF = 0.0;
                DocFreq.put(token, oldDF + 1);

                //Add to inverted index
                String OrigList = InvIndex.get(token);
				if(OrigList == null)
					OrigList = "";
                InvIndex.put(token, OrigList + "|" + NumDocs);

                //SOUNDEX INDEXING
                if(useSoundex == 1){
                    token_snd = ComputeSoundex(token);

                    //increment the frequency of use in documents
                    oldDF = DocFreq.get(token_snd);
					if(oldDF == null)
						oldDF = 0.0;
                    DocFreq.put(token_snd, oldDF + 1);

                    //Add to inverted index
                    OrigList = InvIndex.get(token_snd);
					if(OrigList == null)
						OrigList = "";
                    InvIndex.put(token_snd, OrigList + "|" + NumDocs);
                    SNC += 1;
				}

                //WORDNET INDEXING
                if(useWN == 1){
                    token_wnkeys = WNTable.get(token);
					if(token_wnkeys == null)
						token_wnkeys = "";
                    String[] WNKeys = token_wnkeys.split("[|]");
                    for(String WKey : WNKeys){
                        if(WKey.length() > 0){
                            //increment the frequency of use in documents
                            oldDF = DocFreq.get(WKey);
							if(oldDF == null)
								oldDF = 0.0;
                            DocFreq.put(WKey, oldDF + 1);

                            //Add to inverted index
                            OrigList = InvIndex.get(WKey);
							if(OrigList == null)
								OrigList = "";
                            InvIndex.put(WKey, OrigList + "|" + NumDocs);
                            WNC += 1;
						}
					}
				}
			}
		}
        DocTermCount.put(NumDocs, WNC + n + SNC );  // for plain + soundex
    }

	public static List<Integer> SearchIndex(String searchkey, Integer limit)
	{
        String token;
        String token_snd;
        String token_wnkeys;
        String[] tokens_k = searchkey.toLowerCase().split(" ");
        Integer n = tokens_k.length;
        Double BestScore = -9999.0;

	        List<Integer> ResultList = new ArrayList<Integer>();
        ScoreTable = new HashMap<Integer, Double>();

        for(int i = 0; i < n; i++){
            //For each word
            token = tokens_k[i];

            String tokenSub = BeList.get(token);
            if(tokenSub != null)
				token = tokenSub;

            if(StopList.get(token) == null || StopList.get(token) == 0){

                token_snd = ComputeSoundex(token);
                token_wnkeys = WNTable.get(token);

                //look up the inverted index

                //How much is this token worth
				Double freq = DocFreq.get(token);
				if(freq == null)
					freq = 0.0;
                Double IDFQ = Math.log(NumDocs / (1.0 + freq));

                //Split the options
				String split = InvIndex.get(token);
				if(split == null)
					split = "";
                String[] DocList = split.split("[|]");
                Integer dn = DocList.length;
                int j = 0;
                int DocId;
                for(j = 1; j < dn; j++){
                    DocId = Integer.parseInt(DocList[j]);
					Integer cnt = DocTermCount.get(DocId);
					if(cnt == null)
						cnt = 0;
                    Double add_score = BaseWeight * IDFQ * (1.0 / (1.0 + cnt));
					Double old_score = ScoreTable.get(DocId);
					if(old_score == null)
						old_score = 0.0;
                    ScoreTable.put(DocId, old_score + add_score);
                    if( Debugflg == 1){
						System.out.println("  ---BSE " + token + " " + add_score);
					}
                    if( ScoreTable.get(DocId) > BestScore){
						BestScore = ScoreTable.get(DocId);
					}
                }

                //SOUNDEX INDEXING
                if( useSoundex == 1)
				{
                    //How much is this token worth
					freq = DocFreq.get(token_snd);
					if(freq == null)
						freq = 0.0;
                    Double IDFQSnd = Math.log(NumDocs / (1.0 + freq));

                    //Split the options
					split = InvIndex.get(token_snd);
					if(split == null)
						split = "";
                    String[] DocListSnd = split.split("[|]");
                    Integer dnSnd = DocListSnd.length;
                    //Integer j = 0;
                    //Long DocId;
                    for(j = 1; j< dnSnd; j++)
					{
                        DocId = Integer.parseInt(DocListSnd[j]);
						Integer cnt = DocTermCount.get(DocId);
						if(cnt == null)
							cnt = 0;
                        Double add_score = SoundexWeight * IDFQSnd * (1.0 / (1.0 + cnt));
						Double old_score = ScoreTable.get(DocId);
						if(old_score == null)
							old_score = 0.0;
						ScoreTable.put(DocId, old_score + add_score);
                        if(Debugflg == 1){
							System.out.println("  ---SND " + token_snd + " " + add_score);
						}
                        if(ScoreTable.get(DocId) > BestScore){
							BestScore = ScoreTable.get(DocId);
						}
                    }
                }

                //WORDNET INDEXING
                if( useWN == 1) {
					if(token_wnkeys == null)
						token_wnkeys = "";
                    String[] WNKeys = token_wnkeys.split("[|]");
                    for(String WKey : WNKeys){
                        if( WKey.length() > 0) {
                            //How much is this token worth
							freq = DocFreq.get(WKey);
							if(freq == null)
								freq = 0.0;
                            Double IDFQWN = Math.log(NumDocs / (1.0 + freq));

                            //Split the options
							split = InvIndex.get(WKey);
							if(split == null)
								split = "";
                            String[] DocListWN = split.split("[|]");
                            Integer dnWN = DocListWN.length;
                            //Integer j = 0;
                            //Long DocId;
                            for(j = 1; j<dnWN; j++){
                                DocId = Integer.parseInt(DocListWN[j]);
								Integer cnt = DocTermCount.get(DocId);
								if(cnt == null)
									cnt = 0;
                                Double add_score = WordNetWeight * IDFQWN * (1.0 / (1.0 + cnt));
								Double old_score = ScoreTable.get(DocId);
								if(old_score == null)
									old_score = 0.0;
								ScoreTable.put(DocId, old_score + add_score);
                                if( Debugflg == 1) {
									System.out.println("  ---WN " + WKey + " " + add_score);
								}
                                if( ScoreTable.get(DocId) > BestScore){
									BestScore = ScoreTable.get(DocId);
								}
                            } //j
                        }
                    } //WKEY
                }
            }
        }

        if( BestScore > 0) {
			for(Integer key : ScoreTable.keySet()){
                if(ScoreTable.get(key) >= BestScore * BestScoreCutoff) {
                    ResultList.add(key);
                }
            }
        }
        return ResultList;
    }

	public static void LoadKB(String MeaningXML) throws Throwable
	{
		ClearIndex();
		WNTable = new HashMap<String, String>();
		LoadWNIndex(nounSource);
		LoadWNIndex(verbSource);
		LoadWNIndex(adjSource);
		File f = new File(MeaningXML);
		FileReader fr = new FileReader(f);
		BufferedReader br = new BufferedReader(fr);
		if(f.length() > 0) {
			ClearIndex();
			while(br.ready())
			{
				String ThoughtLine = br.readLine();
				if(ThoughtLine.contains("Token")) {

					ThoughtLine = ThoughtLine.replace("<Token text=", "");
					ThoughtLine = ThoughtLine.replace("/>", "");
					ThoughtLine = ThoughtLine.replace("kind=", "|");
					ThoughtLine = ThoughtLine.replace("meaning=", "|");
					ThoughtLine = ThoughtLine.replace("importance=", "|");
					ThoughtLine = ThoughtLine.replace((char)9, '"');
					ThoughtLine = ThoughtLine.replace("\"", "");

					String[] flds = ThoughtLine.split("[|]");
					if(flds.length == 4) {
						String keywords = flds[0].toLowerCase().trim();
						String kind = flds[1].toLowerCase().trim();
						String meaning = flds[2].toLowerCase().trim();
						String importance = flds[3];
						AddToIndex(keywords, meaning);

					}
				}
			}
			br.close();
		}
		System.out.println("Load KB Complete");
    }
    // Soundex from
    // http://www.codeproject.com/KB/aspnet/Soundex.aspx

    public static String ComputeSoundex(String Word)
	{
        return ComputeSoundex(Word, 4);
    }
    public static String ComputeSoundex(String Word, Integer Length)
	{
        // Value to return
        String Value = "";
        // Size of the word to process
        Integer Size = Word.length();
        // Make sure the word is at least two characters in length
        if(Size > 1) {
            // Convert the word to all uppercase
            Word = Word.toUpperCase();
            // Conver to the word to a character array for faster processing
            char[] Chars = Word.toCharArray();
            // Buffer to build up with character codes
            String Buffer = "";
            // The current and previous character codes
            Integer PrevCode = 0;
            Integer CurrCode = 0;
            // Append the first character to the buffer
            Buffer += Chars[0];
            // Prepare variables for loop
            Integer i;
            Integer LoopLimit = Size - 1;
            // Loop through all the characters and convert them to the proper character code
            for(i = 1; i < LoopLimit; i++){
				char c = Chars[i];
				if(Utils.list('A', 'E', 'I', 'O', 'U', 'H', 'W', 'Y').contains(c)){
                        CurrCode = 0;
				}else if(Utils.list('B', 'F', 'P', 'V').contains(c)){
                        CurrCode = 1;
				}else if(Utils.list('C', 'G', 'J', 'K', 'Q', 'S', 'X', 'Z').contains(c)){
					CurrCode = 2;
				}else if(Utils.list('D', 'T').contains(c)){
					CurrCode = 3;
				}else if(Utils.list('L').contains(c)){
					CurrCode = 4;
				}else if(Utils.list('M', 'N').contains(c)){
					CurrCode = 5;
				}else if(Utils.list('R').contains(c)){
					CurrCode = 6;
				}
                // Check to see if the current code is the same as the last one
                if(CurrCode != PrevCode && CurrCode != 0) {
                        Buffer += CurrCode;
                }
                // if the buffer size meets the length limit, then exit the loop
                if(Buffer.length() == Length) {
					break;
                }
            }
            // Padd the buffer if required
            while(Buffer.length() < Length) {
                Buffer += "0";
            }
            // Set the return value
            Value = Buffer;
        }else{
            Value = Word + "999";
        }
        // Return the computed soundex
        return Value;
    }

    public static void LoadWNIndex(String WNINDEX) throws Throwable
	{
		File file = new File(WNINDEX);
		FileReader fr = new FileReader(file);
		BufferedReader br = new BufferedReader(fr);
        if(file.length() > 0) {
            ClearIndex();
			while(br.ready())
			{
                String WordNetLine = br.readLine();
                if(WordNetLine.contains("@"))
				{
                    String[] flds = WordNetLine.toLowerCase().trim().split(" ");
                    if(flds.length > 4) {
                        String Lemma = flds[0];
                        String Lemma_snd = ComputeSoundex(Lemma);
                        String Pos = flds[1];
                        String synset_cnt = flds[2];
                        String p_cnt = flds[3];
                        Integer L = flds.length - 1;
                        Integer f = 1 + (L - Integer.parseInt(synset_cnt));
                        Integer i = 0;
                        for(i = f; i < L; i++)
						{
                            String SynsetID = "SYNID" + flds[i];
                            //Add to inverted index
                            String OrigList = WNTable.get(Lemma);
							if(OrigList == null)
								OrigList = "";
                            WNTable.put(Lemma, OrigList + "|" + SynsetID);
                            String OrigList2 = WNTable.get(Lemma_snd);
							if(OrigList2 == null)
								OrigList2 = "";
                            WNTable.put(Lemma_snd, OrigList2 + "|" + SynsetID);

                        }
                    }
                }
            }
			br.close();
        }
        System.out.println("Wordnet loaded");
    }
}