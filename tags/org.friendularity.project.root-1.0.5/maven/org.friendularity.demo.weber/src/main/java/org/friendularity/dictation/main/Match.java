/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.main;

/**
 *
 * @author Matt Stevenson
 */
public class Match implements Comparable{
	public String Token;
	public String Meaning;
	public Double MatchScore;
	public Match(String t, String m, Double s){
		Token = t;
		Meaning = m;
		MatchScore = s;
	}

	public int compareTo(Object o) {
		Match m = (Match)o;
		return MatchScore.compareTo(m.MatchScore);
	}

    public String toHtml()
    {
        String html = "<div class=\"match\">";
        html += " Token: " + this.Token;
        html += " Meaning: " + this.Meaning;
        html += " Match Score: " + this.MatchScore;
        html += "</div>";
        return html;
    }
}