/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.unused;

import java.util.Map;



/**
 *
 * @author matt
 */
public class LevenshteinCalculator {
    public static Integer Minimum3(Integer a, Integer b, Integer c) {
		return Math.min(a, Math.min(b, c));
    }
    public static Double Minimum3(Double a, Double b, Double c) {
		return Math.min(a, Math.min(b, c));
    }

    public static Integer LD1(String s, String t) {
		Integer InsertionCost = 1;
		Integer DeletionCost = 1;
        Integer m = t.length(); // length of t
        Integer n = s.length();
        Integer i;
        Integer j; // iterates through t
        String s_i; // ith character of s
        String t_j; // jth character of t
        Integer SubsitutionCost;
        if(n == 0){
            return m;
        }
        if(m == 0){
            return n;
        }
		int[][] d = new int[n][m];
        for(i = 0; i < n; i++){
            d[i][0] = i;
        }
        for(j = 0; j < m; j++){
            d[0][j] = j;
        }

        for(i = 1; i < n; i++){
            s_i = s.substring(i,1);
            for(j = 1; j < m; j++){
                t_j = t.substring(j, 1);
                if(s_i.equals(t_j)){
                    SubsitutionCost = 0;
				}else{
                    SubsitutionCost = 1;
                }
                d[i][j] = Minimum3(d[i - 1][j] + InsertionCost, d[i][j - 1] + DeletionCost, d[i - 1][j - 1] + SubsitutionCost);
            }
        }
        return d[n][m];
    }
    public static Double Calculate(String s, String t, String[] explain, Map<String, Double> CostTable, Map<String, Double> SynoTable) {
		String[] tokens_s = (". " + s.trim()).split(" ");
        String[] tokens_t = (". " + t.trim()).split(" ");

        Integer m = tokens_s.length - 1;
        Integer n = tokens_t.length - 1;
        Integer max_m_n;
        Integer i; // iterates through s
        Integer j; // iterates through t
        String s_i; // ith character of s
        String t_j; // jth character of t
        Double SubsitutionCost; // cost

        if((n > m)){
            tokens_s = (". " + t.trim()).split(" ");
            tokens_t = (". " + s.trim()).split(" ");
            n = tokens_s.length - 1;
            m = tokens_t.length - 1;
        }
        max_m_n = n;
        if(m > max_m_n){
			max_m_n = m;
		}

        if(n == 0){
            return (double)m;
        }
        if(m == 0){
            return (double)n;
        }

		double[][] d = new double[n][m];
		String[][] p = new String[n][m];

		for(i = 0; i < n; i++){
            d[i][0] = i;
            p[i][0] = "";
            if(i > 0){
				p[i][0] = p[i - 1][0] + ": delS " + tokens_s[i];
			}
        }

        for(j = 0; j < m; j++){
            d[0][j] = j;
            p[0][j] = "";
            if(j > 0){
				p[0][j] = p[0][j - 1] + ": insT " + tokens_t[j];
			}
        }

        Double InsertionCost = 1.0;
		Double DeletionCost = 1.0;
        Double DeletionInfoCost = 1.0;

        for(i = 1; i < n; i++){
            s_i = tokens_s[i];
            Integer sl = s_i.length();

			for(j = 1; j < m; j++){
                t_j = tokens_t[j];
			    DeletionInfoCost = CostTable.get(t_j);
                if(DeletionInfoCost == 0){
					DeletionInfoCost = 10.0;  //0 for non-entry
				}
                DeletionCost = DeletionInfoCost;

                //Lookup the information based Insertion / Deletion Costs if any
                Double InsertInfoCost = 1.0;

				InsertInfoCost = CostTable.get(t_j);
				if(InsertInfoCost == null || InsertInfoCost == 0){
					InsertInfoCost = 10.0; //0 for non-entry
				}
				InsertionCost = InsertInfoCost; //Adding to the Destination

                String planStep;
                if(s_i.equals(t_j)){
                    SubsitutionCost = 0.0;
                    planStep = " :match " + t_j;
				}else{
                    SubsitutionCost = InsertionCost; //1
                    String SynoPair = s_i + ":" + t_j;
                    Double synov = SynoTable.get(SynoPair);
                    if(synov > 0){
                        SubsitutionCost = synov;
					}else{
                        Integer tl = t_j.length();
                        Double max_ts = (double)sl;

                        if(tl > max_ts){
							max_ts = (double)tl;
						}
                        SubsitutionCost = (double)LD1(s_i, t_j)/max_ts;
                        SynoTable.put(SynoPair, SubsitutionCost);
                    }
                    planStep = " :sub " + s_i + " " + t_j + " " + SubsitutionCost;
                }

				Double IV = d[i - 1][j] + InsertionCost;
                Double DV = d[i][j - 1] + DeletionCost;
                Double SV = d[i - 1][j - 1] + SubsitutionCost;
                d[i][j] = Minimum3(IV, DV, SV);
                if((d[i][j] == IV)){
					p[i][j] = p[i - i][j] + " :ins " + t_j + " " + InsertionCost;
				}
                if((d[i][j] == DV)){
					p[i][j] = p[i][j - 1] + " :del " + t_j + " " + DeletionCost;
				}
                if((d[i][j] == SV)){
					p[i][j] = p[i - 1][j - 1] + planStep;
				}
            }
        }

        String PD2 = p[n][m];
        explain[0] = PD2;
		return 1.0 - d[n][m] / max_m_n;
    }
}
