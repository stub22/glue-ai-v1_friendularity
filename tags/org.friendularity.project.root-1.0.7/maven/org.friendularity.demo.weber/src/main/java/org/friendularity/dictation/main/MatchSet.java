/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.dictation.main;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

/**
 *
 * @author Matt Stevenson
 */
public class MatchSet implements List<Match> {
	List<Match> myMatches;

	public MatchSet()
	{
		myMatches = new ArrayList<Match>();
	}

	public void sort(){
		Collections.sort(myMatches);
		//We want the highest confidence match first
		Collections.reverse(myMatches);
	}

	public boolean containsMeaning(String meaning){
		for(Match m : this){
			if(m.Meaning.equals(meaning))
				return true;
		}
		return false;
	}

	public void prepend(String prefix){
		for(Match m : this){
			m.Meaning = prefix + " " + m.Meaning;
			m.MatchScore += prefix.length() + 1;
		}
	}

	public boolean add(Match m){
		return myMatches.add(m);
	}
	public void add(int index, Match element) {
		myMatches.add(index, element);
	}
	public boolean addAll(Collection<? extends Match> c) {
		return myMatches.addAll(c);
	}
	public boolean addAll(int index, Collection<? extends Match> c) {
		return myMatches.addAll(c);
	}
	public void clear() {
		myMatches.clear();
	}
	public boolean contains(Object o) {
		return myMatches.contains((Match)o);
	}
	public boolean containsAll(Collection<?> c) {
		return myMatches.containsAll(c);
	}
	public Match get(int index) {
		return myMatches.get(index);
	}
	public int indexOf(Object o) {
		return myMatches.indexOf((Match)o);
	}
	public boolean isEmpty() {
		return myMatches.isEmpty();
	}
	public Iterator<Match> iterator() {
		return myMatches.iterator();
	}
	public int lastIndexOf(Object o) {
		return myMatches.lastIndexOf((Match)o);
	}
	public ListIterator<Match> listIterator() {
		return myMatches.listIterator();
	}
	public ListIterator<Match> listIterator(int index) {
		return myMatches.listIterator(index);
	}
	public boolean remove(Object o) {
		return myMatches.remove((Match)o);
	}
	public Match remove(int index) {
		return myMatches.remove(index);
	}
	public boolean removeAll(Collection<?> c) {
		return myMatches.removeAll(c);
	}
	public boolean retainAll(Collection<?> c) {
		return myMatches.retainAll(c);
	}
	public Match set(int index, Match element) {
		return myMatches.set(index, element);
	}
	public int size() {
		return myMatches.size();
	}
	public List<Match> subList(int fromIndex, int toIndex) {
		return myMatches.subList(fromIndex, toIndex);
	}
	public Object[] toArray() {
		return myMatches.toArray();
	}
	public <T> T[] toArray(T[] a) {
		return myMatches.toArray(a);
	}

    public String toHtml()
    {
        String html = "<div class=\"matches\">";
        for(Match m : this) {
            html += m.toHtml();
        }
        html+="</div>";
        return html;
    }
}
