package org.friendularity.bundle.demo.gmteach;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.HashMap;
import java.util.Map;

public class ListenerMap<K, V> extends HashMap<K, V> implements Map<K, V> {

	public class LEntry {

		private K key;
		private V value;

		public LEntry(K k, V v) {
			this.key = k;
			this.value = v;
		}

		public K getKey() {
			return key;
		}

		public Object getValue() {
			return value;
		}

	}

	public static final String PROP_PUT = "put";
	private PropertyChangeSupport propertySupport;

	public ListenerMap() {
		super();
		propertySupport = new PropertyChangeSupport(this);
	}

	@Override public V put(K k, V v) {
		V old = super.put(k, v);
		propertySupport.firePropertyChange(PROP_PUT, newEntry(k, old), newEntry(k, v));
		return old;
	}

	private Object newEntry(K k, V v) {
		return new LEntry(k, v);
	}

	public V putNoFire(K k, V v) {
		V old = super.put(k, v);
		return old;
	}

	@Override public V remove(Object k) {
		V old = super.remove(k);
		propertySupport.firePropertyChange(PROP_PUT, old, null);
		return old;
	}

	public V removeNoFire(Object k) {
		V old = super.remove(k);
		return old;
	}

	public void addPropertyChangeListener(PropertyChangeListener listener) {
		propertySupport.addPropertyChangeListener(listener);
	}

	public void removePropertyChangeListener(PropertyChangeListener listener) {
		propertySupport.removePropertyChangeListener(listener);
	}
}