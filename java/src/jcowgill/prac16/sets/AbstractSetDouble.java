package jcowgill.prac16.sets;

public abstract class AbstractSetDouble {

	protected AbstractSetDouble(){
		// do nothing
	}

	public abstract boolean add(Double d);
	public abstract boolean remove(Double d);
	public abstract void clear();
	public abstract boolean contains(Double d);
	public abstract int size();


}
