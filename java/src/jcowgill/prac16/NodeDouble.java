package jcowgill.prac16;

public class NodeDouble {

	private Double value;
	private NodeDouble next;

	public NodeDouble(Double value){
		this.value = value;
		next = null;
	}

	public Double getValue(){
		return value;
	}

	public NodeDouble getNext(){
		return next;
	}

	public void setNext(NodeDouble node){
		next = node;
	}

	public void setValue(Double value){
		this.value = value;
	}

}
