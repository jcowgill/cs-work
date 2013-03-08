package jcowgill.prac16.sets;

public class NodeDouble {

	private Double value;
	private NodeDouble next;

	public NodeDouble(Double value){
		this(value, null);
	}

    public NodeDouble(Double value, NodeDouble next){
        this.value = value;
        this.next = next;
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
