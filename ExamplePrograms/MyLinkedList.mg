enum ListState
{
	Element
	Empty
}

class LinkedBoolList
{
	Init[
		Init = {
			init; Usage
		}
		Usage = {
			push; Usage
			getState; <Empty: StopAble
				       Element: PopAble>
		}
		StopAble = {
			push; Usage
			stop; end
		}
		PopAble = {
			push; Usage
			pop; Usage
		}
	]
	
	LinkedBoolListImpl head
	LinkedBoolListImpl tmp
	bool retValue
	ListState tmpState
	
	void init(void x) {
		head = new LinkedBoolListImpl;
		head.emptyList(unit)
	}
	
	void push(bool v) {
		tmp = new LinkedBoolListImpl;
		tmp.push(v);
		tmp.setNode(head);
		head = tmp
	}
	
	void getState(void x) {
		switch(head.getState(unit)) {
			Empty: head = new LinkedBoolListImpl;
				   head.emptyList(unit);
				   Empty
			Element: Element
		}
	}
	
	bool pop(void x) {
		retValue = head.pop(unit);
		head = head.popNode(unit);
		retValue
	}
	
	void stop(void x) {
		head.getState(unit)
	}

}


class LinkedBoolListImpl 
{
	Init [
		Init = {
			push; {setNode; FullList}
			emptyList; EmptyList
		}
		EmptyList = {
			getState; <Element: end 
					   Empty: end>
		}
		FullList = {
			getState; <Element: GetElement 
					   Empty: end>
		}
		GetElement = {
			pop; {popNode; end} 
		}
	]
	
	bool elem
	ListState state
	LinkedBoolListImpl node
	
	void emptyList(void x) {
		state = Empty
	}
	
	void push(bool v) {
		elem = v;
		state = Element
	}
	
	void setNode(LinkedBoolListImpl[FullList] newNode) {
		node = newNode
	}
	
	ListState getState(void x) {
		state
	}
	
	bool pop(void x) {
		elem
	}
		
	LinkedBoolListImpl[FullList] popNode(void x) {
		node
	}
}