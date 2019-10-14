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
	
	void pop(void x) {
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
	Usage [
		Init = {
			push; {setNode; FullList}
			emptyList; EmptyList
		}
		EmptyList = {
			getState; <Element: end // not reachable
					   Empty: end>
		}
		FullList = {
			getState; <Element: GetElement
				       Empty: End>
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
	
	void setNode(LinkedBoolListImpl[Usage] newNode) {
		node = newNode
	}
	
	void getState(void x) {
		state
	}
	
	bool pop(void x) {
		elem
	}
		
	LinkedBoolListImpl[Usage] popNode(void x) {
		node
	}
}