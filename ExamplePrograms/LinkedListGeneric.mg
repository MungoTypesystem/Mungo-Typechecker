enum ListState
{
	Element
	Empty
}

class <C[U]> LinkedList
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
	                      
	LinkedListImpl<C[U]> head
	LinkedListImpl<C[U]> tmp
	C[U] retValue
	ListState tmpState
	
	void init(void x) {
		head = new LinkedListImpl<C[U]>;
		head.emptyList(unit)
	}
	
	void push(C[U] v) {
		tmp = new LinkedListImpl<C[U]>;
		tmp.push(v);
		tmp.setNode(head);
		head = tmp
	}
	
	void getState(void x) {
		switch(head.getState(unit)) {
			Empty: head = new LinkedListImpl<C[U]>;
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


class <C[U]> LinkedListImpl 
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
	
	C[U]  elem
	ListState state
	LinkedListImpl<C[U]> node
	
	void emptyList(void x) {
		state = Empty
	}
	
	void push(C[U] v) {
		elem = v;
		state = Element
	}
	
	void setNode(LinkedListImpl<C[U]>[Usage] newNode) {
		node = newNode
	}
	
	void getState(void x) {
		state
	}
	
	C[U] pop(void x) {
		elem
	}
		
	LinkedListImpl<C[U]>[Usage] popNode(void x) {
		node
	}
}