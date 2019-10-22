class id <C[U]> {
	{identity; end}[]
	
	C f
	
	C[U] identity(C[U] a) {
		f = a;
		f
	}
}

class C {
	{id; end}[]
	
	bool id(bool x) {
		x
	}
}

class main {
	{main; end}[]
	
	id<C[{id;end}]> i
	
	void main(void x) {
		i = new id<C[{id;end}]>;
		i.id(true);
		unit
	}
}