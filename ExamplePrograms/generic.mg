class id <C[U]> {
	{identity; end}[]
	
	C f
	
	C[U] identity(C[U] a) {
		f = a;
		f
	}
}

class B {
	{id; end}[]
	
	bool id(bool x) {
		x
	}
}

class main {
	{main; end}[]
	
	id<B[{id;end}]> i
	B b
	
	void main(void x) {
		b = new B;
		i = new id<B[{id;end}]>;
		b = i.identity(b);
		b.id(true);
		unit
	}
}