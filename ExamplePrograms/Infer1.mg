class simple
{
	{f; end}[]
	
	void f(void x) {
		unit
	}
}

class InferMe
{
	infer []
	
	simple f1
	simple f2
	
	void init(void x) {
		f1 = new simple
	}
	
	void always(void x) {
		f2 = new simple;
		f2.f(unit)
	}
	
	void never(void x) {
		f2.f(unit)
	}
	
	void final(void x) {
		f1.f(unit)
	}
}