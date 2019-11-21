class simple
{
	{f; end}[]
	
	void f(void x) {
		unit
	}
}

class InferMe
{
	/*x2[
        x1 = { final;end always;x1 final;x2 } 
        x2 = { always;end init;x1 always;x2 } ]*/
	start[start =  {init; middle always;start always;end}
	      middle = {final;end  final;start always;middle}]
	
	simple f1
	simple f2
	
	void init(void x) {
		f1 = new simple
	}
	
	void always(void x) {
		/*f2 = new simple;
		f2.f(unit); 
		f2 = null */
		unit
	}
	
	void never(void x) {
		f2.f(unit)
	}
	
	void final(void x) {
		f1.f(unit);
		f1 = null
	}
}