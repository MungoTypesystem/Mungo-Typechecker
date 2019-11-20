class ND 
{
	FC[
		FC = {
			f; FC
			f; end
		}
	]
	
	void f(void x) {
		unit
	}
}

class main {
	{main; end}[]
	
	ND f
	
	void main(void x) {
		f = new ND;
		f.f(unit);
		f.f(unit)
	}
}