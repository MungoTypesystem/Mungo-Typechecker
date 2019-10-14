enum myEnum {
	one
	two
}

class switchable {
	S [
		S = {
			mySwitchFun; <one: S two: end>
		}
		
	]
	
	void mySwitchFun(bool x) {
		if (true) {
			two
		} else {
			one
		}
	}
}


class test1 {
	{init; U} [
		U = end
	]
	
	bool b
	switchable field 
	
	void init(void x) {
		field = new switchable;
		b = true
	}

	void callMany(void x) {
		loop: switch(field.mySwitchFun(b)) {
			one: unit
			two: b = false; continue loop
		}
	}
	
	
}