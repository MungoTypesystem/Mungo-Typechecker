enum BIN
{
	ZERO
	ONE
	THREE
}

class Example
{
	/*X[ X = {assign1; end
			assign1; X}]*/
	infer[]
	/*X1[ X0 = end 
		X1 = { assign1; < ZERO: end ONE: end > 
			   assign1; end 
			   assign2; < ZERO: end ONE: end > 
			   assign2; end 
			   assign3; < ZERO: end ONE: end > 
			   assign3; end 
			   assign1; < ZERO: X1 ONE: X1 > 
			   assign1; X1 
			   assign2; < ZERO: X1 ONE: X1 > 
			   assign2; X1 
			   assign3; < ZERO: X1 ONE: X1 > 
			   assign3; X1 } ] */
	
	BIN b
	BIN c
	BIN d
	
	BIN assign1(void x) {
		b = ZERO; b
	}

	BIN assign2(void y) {
		b = ONE; b
	}
	
	BIN assign3(void x) {
		b = ONE; b
	} 
}
