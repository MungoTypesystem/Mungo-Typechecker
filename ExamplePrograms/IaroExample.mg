enum B {
	YES
	NO
}

class Account {
	{init; X}[
		X = { deposit; X
		      enoughFunds; <YES: {withdraw; X}
						    NO: X>
			  close; end}
	]

	void init(void x) {
		unit
	}
	
	void deposit(void x) {
		unit
	}
	
	B enoughFunds(void x) {
		YES
	}
	
	void withdraw(void x) {
		unit
	}
	
	void close(void x) {
		unit
	}
}


class Customer{
	infer[]

    Account savingsAccount
    Account debitAccount
    
    void createDebitAccount(void x){
        debitAccount = new Account;
        debitAccount.init(unit)
    }
    void createSavingsAccount(void x ){
        savingsAccount = new Account;
        savingsAccount.init(unit);
        savingsAccount.deposit(unit)
    }
    void transferFunds(void x){
        switch(savingsAccount.enoughFunds(unit)){
            YES:  savingsAccount.withdraw(unit); 
                  debitAccount.deposit(unit)
            NO: unit
        }
    }
    void closeAccounts(void x){
        debitAccount.close(unit);
        savingsAccount.close(unit)
    }
}


