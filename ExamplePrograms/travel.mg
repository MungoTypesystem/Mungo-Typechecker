enum Answer {
    YES
    NO
}

class PriceValidator {
    {isFairPrice; <YES: end NO: end>}[]
    Answer isFairPrice(int x) {
        if (x <= 100) {
            YES
        } else {
            NO
        }
    }
}

class Agency {
    {init; Initialised}[
        Initialised = {getQuote; Bargain}
        Bargain = {getQuote; Bargain accept; end}
    ]
    int curQuote

    void init(void x) {
        curQuote = (210)
    }

    int getQuote(void x) {
        curQuote = (curQuote - 10);
        curQuote
    }

    Service[{setAddress; {getDate; end}}] accept(void x) {
        new Service
    }

}

class Service {
    {setAddress; {getDate; end}}[]
    void setAddress(void x) {
        unit
    }

    int getDate(void x) {27}
}

class Customer {
    {bargain; {finalize; end}}[]
    int price
    PriceValidator pv
    Service s
    int date

    void bargain(Agency[Initialised] agent) {
        pv = new PriceValidator;
        price = agent.getQuote(unit);
        switch (pv.isFairPrice(price)) {
            YES: s = agent.accept(unit)
            NO: (
                pv = new PriceValidator;
                loop: (
                    price = agent.getQuote(unit);
                    switch (pv.isFairPrice(price)) {
                        YES: s = agent.accept(unit)
                        NO: (
							//s = agent.accept(unit)
                            pv = new PriceValidator;
                            continue loop
                        )
                    } 
                )
            )
        }
    }

    void finalize(void x) {
        s.setAddress(unit);
        date = s.getDate(unit)
    }
}


class main {
    {main; end}[]
    Customer c
    Agency a
    
    void main(void x) {
        c = new Customer;
        a = new Agency;
        a.init(unit);
        c.bargain(a);
        c.finalize(unit)
    }
}