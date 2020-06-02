# Type Checking Examples in Mungo
This page is a repository of program examples written in the Mungo language and typechecked using the `mungob` tool. All examples type check, and where a main class is available, the output of executing the programs is also available. 

- [File Example](#file-example)
- [Travel Agency Example](#travel-agency-example)

## File Example

| ![File protocol](https://github.com/MungoTypesystem/MungoBehaviouralSeparation/raw/master/protocol_figures/file_protocol.png) | 
|:--:| 
| *Protocol for File class* |

The classic example for typestate programming. A file follows a simple protocol. A file can only be read after it has been opened, and only while the file contains more data than what has already been read. The figure above illustrates this protocol, which can also be seen, expressed as a usage, in the code below.

The implementation of the File class itself is only a skeleton implementation, but the usage is specified according to an actual file implementation. So the interesting class is the main class which actually reads a file. Omitting the line `f.open()` would cause a type error, as would removing the line `f.close()`. 

```java
enum FileStatus 
{
	EOF
	NOTEOF
}

class File {
	{open; checkable} [
		checkable = {
			isEOF; <EOF: checkable
					NOTEOF: {read; checkable}>
			close; end
		}
	]

	bool haveMoreData
	bool data
	
	void open(void x) {
		data = true;
		haveMoreData = true
	}
	
	FileStatus isEOF(void x) {
		if (haveMoreData) {
			NOTEOF
		} else {
			EOF
		}
	}
	
	bool read(void x) {
		data
	}
	
	void close(void x) {
		unit
	}
}


class start {
	{main; end}[] 
	
	File file
	
	void main(void x) {
		file = new File;
		file.open(unit);
		(loop:
		switch(file.isEOF(unit)) {
			EOF: unit
			NOTEOF: file.read(unit);
					continue loop
		});
		file.close(unit)
	}
	
}
```

## Travel Agency Example

| ![Agency protocol](https://github.com/MungoTypesystem/MungoBehaviouralSeparation/raw/master/protocol_figures/agency_protocol.png) |
|:--:| 
| *Protocol for Agency class* |

We present another classic example, this time from session type theory. A customer must contact a travel agency and book a trip, after bargaining for a price that is acceptable. The protocol for the travel agent is shown above, and shows a quote can be continuosly requested until the price is reasonable, in which case the trip is accepted, and the travel service is contacted and given the buyers information, and a date for the trip is returned. 

```java
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
```
