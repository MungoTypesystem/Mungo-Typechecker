enum FileStatus 
{
	EOF
	NOTEOF
}

class File <C[U]> {
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