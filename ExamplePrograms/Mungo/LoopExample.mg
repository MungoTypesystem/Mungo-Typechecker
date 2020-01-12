enum Boolean {
    No 
    Yes 
}

class FileData {
    start[start = {done; end}]

    void done(void x) { x }
}

class File {
    {open; opened}[
        opened = {
            hasNext; <Yes: readable 
                      No:  opened>
            close; end
        }

        readable = {
            read; opened
            hasNext; <Yes: readable 
                      No:  opened>
            close; end
        }
    ]

    bool haveData

    void open(void x) {
        haveData = true 
    }

    Boolean hasNext(void y) {
        if (haveData) {
            haveData = false;
            Yes
        } else {
            No
        }   
    }

    FileData[start] read(void y) {
        new FileData
    }

    void close(void y) {
        y
    }
}

class FileClient {
    infer[]

    File f
    FileData fd

    void main(void x) {
        f = new File;
        f.open(x)
    }

    void readAll(void x) {
        con:
        switch(f.hasNext(x)) {
            Yes: fd = f.read(x);
                 fd.done(x);
                 continue con
            No: unit
        }
    }

    void final(void x) {
        f.close(x)
    }
}





/*class LoopImpl {
    Start[
        Start = { finished; <Yes:Start No:Start>
                  terminate; end }
    ]

    Boolean finished(void x) {
        Yes 
    }

    void terminate(void x) {
        unit
    }
}

class Client1 {
    infer[]

    LoopImpl loop

    void test(void x) {
        loop = new LoopImpl;
        out: 
        switch(loop.finished(unit)) {
            Yes:
                continue out
            No: 
                unit
        };
        loop.terminate(unit)
    }
}

class Iter {
    start[
        start = {
            hasNext; start
            next; start
            remove; end
        }
    ]

    bool hasNext(void x) {
        true 
    }

    void next(void x) {
        unit
    }

    void remove(void x) {
        unit
    }
}

class StateIterator {
    infer[]

    Iter iter

    void init(void x) {
        iter = new Iter
    }

    Boolean hasNext(void x) {
        if (iter.hasNext(x)) {
            Yes
        } else {
            No
        } 
    }
    
    void next(void x) {
        iter.next(x)
    }

    void remove(void x) {
        iter.remove(x)
    }
} */
