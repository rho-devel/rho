GS = -g
JC = javac
.SUFFIXES: .java .class
.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = \
        CaptureFileSplitter.java \
        CaptureTestGen.java \
        Prefixes.java \
        ProcessTC.java 

default: classes

classes: $(CLASSES:.java=.class)

clean:
	$(RM) *.class
