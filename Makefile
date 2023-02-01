all:
	sbt compile

test:
	sbt test

clean:
	rm -rf *.jar
	sbt clean

.PHONY: all test clean

