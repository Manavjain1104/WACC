all:
	sbt assembly

test:
	sbt test
	# bash compile_and_run.sh

clean:
	rm -rf *.jar
	sbt clean
	find . -name "*.s" -type f -delete
	rm -f $*

.PHONY: all test clean

