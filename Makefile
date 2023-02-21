BUILD_DIR := ./build

all:
	mkdir -p $(BUILD_DIR)
	sbt assembly

test:
	sbt test
	# bash compile_and_run.sh
	

clean:
	rm -rf *.jar
	sbt clean
	find . -name "*.s" -type f -delete
	rm -f $* test
	rm -r $(BUILD_DIR)

.PHONY: all test clean