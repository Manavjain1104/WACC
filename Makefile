all:
	sbt compile assembly

clean:
	sbt clean && rm -rf wacc-25-compiler.jar

.PHONY: all clean
