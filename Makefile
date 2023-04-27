init:
	git submodule update --init
	cd rocket-chip && git submodule update --init api-config-chipsalliance hardfloat

rtl: clean
	mill xs-issue.test.runMain GenRtl -td build --full-stacktrace -X sverilog -e sverilog --emission-options disableRegisterRandomization --no-cse
	sed -i 's/\?baifenhao\!/%/g' build/*.sv
	rm build/*.fir
	rm build/*.anno.json

help:
	mill xs-issue.test.runMain GenRtl --help

idea:
	mill -i mill.scalalib.GenIdea/idea

clean:
	rm -rf build/*