init:
	git submodule update --init
	cd rocket-chip && git submodule update --init api-config-chipsalliance hardfloat

rtl:
	mill xs-issue.test.runMain GenRtl -td build --full-stacktrace -X sverilog --emission-options disableRegisterRandomization

idea:
	mill -i mill.scalalib.GenIdea/idea

clean:
	rm -rf ./build