MMC = mmc

.PHONY: runtests
runtests: test_json
	@./test_json

.PHONY: runtests-verbose
runtests-verbose: test_json
	@./test_json -v

Mercury.modules: $(wildcard *.m) $(wildcard ../src/*.m)
	$(MMC) -f $(wildcard *.m) $(wildcard ../src/*.m)

test_json: Mercury.modules
	$(MMC) --make $@

tags: $(wildcard *.m) $(wildcard ../src/*.m)
	mtags $(wildcard *.m) $(wildcard ../src/*.m)

.PHONY: realclean
realclean:
	$(MMC) --make test_json.realclean
	/bin/rm -rf Mercury
	/bin/rm -f Mercury.modules
	/bin/rm -f FAILED_TESTS ABORTED_TESTS
