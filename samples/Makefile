MMC = mmc

PROGS = pretty messages

pretty$(EXEEXT): pretty.m Mercury.modules
	$(MMC) --make $@

messages$(EXEEXT): messages.m Mercury.modules
	$(MMC) -lcurl --make $@

Mercury.modules: $(wildcard *.m) $(wildcard ../src/*.m)
	$(MMC) -f $(wildcard *.m) $(wildcard ../src/*.m)

tags: $(wildcard *.m) $(wildcard ../src/*.m)
	mtags $(wildcard *.m) $(wildcard ../src/*.m)

.PHONY: realclean
realclean:
	$(MMC) --make pretty.realclean
	/bin/rm -rf Mercury
	/bin/rm -f Mercury.modules
