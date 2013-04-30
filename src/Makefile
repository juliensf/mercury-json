include ../Make.options

.PHONY: default
default:
	$(MMC) --make libmercury_json

.PHONY: rebuild
rebuild:
	$(MMC) --rebuild libmercury_json

.PHONY: install
install:
	$(MMC) --make libmercury_json.install

tags: $(wildcard *.m)
	mtags $^

.PHONY: clean
clean:
	$(MMC) --make mercury_json.clean

.PHONY: realclean
realclean:
	$(MMC) --make mercury_json.realclean
	/bin/rm -rf Mercury
	/bin/rm -f $(wildcard *.err) $(wildcard *.mh)
	/bin/rm -f tags
