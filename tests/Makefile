MMC = mmc

# The expected outputs do not use CRLF line endings, so if we are on
# Windows we need to account for this.
# We assume the use of GNU diff if this is the case.
#
ifdef WINDIR
DIFF_FLAGS=--diff-flags "--strip-trailing-cr"
else
DIFF_FLAGS=
endif

ifdef GRADE
GRADEFLAGS=--grade $(GRADE)
else
GRADEFLAGS=
endif

.PHONY: runtests
runtests: test_json
	@./test_json $(DIFF_FLAGS)

.PHONY: runtests-verbose
runtests-verbose: test_json
	@./test_json $(DIFF_FLAGS) -v

Mercury.modules: $(wildcard *.m) $(wildcard ../src/*.m)
	$(MMC) -f $(wildcard *.m) $(wildcard ../src/*.m)

test_json: Mercury.modules
	$(MMC) $(GRADEFLAGS) --make $@

tags: $(wildcard *.m) $(wildcard ../src/*.m)
	mtags $(wildcard *.m) $(wildcard ../src/*.m)

# We're only interested in coverage information from the JSON library modules.
#
COVERAGE_RESTRICTION = \
	-m json 		\
	-m json.char_buffer 	\
	-m json.json_lexer 	\
	-m json.json_parser	\
	-m json.marshal         \
	-m json.pointer		\
	-m json.string_reader	\
	-m json.umarshal	\
	-m json.writer

.PHONY: coverage
coverage: test_json
	mtc --coverage-test -o test_json.trace_counts ./test_json
	mcov $(COVERAGE_RESTRICTION) test_json.trace_counts -o test_json.coverage-procs
	mcov $(COVERAGE_RESTRICTION) test_json.trace_counts -d -o test_json.coverage-labels

.PHONY: realclean
realclean:
	$(MMC) --make test_json.realclean
	/bin/rm -rf Mercury
	/bin/rm -f Mercury.modules tags
	/bin/rm -f FAILED_TESTS ABORTED_TESTS
	/bin/rm -f test_json.trace_counts test_json.coverage-procs
	/bin/rm -f test_json.coverage-labels
