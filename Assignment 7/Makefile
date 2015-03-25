all: nbody

SEQ=sequence.mli sequence.ml

SOURCES=$(SEQ) \
	plane.mli plane.ml \
	common_nbody.ml \
        naive_nbody.ml \
        barnes_hut_nbody.ml \
	test_nbody.ml \
	main_nbody.ml

LOGGING_SOURCES=order.ml dict.ml myset.ml \
	logger.mli logger.ml \
	par.mli par.ml \
	arr.mli arr.ml

APPS=$(SEQ) \
	apps/util.mli apps/util.ml \
	apps/inverted_index.mli apps/inverted_index.ml \
	apps/apm.mli apps/apm.ml \
	apps/main_apps.ml

nbody: $(LOGGING_SOURCES) $(SOURCES)
	ocamlc -o nbody unix.cma \
	$(LOGGING_SOURCES) $(SOURCES)

seqapps: $(LOGGING_SOURCES) $(APPS)
	ocamlc -o seqapps -I apps/ unix.cma str.cma \
	$(LOGGING_SOURCES) $(APPS)

sequence: $(LOGGING_SOURCES) $(SEQ)
	ocamlc -o sequence -I apps/ unix.cma str.cma \
	$(LOGGING_SOURCES) $(SEQ)

sequence_test: $(LOGGING_SOURCES) $(SEQ) sequence_test.ml
	ocamlc -o sequence_test unix.cma str.cma \
	$(LOGGING_SOURCES) $(SEQ) sequence_test.ml

clean:
	rm -fr *.cmo *.cmi *.class nbody nbody.exe \
	sequence seqapps sequence_test