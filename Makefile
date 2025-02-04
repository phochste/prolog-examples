.PHONY: clean

RULES = $(wildcard *.rls)
RULES_OUT = $(patsubst %.rls,%.ttl,$(RULES))

all: $(RULES_OUT) example5.trig

%.ttl : %.rls
	nmo --overwrite-results -D . $*.rls

example5.trig : example5.nq
	riot --syntax=nquads --out=trig example5.nq > example5.trig

clean:
	rm *.ttl *.nq *.trig