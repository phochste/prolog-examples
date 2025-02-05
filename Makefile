.PHONY: clean

RULES = $(wildcard *.pl)
RULES_OUT = $(patsubst %.pl,%.ttl,$(RULES))

all: $(RULES_OUT) 

%.ttl : %.pl
	swipl -g run -t run $*.pl

clean:
	rm *.ttl *.trig