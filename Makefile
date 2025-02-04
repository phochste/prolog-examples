.PHONY: clean

SRC = $(wildcard *.rls)
OUT = $(patsubst %.rls,%.ttl,$(SRC))


all: $(OUT)

%.ttl : %.rls
	nmo --overwrite-results -D . $*.rls
	
clean:
	rm *.ttl