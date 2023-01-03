.PHONY: all clean
all:
	stack build --allow-different-user 
	cp $(shell stack path --local-install-root --allow-different-user)/bin/bmath bmath

clean:
	stack clean

