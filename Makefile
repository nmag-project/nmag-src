.PHONY: all clean

all:
	(cd src && $(MAKE) all)
	(cd bin && $(MAKE) all)

clean:
	(cd src && $(MAKE) clean)
	(cd bin && $(MAKE) clean)
