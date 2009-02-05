help: 
	@echo "run 'make local' to test against working directory"
	@echo "run 'make installed' to test against installed libraries (installed libraries must be configured with -f tests)"

local:
	runhaskell -i../src Test.hs

installed:
	runhaskell Test.hs

.PHONY: help local installed
