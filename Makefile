.PHONY: all test

all:
	stack build $(STACK_FLAGS)

test:
	stack test $(STACK_FLAGS)
