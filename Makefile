STACK=stack
# STACK=/home/students/inf/PUBLIC/MRJP/Stack/stack

all:
	$(STACK) setup
	$(STACK) install

test:
	$(STACK) test

testclean:
	find test/tests -regex '.*.\(ll\|bc\)' -type f -delete

clean:
	$(STACK) clean
	-rm -rf latc_llvm

.PHONY: test testclean clean