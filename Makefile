STACK=stack
# STACK=/home/students/inf/PUBLIC/MRJP/Stack/stack

all:
	$(STACK) setup
	$(STACK) install

clean: testclean
	$(STACK) clean
	-rm -rf latc_llvm

testclean:
	find test/tests -regex '.*.\(ll\|bc\)' -type f -delete

test: testclean all
	$(STACK) test --ta "--quiet -j4"

.PHONY: test testclean clean