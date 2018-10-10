STACK=stack

all:
	$(STACK) setup
	$(STACK) install
	cp latc_llvm latc

clean: testclean
	$(STACK) clean
	-rm -rf latc_llvm

testclean:
	find test/tests -regex '.*.\(ll\|bc\)' -type f -delete

test: testclean all
	$(STACK) test

.PHONY: test testclean clean
