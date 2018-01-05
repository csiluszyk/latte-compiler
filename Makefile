all:
	stack install

test:
	stack test

testclean:
	find test/tests -regex '.*.\(ll\|bc\)' -type f -delete

clean:
	stack clean
	-rm -rf latte

.PHONY: test testclean clean