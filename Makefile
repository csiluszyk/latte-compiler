all:
	stack install

testclean:
	find test/tests -regex '.*.\(ll\|bc\)' -type f -delete

clean:
	stack clean
	-rm -rf latte
