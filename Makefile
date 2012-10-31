target=sext
swidev=swipl
swicc=swipl

$(target): *.pl
	$(swicc) -O --stand_alone=true -o $(target) -g main -c sext

run: $(target)
	./$(target)

test:
	$(swidev) -t run_tests -s sext

repl:
	$(swidev) -g repl -s sext
