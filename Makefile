target=sex
swidev=swipl
swicc=swipl

$(target): *.pl
	$(swicc) -O --stand_alone=true -o $(target) -g main -c sex

run: $(target)
	./$(target)

test:
	$(swidev) -t run_tests -s sex

repl:
	$(swidev) -g repl -s sex
