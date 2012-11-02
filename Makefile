target=sex
swidev=swipl
swicc=swipl

$(target): *.pl
	time $(swicc) -q -O --stand_alone=true -o $(target) -g main -c sex

verbose:
	$(swicc) -O --stand_alone=true -o $(target) -g main -c sex

run: $(target)
	./$(target)

test:
	$(swidev) -t run_tests -s sex

repl:
	$(swidev) -g repl -s sex

install-sublime-plugin: $(target)
	@echo "installing sex plugin for Sublime2"
	@rm -f "${HOME}/Library/Application Support/Sublime Text 2/Packages/User/sex.pyc"
	@cp $(target) sex.py "${HOME}/Library/Application Support/Sublime Text 2/Packages/User/"

install-plugin:	install-sublime-plugin

install:	install-plugin

forever:
	@curl https://raw.github.com/herenowcoder/forever/master/forever -o forever
	@chmod a+x forever
