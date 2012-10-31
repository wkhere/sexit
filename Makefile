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

install-sublime-plugin:
	@echo "installing sex plugin for Sublime2"
	@cp -a sex.py "${HOME}/Library/Application Support/Sublime Text 2/Packages/User/"
	@rm -f "${HOME}/Library/Application Support/Sublime Text 2/Packages/User/sex.pyc"

install-plugin:	install-sublime-plugin

install:	install-plugin
