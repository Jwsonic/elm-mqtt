PHONY: test test-watch

test:
	./node_modules/.bin/elm-test

test-watch:
	./node_modules/.bin/elm-test --watch