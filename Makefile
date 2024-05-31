
PHONY: clean build
build: 
	gcc -o bin/alaska src/main.c -lpthread -Wall -Wextra -Werror

PHONY: clean
clean:
	rm -f bin/alaska

PHONY: run
run: build	
	./bin/alaska