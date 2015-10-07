SRC := $(wildcard src/*.c)
DEPS := $(SRC:src/%.c=build/%.d)
OBJ := $(SRC:src/%.c=build/%.o)
CC := gcc
CFLAGS := -Wall -pedantic -Werror -std=c11

.PHONY: all run clean

all: build/lantis

run: build/lantis
	./build/lantis

build/lantis: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) $(LOADLIBES) $(LDLIBS) -o $@

build/%.o: src/%.c
	mkdir -p build
	$(CC) -MM -MG -MT $@ $< | awk '{print; for(i=2;i<=NF;i++){print $$i ":"}}' > $(@:.o=.d)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

-include $(DEPS)

clean:
	find build -depth -mindepth 1 -delete

