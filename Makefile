CC = gcc
CFLAGS = -Wall -Wextra -std=c17
LDFLAGS =

SOURCE_DIR = src
INCLUDE_DIR = include

CFLAGS += -I$(INCLUDE_DIR)

PARSER_H = $(INCLUDE_DIR)/_parser.h
LEXER_H = $(INCLUDE_DIR)/_lexer.h
PARSER = $(SOURCE_DIR)/parser.c
LEXER = $(SOURCE_DIR)/lexer.c

SOURCE = $(wildcard $(SOURCE_DIR)/*.c)
SOURCE := $(filter-out $(PARSER), $(SOURCE))
SOURCE := $(filter-out $(LEXER), $(SOURCE))
OBJECT = $(SOURCE:.c=.o)

TARGET = blac

all: $(TARGET)

$(TARGET): $(LEXER) $(PARSER) $(OBJECT)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@

$(SOURCE_DIR)/main.o: $(LEXER) $(PARSER)

$(LEXER): $(SOURCE_DIR)/lexer.l
	flex $<

$(PARSER): $(SOURCE_DIR)/parser.y
	bison $<

test: $(LEXER) $(PARSER) $(OBJECT)
	$(MAKE) -C tests run-tests

clean:
	rm -f $(TARGET) $(PARSER) $(PARSER_H) $(LEXER) $(LEXER_H) $(OBJECT)
	$(MAKE) -C tests clean

.PHONY: all clean

export CC CFLAGS LDFLAGS SOURCE_DIR SOURCE OBJECT PARSER LEXER

