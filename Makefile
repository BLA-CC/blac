CC = gcc
CFLAGS = -Wall -Wextra -std=c17
LDFLAGS =

SOURCE_DIR = src
INCLUDE_DIR = include

CFLAGS += -I$(INCLUDE_DIR)

PARSER_H = $(INCLUDE_DIR)/parser.h
LEXER_H = $(INCLUDE_DIR)/lexer.h
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

$(SOURCE_DIR)/main.o: $(LEXER)

$(LEXER): $(SOURCE_DIR)/lexer.l
	flex $<

$(PARSER): $(SOURCE_DIR)/parser.y $(LEXER)
	bison $<

clean:
	rm -f $(TARGET) $(PARSER) $(PARSER_H) $(LEXER) $(LEXER_H) $(OBJECT)

.PHONY: all clean

