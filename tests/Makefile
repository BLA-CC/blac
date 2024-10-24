CXX = g++
CXXFLAGS = `pkg-config --cflags gtest` -I../include -I../src
LDFLAGS += `pkg-config --libs gtest`

CFLAGS += -I../include -I../src

SOURCE_DIR := $(addprefix ../, $(SOURCE_DIR))
SOURCE := $(addprefix ../, $(SOURCE))
OBJECT := $(addprefix ../, $(OBJECT))
PARSER := $(addprefix ../, $(PARSER))
LEXER := $(addprefix ../, $(LEXER))

PARSER_O = $(PARSER:.c=.o)
LEXER_O = $(LEXER:.c=.o)

BLAC_MAIN = $(SOURCE_DIR)/main.o
OBJECT := $(filter-out $(BLAC_MAIN), $(OBJECT))

TEST_SOURCE_DIR = .
TEST_SOURCE = $(wildcard $(TEST_SOURCE_DIR)/*.cpp)
TEST_OBJECT = $(TEST_SOURCE:.cpp=.o)

TARGET = test

$(TARGET): $(OBJECT) $(TEST_OBJECT) $(PARSER_O) $(LEXER_O)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $(TARGET)

$(PARSER_O): $(PARSER)
	$(CC) $(CFLAGS) $(LDFLAGS) -c $< -o $@

$(LEXER_O): $(LEXER)
	$(CC) $(CFLAGS) $(LDFLAGS) -c $< -o $@

run-tests: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TEST_OBJECT) $(TARGET) $(PARSER_O) $(LEXER_O)

