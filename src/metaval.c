#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

typedef struct {
  uint8_t* text;
  int length;
} Symbol;

typedef struct {
  int car;
  int cdr;
} Cons;

Cons initCons(int car, int cdr) {
  Cons cons;
  cons.car = car;
  cons.cdr = cdr;
  return cons;
}

Symbol initSymbol(uint8_t* text, int ln) {
  Symbol sym;
  sym.text = text;
  sym.length = ln;
  return sym;
}

static char* readFile(const char* path) {
  FILE* file = fopen(path, "rb");
  if (file == NULL) {
    fprintf(stderr, "Could not open file \"%s\".\n", path);
    exit(74);
  }

  fseek(file, 0L, SEEK_END);
  size_t fileSize = ftell(file);
  rewind(file);

  char* buffer = (char*)malloc(fileSize + 1);
  if (buffer == NULL) {
    fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
    exit(74);
  }

  size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
  if (bytesRead < fileSize) {
    fprintf(stderr, "Could not read file \"%s\".\n", path);
    exit(74);
  }

  buffer[bytesRead] = '\0';

  fclose(file);
  return buffer;
}

static void runFile(const char* path) {
  char* source = readFile(path);
  //   InterpretResult result = interpret(source);
  free(source);

  //   if (result == INTERPRET_COMPILE_ERROR) exit(65);
  //   if (result == INTERPRET_RUNTIME_ERROR) exit(70);
}

int main(int argc, char **argv)
{
  initVM();

  if (argc == 1) {
    repl();
  } else if (argc == 2) {
    runFile(argv[1]);
  } else {
    fprintf(stderr, "Usage: metaval [path]\n");
    exit(64);
  }

  freeVM();
  return 0;
}
