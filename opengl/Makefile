CC = gcc -std=c99
CFLAGS = -c -g -Wall -Wextra

# LINKER_FLAGS specifies the libraries we're linking against
LINKER_FLAGS = -lGL -lglut -lGLEW -lglfw
LINKER_FLAGS = -framework GLUT -framework OpenGL -framework CoreFoundation -lGLEW -lglfw -lsoil

# COMPILER_FLAGS specifies the additional compilation options we're using
# -Wall will turn on all standard warnings
COMPILER_FLAGS = -Wall

all: main.o

main.o: main.c
	$(CC) -c -o $@ main.c $(COMPILER_FLAGS)

clean:
	rm main.o
