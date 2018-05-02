CC = gcc -std=c99
CFLAGS = -c -g -Wall -Wextra
LDFLAGS = -framework GLUT -framework OpenGL -lGLEW -lglfw

# COMPILER_FLAGS specifies the additional compilation options we're using
# -Wall will turn on all standard warnings
COMPILER_FLAGS = -Wall

# LINKER_FLAGS specifies the libraries we're linking against
LINKER_FLAGS = -lGL -lglut -lGLEW -lglfw
all: main.o

main.o: main.c
	$(CC) -c -o $@ main.c $(COMPILER_FLAGS) $(LINKER_FLAGS)
	#$(CC) -c -o $@ main.c $(CFLAGS)

clean:
	rm main.o

new: clean all
