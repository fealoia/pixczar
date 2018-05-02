CC = gcc
CFLAGS = -c -g -Wall -Wextra
LDFLAGS = -framework GLUT -framework OpenGL -framework CoreFoundation -lsoil -lGLEW -lglfw

all: main.o

main.o: main.c
	$(CC) -c -o $@ main.c $(CFLAGS)

clean:
	rm main.o

new: clean all
