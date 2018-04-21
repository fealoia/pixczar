CC = gcc
CFLAGS = -c -g -Wall -Wextra
LDFLAGS = -framework GLUT -framework OpenGL -lGLEW -lglfw

all: main.o

main.o:
	$(CC) -c -o $@ main.c $(CFLAGS)

clean:
	rm main.o
