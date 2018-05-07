#define GLEW_STATIC

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include "soil.h"

struct pix {
    int type;
    char *filename;
    int width;
    int height;
    int *rgb;
} typedef pix;

struct placement {
    pix *ref;
    int x;
    int y;
    int rank;
    int group;
} typedef placement;

struct placement_node {
    struct placement_node *next;
    struct placement *placed;
} typedef placement_node;

struct frame {
    struct placement_node *head;
} typedef frame;

void color(int rgb[]) {
    float r = (1/255.0)*rgb[1];
    float g = (1/255.0)*rgb[2];
    float b = (1/255.0)*rgb[3];

    glColor3f(r, g, b);
}

void display_rect(int x, int y, int width, int height, int rgb[]) {
    color(rgb);

    glBegin(GL_POLYGON);
    glVertex2f(x, y);
    glVertex2f(x, y+height);
    glVertex2f(x+width, y+height);
    glVertex2f(x+width, y);
    glEnd();
}

void display_triangle(int x, int y, int length, int rgb[]) {
    color(rgb);

    glBegin(GL_TRIANGLES);
    glVertex2f(x, y);
    glVertex2f(x + length, y);
    glVertex2f(x + length/2, y + length/2);
    glEnd();
}

void display_ellipse(int x, int y, int width, int height, int rgb[]) {
    color(rgb);

    const float Pi2=2*3.141593;
    int centerX = x + width/2;
    int centerY = y + height/2;
    int i;

    glBegin(GL_TRIANGLE_FAN);
    glVertex2f(centerX, centerY);
    for(i=0; i <= 25; i++) {
        glVertex2f(
            centerX + ((width/2) * cos(i * Pi2/ 25)),
            centerY + ((height/2) * sin(i * Pi2/ 25))
        );
    }
    glEnd();
}

void display_image(int x, int y, int width, int height, char *filename) {
    glColor3f(1.0, 1.0, 1.0);
    glEnable (GL_BLEND);
    glBlendFunc (GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_TEXTURE_2D);
    int img_width, img_height;

    char absolutepath[1000];
    if(realpath(filename,absolutepath) == NULL) {
        printf("Invalid image: %s\n", filename);
        return;
    }

    unsigned char* image =
    SOIL_load_image(absolutepath, &img_width, &img_height, 0, SOIL_LOAD_RGBA);

    if(image == NULL) {
        printf("Invalid image: %s\n", filename);
        return;
    }

    GLuint gltext;
    glGenTextures(1,&gltext);
    glBindTexture(GL_TEXTURE_2D, gltext);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, img_width, img_height, 0, GL_RGBA,
                 GL_UNSIGNED_BYTE, image);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glBegin(GL_QUADS);
    glTexCoord2i(0, 0); glVertex2i(x, y+height);
    glTexCoord2i(0, 1); glVertex2i(x, y);
    glTexCoord2i(1, 1); glVertex2i(x+width, y);
    glTexCoord2i(1, 0); glVertex2i(x+width, y+height);

    glEnd();
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);

    SOIL_free_image_data(image);
}

int render(int numFrames, frame *frames[], int fps, int width, int height) {
    glfwInit();

    glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);

    GLFWwindow *window = glfwCreateWindow(width, height, "PixCzar", NULL, NULL);

    int screenWidth, screenHeight;
    glfwGetFramebufferSize( window, &screenWidth, &screenHeight);

    if( window == NULL ) {
        printf("Failed to create GLFW window\n");
        glfwTerminate();
        return -1;
    }

    glfwMakeContextCurrent( window );
    glewExperimental = GL_TRUE;

    if( GLEW_OK != glewInit() ) {
        printf("Failed to initialize GLEW\n");
        glfwTerminate();
    }

    glViewport( 0, 0, screenWidth, screenHeight );
    glOrtho(0.0f,width, 0.0f, height, -1.0f, 1.0f);

    double spf = 1.0/fps;
    double lastDrawTime = glfwGetTime();

    int i;
    for(i=1; i<numFrames; i++) {
        if(glfwWindowShouldClose(window)) break;

        glClearColor( 1.0f, 1.0f, 1.0f, 1.0f );
        glClear( GL_COLOR_BUFFER_BIT );

        placement_node *node = frames[i]->head;
        while(node->placed) {
            if(node->placed->ref->type == 1) {
                display_rect(node->placed->x, node->placed->y, node->placed->ref->width,
                             node->placed->ref->height, node->placed->ref->rgb);
            } else if(node->placed->ref->type == 2) {
                display_triangle(node->placed->x, node->placed->y, node->placed->ref->height,
                                 node->placed->ref->rgb);
            } else if(node->placed->ref->type == 3) {
                display_ellipse(node->placed->x, node->placed->y, node->placed->ref->width,
                             node->placed->ref->height, node->placed->ref->rgb);
            } else if(node->placed->ref->type == 4) {
                display_image(node->placed->x, node->placed->y, node->placed->ref->width,
                              node->placed->ref->height, node->placed->ref->filename);
            }

            node = node->next;
        }
        glfwSwapBuffers( window );

        lastDrawTime = glfwGetTime();
        glfwPollEvents();

        while(glfwGetTime() - lastDrawTime < spf);
    }

    glfwTerminate();
    return 0;
}
