#define GLEW_STATIC

#include <stdio.h>
#include <GL/glew.h>
#include <GLFW/glfw3.h>

const GLint WIDTH = 800, HEIGHT = 600;

struct pix {
    int temp;
} typedef pix;

struct placement {
    pix *ref;
    int x;
    int y;
    int rank;
    int group;
} typedef placement;

struct frame {
    placement *placed;
    int width;
    int height;
} typedef frame;

void display_square(int x, int y, int length) {
    glColor3f(1.0f,0.0f,0.0f);

    glBegin(GL_POLYGON);
    glVertex2f(x, y);
    glVertex2f(x, y+length);
    glVertex2f(x+length, y+length);
    glVertex2f(x+length, y);
    glEnd();
}

int render(frame *frames[], int fps, int width, int height) {
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
        return -1;
    }
    
    glViewport( 0, 0, screenWidth, screenHeight );
    glOrtho(0.0f,width, 0.0f, height, -1.0f, 1.0f);

    double spf = 1.0/fps;
    double lastDrawTime = glfwGetTime();
    
    while( !glfwWindowShouldClose( window ) ) {
        if (glfwGetTime() - lastDrawTime >= spf) {
            glClearColor( 1.0f, 1.0f, 1.0f, 1.0f );
            glClear( GL_COLOR_BUFFER_BIT );
            display_square(frames[1]->placed->x, frames[1]->placed->x,200);
            glfwSwapBuffers( window );
            
            lastDrawTime = glfwGetTime();
        }
        glfwPollEvents();
   }

   glfwTerminate();
   return 0;
}
