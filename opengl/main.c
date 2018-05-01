#define GLEW_STATIC

#include <stdio.h>
#include <GL/glew.h>
#include <GLFW/glfw3.h>

struct pix {
    int type;
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

void display_rect(int x, int y, int width, int height, int rgb[]) {
    float r = (1/255.0)*rgb[0];
    float g = (1/255.0)*rgb[1];
    float b = (1/255.0)*rgb[2];

    glColor3f(r, g, b);

    glBegin(GL_POLYGON);
    glVertex2f(x, y);
    glVertex2f(x, y+height);
    glVertex2f(x+width, y+height);
    glVertex2f(x+width, y);
    glEnd();
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
        return -1;
    }
    
    glViewport( 0, 0, screenWidth, screenHeight );
    glOrtho(0.0f,width, 0.0f, height, -1.0f, 1.0f);

    double spf = 1.0/fps;
    double lastDrawTime = glfwGetTime();

    for(int i=0; i<numFrames; i++) {
        if(glfwWindowShouldClose(window)) break;
        
        glClearColor( 1.0f, 1.0f, 1.0f, 1.0f );
        glClear( GL_COLOR_BUFFER_BIT );
        
        placement_node *node = frames[i]->head;

        while(node->placed) {
            if(node->placed->ref->type == 1)
                display_rect(node->placed->x, node->placed->y, node->placed->ref->width,
                             node->placed->ref->height, node->placed->ref->rgb);
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
