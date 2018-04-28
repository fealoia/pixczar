#define GLEW_STATIC

#include <stdio.h>
#include <GL/glew.h>
#include <GLFW/glfw3.h>

const GLint WIDTH = 800, HEIGHT = 600;

struct frame {
    int width;
    int height;
} typedef frame;

int render(frame *frames[], int fps) {
    glfwInit();
    
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
    
    char buff[10];
    sprintf(buff, "%d",frames[1]->width);
    
    GLFWwindow *window = glfwCreateWindow(WIDTH, HEIGHT, buff, NULL, NULL);
    
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
        return -1;
    }
    
    glViewport( 0, 0, screenWidth, screenHeight );
    
    while( !glfwWindowShouldClose( window ) ) {
        glfwPollEvents();
        glClearColor( 0.2f, 0.3f, 0.3, 1.0f );
        glClear( GL_COLOR_BUFFER_BIT );
        glfwSwapBuffers( window );
    }
    
    glfwTerminate();
    
    return 0;
}
