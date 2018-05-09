PixCzar: An Animation Language

Frank Aloia - fea2113@columbia.edu
Gary Chen - gc2676@columbia.edu
Bryan Li - bl2557@columbia.edu
Matias Lirman - ml3707@columbia.edu

Commands for compilation/executation instructions below.
For detailed descriptions, please see the report.

________________________________________________________________
Installing Dependencies: (OpenGL and SOIL)
  Ubuntu:
    sudo apt-get install cmake libx11-dev xorg-dev 
    libglu1-mesa-dev freeglut3-dev libglew1.5 libglew1.5-dev 
    libglu1-mesa libglu1-mesa-dev libgl1-mesa-glx 
    libgl1-mesa-dev libsoil-dev

  MacOS:
    brew install glew;
    brew install glfw;
    git clone https://github.com/kbranigan/Simple-OpenGL-Image- Library.git SOIL;
    cd SOIL;
    make;
    make install;
________________________________________________________________

How to Compile: ./make.sh

*** make.sh exports specific path of llvm found in the group's computer. 
    THIS MUST BE THE PATH FOR LLVM COMMANDS: /usr/local/opt/llvm/bin
    
    If llvm is located in another directory then the function will not build. To fix problem
    please change the path to the users appropriate llvm directory ***

*** make.sh uses "-ocamlfind" and "lli" to compile source code so it is assumed
    that these compiling tools are already downloaded on user's computer ***
________________________________________________________________

How to Execute the test suite: ./testall.sh


  testall.sh script should do the following:
  1. run ./make.sh
  2. Compile the files and produce an executable
  3. Test the executable against the tests within the tests directory and return
  information on the success of the tests
    a.  - Green text indicates expected output for a test
        - Red text indicates unexpected output for a test
        -Ex: "./pixczar.native shouldfail.pxr" should print FAILURE in green
________________________________________________________________

How to Execute individual .pxr files: ./compile.sh <file.pxr> run
  
  This command will do the following:
  1. run ./make.sh
  2. Replaces #include lines if they exist, creating a new file
  3. Compiles .pxr to .ll using pixczar.native
  4. Compiles .ll to .s using llc
  5. Compiles .s to .exe using clang, linking the OpenGl, Soil, and opengl/main.o libraries
  6. runs the executable

  Assumed dependencies: llvm, ocamlbuild, llc, clang
  Common errors: llc path not in $PATH variable 
________________________________________________________________

How to Include files:

***Only if not using compile.sh***

include.sh is the implementation of "#include", as suggested by one of Richard's Piazza posts.

How to run:
command: source include.sh && generate_includes input.pxr
returns: input_included.pxr

With this new .pxr file, you can proceed with compilation. Note that this means that
the pixczar compiler will never see the #include.
