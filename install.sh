#/bin/bash
sudo apt-get install freeglut3 freeglut3-dev libglfw2 libglfw-dev
sudo apt-get install libassimp-dev 
sudo apt-get install libdevil-dev
sudo apt-get install alex

cabal update
git clone https://github.com/ony/yampa-glut.git
cd yampa-glut
cabal install -j
cabal install c2hs -j

git clone https://github.com/joelburget/assimp.git
cd assimp
cabal install -j

cabal install GLUT -j
cabal install OpenGL -j
cabal install Codec-Image-DevIL -j
cabal install MissingH -j
