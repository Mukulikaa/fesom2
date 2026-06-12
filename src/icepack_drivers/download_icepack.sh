# Download icepack columphysics code
# from CICE-Consortium on github and
# switch to the appropriate branch

DIR="./Icepack"
if [ ! -d "$DIR" ]; then
    git clone https://github.com/CICE-Consortium/Icepack.git
    cd $DIR
    git checkout Icepack1.5.3
fi
