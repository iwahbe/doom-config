#!/usr/bin/env bash

NJOBS=16

cflags () {
    export CFLAGS="-$1$2 $CFLAGS"
}

ldflags () {
    export LDFLAGS="-$1$2 $LDFLAGS"
}

setup () {
    if ! [ "$(git branch --show-current)" = "feature/native-comp" ]; then
        git checkout --track origin/feature/native-comp
    fi

    cflags I /usr/local/opt/gcc/include
    cflags I /usr/local/opt/libgccjit/include 
    cflags I /usr/local/opt/gmp/include 
    cflags I /usr/local/opt/jpeg/include

    ldflags L /usr/local/lib/gcc/10 
    ldflags I /usr/local/opt/gcc/include 
    ldflags I /usr/local/opt/libgccjit/include 
    ldflags I /usr/local/opt/gmp/include 
    ldflags I /usr/local/opt/jpeg/include
}

build () {
    ./autogen.sh
    ./configure --with-nativecomp \
        --with-imagemagick \
        --with-modules \
        --with-rsvg \
        --with-xwidgets \
        --with-ns \
        --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS" \
        --enable-locallisppath="${PWD}/nextstep/Emacs.app/Contents/MacOS" \
        --with-harfbuzz \
        --with-json \

    make --jobs=$NJOBS NATIVE_FAST_BOOT=1
    cp -r lisp nextstep/Emacs.app/Contents/Resources/
    cp -r native-lisp nextstep/Emacs.app/Contents
    mkdir nextstep/Emacs.app/Contents/MacOS/libexec
    if [ -f ~/.doom.d/doom3.icns ]; then
         cp ~/.doom.d/doom3.icns "${PWD}/nextstep/Emacs.app/Contents/Resources/Emacs.icns"
    fi
    make install --jobs=$NJOBS
}

clone () {
    return git clone 'https://github.com/emacs-mirror/emacs.git' "$1" --jobs $NJOBS
}

install () {
    select loc in "Applications" "Done" "Doom" "Path"; do
        case $loc in
            Applications )
                if [ -d nextstep/Emacs.app ]; then
                    ln -s $(realpath nextstep/Emacs.app) /Applications/Emacs.app;
                    echo "Emacs.app installed in the Applications folder";
                elif [ -d  /Applications/Emacs.app ]; then
                    echo "Emacs.app was already installed in the Applications folder"
                else
                    echo "an error has occured."
                fi
                ;;
            Doom )
                if [ -f ~/.emacs.d/bin/doom ]; then
                    echo "Executing doom sync"
                    ~/.emacs.d/bin/doom sync
                elif ! [ -d ~/.emacs.d ]; then
                    echo "Executing doom install"
                    git clone 'https://github.com/hlissner/doom-emacs.git' ~/.emacs.d
                    ~/.emacs.d/bin/doom install
                else
                    echo "It looks like emacs exists, and it's not Doom"
                fi
                ;;
            Path )
                ln -s $(realpath nextstep/Emacs.app/Contents/MacOS/Emacs) ~/.local/bin/emacs
                ;;
            Done )
                echo "exit";
                break;
                ;;
        esac
    done
}

# The first argument is the repo source. It is either
# -C meaning clone the repo, or
# the name of an already existing emacs repo.
#
# The second argument is the name of the new emacs build folder.
# It defualts to emacs.


SOURCE="$1"
EMACS_BUILD_DIR="$2"
if [ "$EMACS_BUILD_DIR" = "" ]; then
    EMACS_BUILD_DIR="emacs"
fi

if [ -d "$EMACS_BUILD_DIR" ]; then
    echo "Delete old \"$EMACS_BUILD_DIR\" folder?"
    select yn in "Yes" "No"; do
        case $yn in
            Yes )
                echo -n "Deleting ..."
                rm -fr $EMACS_BUILD_DIR;
                echo " done"
                break;
                ;;
            No )
                echo "You chose not to delete..."
                exit;
                ;;
        esac
    done
fi

if [ "$SOURCE" = "-C" ]; then
    clone "$EMACS_BUILD_DIR" || exit 1
else
    cp -r "$SOURCE" "$EMACS_BUILD_DIR" || exit 1
fi

cd "$EMACS_BUILD_DIR"
setup || exit 2
build || exit 3
install || exit 4

