#!/usr/bin/env bash


if ! [ "$(git branch --show-current)" = "feature/native-comp" ]; then
    git checkout --track origin/feature/native-comp
fi

cflags () {
    export CFLAGS="-$1$2 $CFLAGS"
}
cflags I /usr/local/opt/gcc/include
cflags I /usr/local/opt/libgccjit/include 
cflags I /usr/local/opt/gmp/include 
cflags I /usr/local/opt/jpeg/include

ldflags () {
    export LDFLAGS="-$1$2 $LDFLAGS"
}

ldflags L /usr/local/lib/gcc/10 
ldflags I /usr/local/opt/gcc/include 
ldflags I /usr/local/opt/libgccjit/include 
ldflags I /usr/local/opt/gmp/include 
ldflags I /usr/local/opt/jpeg/include

./autogen.sh
./configure --with-nativecomp \
    --with-imagemagick \
    --with-modules \
    --with-rsvg \
    --with-xwidgets \
    --with-ns \
    --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS" \
    --enable-locallisppath="${PWD}/nextstep/Emacs.app/Contents/MacOS"
make --jobs=16 NATIVE_FAST_BOOT=1
cp -r lisp nextstep/Emacs.app/Contents/Resources/
cp -r native-lisp nextstep/Emacs.app/Contents
make install --jobs=16
