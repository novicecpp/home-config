#!/bin/bash
set -euo pipefail
VERSION_CHECKOUT=45cdc89986
rm -rf emacs
git clone git://git.sv.gnu.org/emacs.git
cd emacs
git checkout $VERSION_CHECKOUT
./autogen.sh
./configure --prefix=/usr --exec-prefix=/usr --bindir=/usr/bin --sbindir=/usr/sbin --sysconfdir=/etc --datadir=/usr/share --includedir=/usr/include --libdir=/usr/lib64 --libexecdir=/usr/libexec --localstatedir=/var --sharedstatedir=/var/lib --mandir=/usr/share/man --infodir=/usr/share/info --with-dbus --with-gif --with-jpeg --with-png --with-rsvg --with-tiff --with-xft --with-xpm --with-x-toolkit=gtk3 --with-gpm=no --with-xwidgets --with-modules --with-harfbuzz --with-cairo --with-json --with-mailutils --with-native-compilation
make -j $(cat /proc/cpuinfo | awk '/^processor/{print $3}' | wc -l)
sudo make install
