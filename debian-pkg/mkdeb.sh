#!/bin/sh
set -e

cabal build
BINARY_PATH=$(cabal exec -- which gopherdashboard-exe)
mkdir -p gopherdashboard-deb/DEBIAN
mkdir -p gopherdashboard-deb/usr/local/bin
cp debian-pkg/control gopherdashboard-deb/DEBIAN/control
cp "$BINARY_PATH" gopherdashboard-deb/usr/local/bin/gopherdashboard

mkdir -p gopherdashboard-deb/lib/systemd/system
mkdir -p gopherdashboard-deb/etc
cp etc/gopherdashboard.service gopherdashboard-deb/lib/systemd/system/
cp etc/gopherdashboard.json gopherdashboard-deb/etc/
cp debian-pkg/postinst gopherdashboard-deb/DEBIAN/postinst
chmod +x gopherdashboard-deb/DEBIAN/postinst
mkdir -p gopherdashboard-deb/var/gopherdashboard

cp debian-pkg/prerm gopherdashboard-deb/DEBIAN/prerm
chmod +x gopherdashboard-deb/DEBIAN/prerm

dpkg-deb --build gopherdashboard-deb
if [ -n "$1" ]; then
    mv gopherdashboard-deb.deb "gopherdashboard-$1-amd64.deb"
else
    mv gopherdashboard-deb.deb "gopherdashboard-amd64.deb"
fi

rm -rf gopherdashboard-deb
