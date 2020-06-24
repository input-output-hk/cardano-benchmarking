
## preparations

needs the program `patchelf` (Debian: `apt install patchelf`)

first, build the project and copy binaries to ../bin directory

> `stack build --copy-bins`


## create AppImage using [appimagetool](https://github.com/AppImage/AppImageKit)

download appimatetool.AppImage from this URL

see the following script for instructions, whether FUSE is available, and them simply launch:

`./mkapp.sh`


## alternative creation using [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy)

prepare AppDir:

```
ARCH=x86_64 appimage-run $HOME/Downloads/linuxdeploy-x86_64.AppImage --appdir AppDir -e `pwd`/../bin/cardano-rt-view-service -i resources/cardano-rt-view.png  -d resources/cardano-rt-view.desktop  -e $(which patchelf) -e $(which mksquashfs) -e $(which strip)
```

