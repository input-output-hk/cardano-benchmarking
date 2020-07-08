## Preparations

Needs the program `patchelf` (Debian: `apt install patchelf`)

First, build the project and copy binaries to `../bin` directory. If you use `stack`:

```
$ stack build --copy-bins
```

If you use `cabal`:

```
$ cabal install all --install-method=copy --installdir=bin/
```

### NixOS Preparations

If you use NixOS, please make sure you have `file` command, if not - install it:

```
$ nix-env -iA nixos.file
```

To be able to run `AppImage`s, you need `appimage-run` program as well, install it:

```
$ nix-env -iA nixos.appimage-run
```

## NixOS: Static Compilation

By default, `cardano-rt-view-service` executable is built with dynamic dependencies
(you can check it using `ldd` command). To simplify distribution of `AppImage`, it is
highly recommended to compile `cardano-rt-view-service` with static linking. In this case
all dependencies will be a part of executable, so `ldd` command returns nothing.

Use this command:

```
$ cd path/to/cardano-benchmarking/
$ nix build -L -f static.nix cardano-rt-view.components.exes.cardano-rt-view-service
```

Then copy executable in `bin` directory:

```
$ cp result/bin/cardano-rt-view-service bin/
```

## Recommended: Create AppImage using [appimagetool](https://github.com/AppImage/AppImageKit)

First, download `appimatetool` from this [AppImageKit/Releases](https://github.com/AppImage/AppImageKit/releases).
For example, `appimagetool-x86_64.AppImage` file.

Then make it executable:

```
$ cd ~/Downloads
$ chmod a+x appimagetool-x86_64.AppImage
```

See the following script for instructions, whether FUSE is available, and them simply launch:

```
$ cd path/to/cardano-benchmarking/cardano-rt-view
$ ./mkapp.sh
```

As a result, you'll see a directory like `appdir-2020-07-01T16_01_43`. Corresponding `*AppImage` was built and placed
in `/tmp` directory, for example:

```
$ ls -al /tmp/Cardano_RTview-x86_64.AppImage
-rwxr-xr-x 1 denis users 7356392 Jul  1 21:13 /tmp/Cardano_RTview-x86_64.AppImage
```

This `*AppImage` is ready for use.

## Alternative creation using [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy)

Prepare AppDir:

```
ARCH=x86_64 appimage-run $HOME/Downloads/linuxdeploy-x86_64.AppImage --appdir AppDir -e `pwd`/../bin/cardano-rt-view-service -i resources/cardano-rt-view.png  -d resources/cardano-rt-view.desktop  -e $(which patchelf) -e $(which mksquashfs) -e $(which strip)
```

