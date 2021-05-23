# Emacs configuration
This is my Emacs configuration that I continuously use and improve to meet my programming and text editing needs.

![Screenshot](https://github.com/imryche/emacs.d/raw/master/images/screenshot.png)

# Summary
My goal is to keep this configuration as minimal as possible while providing a fast editing experience and extensive support for the programming languages I use.

# Installation

## Install Emacs
It runs smoothly on Emacs 27.2.
```bash
$ brew tap d12frosted/emacs-plus
$ brew install emacs-plus@27 --with-elrumo2-icon
```

## Install dependencies
Ripgrep for blazingly fast recursive search in a directory:
```bash
$ brew install ripgrep
```

Coreutils for GNU like utilities. This config uses `gls` with `--group-directories-first` as directory program for `dired`:
```bash
$ brew install coreutils
```

## Install configuration
Clone this repo to `~/.emacs.d` directory. Launch Emacs and wait for several minutes:
```bash
git clone https://github.com/imryche/emacs.d ~/.emacs.d
```

## Prepare keyboard
Configure your keyboard so your `Caps Lock` is `Right Ctrl`. Go to System Preferences -> Keyboard -> Modifier Keys.
