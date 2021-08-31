# Emacs configuration
This is my Emacs configuration that I continuously use and improve to
meet my programming and text editing needs.

![Screenshot](https://github.com/imryche/emacs.d/raw/master/images/screenshot.png)

# Summary
My goal is to keep this configuration as minimal as possible while
providing a fast editing experience and extensive support for the
programming languages I use.

# Installation

## Install Emacs
It runs smoothly on Emacs 27.2.
```bash
brew tap d12frosted/emacs-plus
brew install emacs-plus@27 --with-elrumo2-icon
```

## Install dependencies
Ripgrep for blazingly fast recursive search in a directory:
```bash
brew install ripgrep
```

Coreutils for GNU like utilities. This config uses `gls` with `--group-directories-first` as directory program for `dired`:
```bash
brew install coreutils
```

## Install configuration
Clone this repo to `~/.emacs.d` directory. Launch Emacs and wait for several minutes:
```bash
git clone git@github.com:imryche/emacs.d.git ~/.emacs.d
```

# Prepare keyboard

## Remap Caps Lock
Configure your keyboard, so your `Caps Lock` is `Left Ctrl`. Go to
System Preferences -> Keyboard -> Modifier Keys.

## Arrow keys on the home row
Arrow keys are essential for navigation in modern applications,
but their location on most keyboards looks like an afterthought that
forces you to move your palm back and forth frequently.

To make workflow more consistent, I think it's a good idea to make
`Ctrl+hjkl` combination act as arrow keys.

Karabiner-Elements can help with this task:
```bash
brew install --cask karabiner-elements
```

Search for `ctrl + hjkl`
[here](https://ke-complex-modifications.pqrs.org/?q=ctrl%20%2B%20hjkl) to install the
modification.
