my macos dot files.
===

Configures `bash`, `tmux`, `emacs`, and `git`.

Installs `homebrew`. Uses ```brew cask``` to install some browsers, some
unix-y tools (mostly gnu), `slack`, `spotify`, `X11`, `emacs`, and `docker`.

Does not install any programming languages. I use docker for that. (https://github.com/massemanet/dockerfiles)

The easiest way to install this is to copy your ssh keys;

```scp -r some-syst.em:~/.ssh ~/.ssh```

and pipe the install script to bash. yolo.

```curl https://raw.githubusercontent.com/massemanet/dotfiles.mac/master/INSTALL | bash```
