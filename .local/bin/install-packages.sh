#!/usr/bin/env bash

set -o errexit
set -o nounset

readonly packages=(

  dmenu
  i3-wm
  i3lock
  i3status
  xorg-server
  xorg-xinit

  firefox

  tmux
  xterm
  xsel
  zsh
  zsh-syntax-highlighting

  git

  vim
  emacs

  pulseaudio
  pavucontrol

  ttf-dejavu
  ttf-inconsolata
  noto-fonts-cjk

  jdk8-openjdk
  openjdk8-src

  compton
  numlockx

  fcitx
  fcitx-gtk3
  fcitx-libpinyin
  fcitx-configtool

  wpa_supplicant

  openssh
)

pacman -S --needed "${packages[@]}"

