#!/usr/bin/env bash

set -o errexit
set -o nounset

readonly packages=(

  j4-dmenu-desktop
  i3
  xinit
  x11-xserver-utils

  firefox-esr

  xterm
  xsel
  zsh
  zsh-syntax-highlighting

  git

  vim
  emacs25

  pulseaudio
  pavucontrol

  fonts-noto

  openjdk-8-jdk
  openjdk-8-source

  compton
  numlockx

  fcitx
  fcitx-libpinyin
  dbus-x11

  wpasupplicant

  openssh-server
  openssh-client
)

apt update
apt install "${packages[@]}"
