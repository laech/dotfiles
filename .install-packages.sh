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

  xterm
  xsel
  tmux
  zsh
  zsh-syntax-highlighting

  git

  vim
  emacs

  pulseaudio
  pavucontrol

  ttf-dejavu
  noto-fonts-cjk

  jdk8-openjdk
  openjdk8-src

  compton
  numlockx

  fcitx
  fcitx-gtk3
  fcitx-libpinyin
  fcitx-configtool

  openssh

  # For encrypting wireless/subversion password, ssh forward agent etc
  # https://wiki.archlinux.org/index.php/GNOME/Keyring
  # https://wiki.archlinux.org/index.php/GNOME/Keyring#PAM_method
  # https://wiki.archlinux.org/index.php/NetworkManager#Encrypted_Wi-Fi_passwords
  gnome-keyring
  libgnome-keyring
  libsecret
  seahorse

  # https://wiki.archlinux.org/index.php/NetworkManager#Encrypted_Wi-Fi_passwords
  networkmanager
  network-manager-applet
)

pacman -S --needed "${packages[@]}"
