#!/usr/bin/env bash

set -o errexit

readonly packages=(
  i3
  firefox-esr

  dbus-x11   # required by terminator
  terminator

  git

  vim
  emacs25

  pulseaudio
  pavucontrol

  fonts-font-awesome # Used in i3bar config
)

apt update
apt install -y "${packages[@]}"
