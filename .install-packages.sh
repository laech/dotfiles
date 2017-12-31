#!/usr/bin/env bash

set -o errexit
set -o nounset

readonly packages=(
  i3
  firefox-esr

  # Required by terminator
  dbus-x11
  terminator

  git

  vim
  emacs25

  pulseaudio
  pavucontrol

  # Used in i3bar config
  fonts-font-awesome

  # Use in default fonts
  fonts-noto

  openjdk-8-jdk
  openjdk-8-source

  compton
  numlockx
)

apt update
apt install -y "${packages[@]}"
