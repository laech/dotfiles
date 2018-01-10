#!/usr/bin/env bash

set -o errexit
set -o nounset

readonly packages=(

  # Use j4-dmenu-desktop instead of the default dmenu_run in i3
  # as apps started j4-demnu-desktop can receive SIGINT, e.g.
  # without this IntelliJ won't be able to terminate running
  # process when stop button is clicked.
  j4-dmenu-desktop
  i3

  firefox-esr

  # Required by terminator
  dbus-x11
  terminator
  tmux

  # Integration with tmux and system clipboard
  xsel

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
