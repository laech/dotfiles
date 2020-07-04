#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

if [[ $UID == 0 ]]; then
  echo "don't run as root" 1>&2
  exit 1
fi

"$(dirname "${BASH_SOURCE[0]}")"/wsl.sh

readonly packages=(
  gnome-tweaks
  xsel
  tlp
  powertop
  firewalld
  firewall-config
  keepassxc
  syncthing
  syncthing-gtk
  openssh-server
  ibus-libpinyin
  gimp
)

sudo apt install -y "${packages[@]}"

sudo systemctl enable --now tlp

sudo systemctl enable --now firewalld
sudo firewall-cmd --zone=public --remove-service=ssh
sudo firewall-cmd --zone=public --remove-service=ssh --permanent

sudo systemctl stop ssh
sudo systemctl disable ssh
