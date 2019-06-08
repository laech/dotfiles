#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

[[ "$(id -u)" == "0" ]] \
    && echo "run this script as a normal user" 1>&2 \
    && exit 1

readonly profile_base=base
readonly profile_nokbd=nokbd
readonly profile_fruit=fruit
readonly profile=${profile:?"'$profile_base' or '$profile_fruit' or '$profile_nokbd'?"}

[[ "$profile" != "$profile_base" ]] \
    && [[ "$profile" != "$profile_fruit" ]] \
    && echo "no such profile: $profile" 1>&2 \
    && exit 1

packages=(

    i3-wm
    i3lock
    i3status
    xorg-server
    xorg-xinit
    xorg-xev
    xorg-xbacklight
    xss-lock
    rofi
    feh

    firefox

    tmux
    xterm
    xsel
    zsh
    zsh-completions
    trash-cli

    git

    vim
    emacs
    xcape

    pulseaudio
    pavucontrol

    ttf-dejavu
    ttf-liberation
    ttf-ubuntu-font-family
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    adobe-source-sans-pro-fonts
    adobe-source-serif-pro-fonts

    jdk8-openjdk
    openjdk8-src

    argyllcms
    tlp
    powertop
    x86_energy_perf_policy
    ethtool
    smartmontools

    compton

    ibus
    ibus-libpinyin

    wpa_supplicant

    openssh
    keepassxc

    cups # Printing
    sane  # Scaning
    hplip # HP PSC 1315 Series, hp-setup, hp-scan
    python-pyqt5 # Needed by hp-setup
)

dirs_prefix='base/system/'

dirs=(
    'usr/share/X11/xkb/symbols'
)

services=(
    systemd-timesyncd
    tlp
)

console_map='base/system/kbd/custom.map'

if [[ "$profile" == "$profile_fruit" ]]; then

    packages+=(
        base-devel
        broadcom-wl
        refind-efi
    )

    dirs_prefix='fruit/system/'

    dirs+=(
        'boot/EFI/refind'
        'boot/EFI/arch'
        'etc/modprobe.d'
        'etc/systemd/system/dhcpcd@wlp2s0.service.d'
        'etc/udev/rules.d'
        'etc/X11/xorg.conf.d'
        'usr/lib/systemd/system'
    )

    services+=(
        disable-bluetooth.service
        disable-ir.service
    )

    console_map='fruit/system/kbd/custom-fruit.map'
fi

echo ""
echo "installing packages..."
sudo pacman -S --needed "${packages[@]}"

echo ""
echo "linking font config..."
sudo ln -vsfn ../conf.avail/30-ttf-liberation-sans.conf  /etc/fonts/conf.d/
sudo ln -vsfn ../conf.avail/30-ttf-liberation-serif.conf /etc/fonts/conf.d/
sudo ln -vsfn ../conf.avail/30-ttf-liberation-mono.conf  /etc/fonts/conf.d/

cd "$(dirname "${BASH_SOURCE[0]}")"

echo ""
echo "copying system files..."
for dir in "${dirs[@]}"; do
    sudo mkdir -p "/$dir"
    for file in $(ls -I '*~' -I '#*#' "$dirs_prefix$dir"); do
        sudo cp -v "$dirs_prefix$dir/$file" "/$dir/"
    done
done

echo ""
echo "reloading services..."
sudo systemctl daemon-reload
for service in "${services[@]}"; do
    echo "starting $service..."
    sudo systemctl enable "$service"
    sudo systemctl start "$service"
done

[[ "$profile" == "$profile_fruit" ]] \
    && echo "" \
    && echo "setting refind..." \
    && sudo refind-install \
    && (grep -F 'include fruit.conf' /boot/EFI/refind/refind.conf || \
            echo 'include fruit.conf' >> /boot/EFI/refind/refind.conf) \
    && echo "" \
    && echo "setting nvidia..." \
    && sudo pacman -U --noconfirm \
            fruit/nvidia/nvidia-340xx-340.107-90-x86_64.pkg.tar.xz \
            fruit/nvidia/nvidia-340xx-utils-340.107-3-x86_64.pkg.tar.xz \
    && echo "" \
    && echo "setting mkinitcpio..." \
    && sudo mkinitcpio -p linux

[[ "$profile" != "$profile_nokbd" ]] \
    && echo "" \
    && echo "setting virtual console keymap..." \
    && sudo cp -v "$console_map" '/usr/share/kbd/keymaps/i386/qwerty/custom.map' \
    && sudo localectl set-keymap custom

echo ""
echo "linking default user profile..."
ln -vsfn "$profile" default
