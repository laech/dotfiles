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
readonly profile_build=build
readonly profile_chain=chain
readonly profile=${profile:?"'$profile_base' or '$profile_fruit' or '$profile_nokbd' or '$profile_build' or '$profile_chain'?"}

[[ "$profile" != "$profile_base" ]] \
    && [[ "$profile" != "$profile_fruit" ]] \
    && [[ "$profile" != "$profile_build" ]] \
    && [[ "$profile" != "$profile_chain" ]] \
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
    ttf-fantasque-sans-mono
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    adobe-source-sans-pro-fonts
    adobe-source-serif-pro-fonts

    # More unicode coverage, test
    # https://unicode-table.com/en/
    ttf-hanazono

    jdk8-openjdk
    openjdk8-src

    argyllcms
    tlp
    powertop
    x86_energy_perf_policy
    ethtool
    smartmontools

    compton
    hsetroot

    ibus
    ibus-libpinyin

    wpa_supplicant

    nftables
    firewalld
    openssh
    keepassxc
    syncthing

    cups # Printing
    sane  # Scaning
    hplip # HP PSC 1315 Series, hp-setup, hp-scan
    python-pyqt5 # Needed by hp-setup
    python-pillow # Needed by hp-scan
)

readonly base_dirs_prefix=base/system/
dirs_prefix="$base_dirs_prefix"

dirs=(
    'usr/share/X11/xkb/symbols'
)

services=(
    nftables
    firewalld
    systemd-timesyncd
    tlp
    tlp-sleep
)

console_map='base/system/kbd/custom.map'

if [[ "$profile" == "$profile_build" ]]; then

    packages+=(
        intel-ucode
    )
    dirs_prefix='build/system/'
    dirs+=(
        'etc/systemd/system/dhcpcd@enp5s0.service.d'
    )
    services+=(
        dhcpcd@enp5s0.service
    )

elif [[ "$profile" == "$profile_fruit" ]]; then

    packages+=(
        intel-ucode
        dkms
        broadcom-wl-dkms
        refind-efi
    )

    dirs_prefix='fruit/system/'

    dirs+=(
        'boot/efi/EFI/refind'
        'boot/efi/EFI/arch'
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

    console_map='fruit/system/kbd/custom.map'

elif [[ "$profile" == "$profile_chain" ]]; then

    packages+=(
        intel-ucode
        dkms
        bbswitch-dkms
        bluez
        bluez-utils
    )
    dirs_prefix='chain/system/'
    dirs+=(
        'etc/modprobe.d'
        'etc/modules-load.d'
        'etc/udev/rules.d'
        'etc/systemd/system/dhcpcd@wlo1.service.d'
        'etc/X11/xorg.conf.d'
    )
    console_map='chain/system/kbd/custom.map'
fi

echo ""
echo "installing packages..."
sudo pacman --sync --needed --noconfirm "${packages[@]}"

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

    [[ -e "$base_dirs_prefix$dir" ]] \
        && find "$base_dirs_prefix$dir" \
                -type f \
                -not -name '*~' \
                -not -name '#*#' \
                -exec sudo cp -v {} "/$dir/" \;

    [[ "$dirs_prefix" != "$base_dirs_prefix" ]] \
        && [[ -e "$dirs_prefix$dir" ]] \
        && find "$dirs_prefix$dir" \
                -type f \
                -not -name '*~' \
                -not -name '#*#' \
                -exec sudo cp -v {} "/$dir/" \;
done

echo ""
echo "reloading services..."
sudo systemctl daemon-reload
for service in "${services[@]}"; do
    echo "starting $service..."
    sudo systemctl enable --now "$service"
done

if [[ "$profile" == "$profile_fruit" ]]; then
    echo ""
    echo "setting refind..."
    sudo refind-install
    if ! grep -F 'include fruit.conf' /boot/efi/EFI/refind/refind.conf; then
        sudo bash -c 'echo "include fruit.conf" >> /boot/efi/EFI/refind/refind.conf'
    fi

    echo ""
    echo "setting nvidia..."
    (cd "$(dirname $0)"/fruit/nvidia-340xx-utils \
         && git clean --force -d \
         && makepkg --cleanbuild --needed --noconfirm --syncdeps)
    sudo pacman --upgrade --needed --noconfirm "$(dirname $0)"/fruit/nvidia-340xx-utils/nvidia-340xx-util*.pkg.tar.xz

    (cd "$(dirname $0)"/fruit/nvidia-340xx \
         && git clean --force -d \
         && makepkg --cleanbuild --needed --noconfirm --syncdeps)
    sudo pacman --upgrade --needed --noconfirm "$(dirname $0)"/fruit/nvidia-340xx/nvidia-340xx-dkms*.pkg.tar.xz

    echo ""
    echo "setting mkinitcpio..."
    sudo mkinitcpio -p linux
fi

if [[ "$profile" != "$profile_nokbd" ]]; then
    echo ""
    echo "setting virtual console keymap..."
    sudo cp -v "$console_map" '/usr/share/kbd/keymaps/i386/qwerty/custom.map'
    sudo localectl set-keymap custom || true # Fails in chroot
fi

echo ""
echo "linking default user profile..."
ln -vsfn "$profile" default
