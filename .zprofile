
if [[ -z "$(grep pam_gnome_keyring /etc/pam.d/login)" ]]; then
    echo "
GNOME Keyring PAM configuration not found.
See https://wiki.archlinux.org/index.php/GNOME/Keyring#PAM_method
" 1>&2
fi
