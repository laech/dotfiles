
if [[ -z "$(grep pam_gnome_keyring /etc/pam.d/login)" ]]; then
    echo "
Message from ${BASH_SOURCE[0]}:

GNOME Keyring PAM configuration not found.
See https://wiki.archlinux.org/index.php/GNOME/Keyring#PAM_method

Git integration:
git config --global credential.helper /usr/lib/git-core/git-credential-libsecret
" 1>&2
fi

aa-enabled &> /dev/null || echo -n "
AppArmor status: $(aa-enabled)
See https://wiki.archlinux.org/index.php/AppArmor
" 1>&2
