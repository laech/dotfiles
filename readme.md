Starting from scratch:

```
git init --bare "${HOME}/.cfg"
alias config='git --git-dir="${HOME}/.cfg" --work-tree="${HOME}"'
config config --local status.showUntrackedFiles no
```

Install on new system:

```
git clone --bare git@gitlab.com:lae/dotfiles.git "${HOME}/.cfg"
alias config='git --git-dir="${HOME}/.cfg" --work-tree="${HOME}"'
config config --local status.showUntrackedFiles no
config checkout
```
