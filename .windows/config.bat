for /d %%p in ("C:\Users\Lae\AppData\Local\Packages\Microsoft.WindowsTerminal_*") do (
  mklink /D "%%~fp\LocalState" C:\Users\Lae\projects\self\dotfiles\.windows\terminal
)

mklink /D C:\Users\Lae\Documents\PowerShell C:\Users\Lae\projects\self\dotfiles\.windows\powershell
mklink /D C:\Users\Lae\AppData\Roaming\.emacs.d C:\Users\Lae\projects\self\dotfiles\.emacs.d
mklink /D C:\Users\Lae\AppData\Roaming\Code\User C:\Users\Lae\projects\self\dotfiles\.config\Code\User
mklink /J C:\Users\Lae\AppData\Roaming\JetBrains\IdeaIC2020.2 C:\Users\Lae\projects\self\dotfiles\.config\JetBrains\idea

mklink C:\Users\Lae\.gitconfig C:\Users\Lae\projects\self\dotfiles\.windows\.gitconfig

pause
