for /d %%p in ("C:\Users\Lae\AppData\Local\Packages\Microsoft.WindowsTerminal_*") do (
  mklink /D "%%~fp\LocalState" C:\Users\Lae\projects\self\dotfiles\.windows\terminal
)

mklink /D C:\Users\Lae\AppData\Roaming\.emacs.d C:\Users\Lae\projects\self\dotfiles\.emacs.d
mklink /D C:\Users\Lae\AppData\Roaming\Code\User C:\Users\Lae\projects\self\dotfiles\.config\Code\User
mklink /J C:\Users\Lae\AppData\Roaming\JetBrains\IdeaIC2020.2 C:\Users\Lae\projects\self\dotfiles\.config\JetBrains\idea

pause
