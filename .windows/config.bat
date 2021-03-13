for /d %%p in ("C:\Users\Lae\AppData\Local\Packages\Microsoft.WindowsTerminal_*") do (
  mklink /D "%%~fp\LocalState" C:\Users\Lae\projects\self\dotfiles\.windows\terminal
)

mklink /D C:\Users\Lae\.azure C:\Users\Lae\projects\self\dotfiles\.azure
mklink /D C:\Users\Lae\Documents\PowerShell C:\Users\Lae\projects\self\dotfiles\.windows\powershell
mklink /D C:\Users\Lae\AppData\Roaming\Code\User C:\Users\Lae\projects\self\dotfiles\.config\Code\User
mklink /J C:\Users\Lae\AppData\Roaming\JetBrains\IdeaIC2021.1 C:\Users\Lae\projects\self\dotfiles\.config\JetBrains\idea
mklink /J C:\Users\Lae\AppData\Roaming\JetBrains\IntelliJIdea2021.1 C:\Users\Lae\projects\self\dotfiles\.config\JetBrains\idea

mklink C:\Users\Lae\.gitconfig C:\Users\Lae\projects\self\dotfiles\.windows\.gitconfig

pause
