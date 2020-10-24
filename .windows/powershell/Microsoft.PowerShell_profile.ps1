Set-PSReadlineOption -EditMode Emacs
Set-PSReadLineKeyHandler -Key 'delete' -Function DeleteCharOrExit
Set-PSReadLineKeyHandler -Key 'ctrl+rightarrow' -Function ForwardWord
Set-PSReadLineKeyHandler -Key 'ctrl+leftarrow' -Function BackwardWord
Set-PSReadLineKeyHandler -Key 'ctrl+delete' -Function KillWord
Set-PSReadLineKeyHandler -Key 'ctrl+x' -Function Cut
Set-PSReadLineKeyHandler -Key 'ctrl+c' -Function CopyOrCancelLine
Set-PSReadLineKeyHandler -Key 'ctrl+v' -Function Paste
Set-PSReadLineKeyHandler -Key 'ctrl+z' -Function Undo
Set-PSReadLineKeyHandler -Key 'ctrl+shift+z' -Function Redo
Set-Alias -Name ll -Value ls
