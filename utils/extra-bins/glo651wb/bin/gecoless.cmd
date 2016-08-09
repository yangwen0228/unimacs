:: See ../share/gtags/geco.rc for details.
@more > %TEMP%\global.tags
@for %%T in (%TEMP%\global.tags) do @(
  if %%~zT LEQ 2 (
    echo No such tag in tags file
  ) else (
    peco < %TEMP%\global.tags | less -T-
  )
)
@del %TEMP%\global.tags
