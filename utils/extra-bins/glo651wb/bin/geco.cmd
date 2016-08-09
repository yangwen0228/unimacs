:: See ../share/gtags/geco.rc for details.
@SetLocal EnableDelayedExpansion
@set arg=
@for %%O in (%*) do @(
  set opt=%%O
  if not "!opt:~0,1!" == "-" set arg=1
)
@if not defined arg (
  global 2> %TEMP%\global.help
  for /f "delims=" %%L in (%TEMP%\global.help) do @(
    set line=%%L
    if not "!line:~7,10!"=="global -p[" if not "!line:~7,10!"=="global -u[" (
      set line=!line:global=geco!
      echo !line!
    )
  )
  del %TEMP%\global.help
  goto :eof
)
@set GTAGSBLANKENCODE=
@global %* --result=ctags-x | gecoless
