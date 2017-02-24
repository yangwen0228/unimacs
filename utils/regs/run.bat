@echo off
setlocal

:begin
cls
echo.
echo. 1.将"Edit With Emacs"添加到右键菜单中
echo. 2.将"Edit With Emacs"从右键菜单中移除
echo. 3.将"复制文件路径"添加到右键菜单中
echo. 4.将"复制文件路径"从右键菜单中移除
echo. 5.将"交换CapsLock与右ctrl键"添加到注册表中，需重启电脑生效
echo. 6.将"交换CapsLock与右ctrl键"从注册表中移除，需重启电脑生效
echo. q.退出
echo.

set type=q
set /p type= 请选择相应数字，或者按q退出：

if "%type%" == "1" cls & goto addEdit
if "%type%" == "2" cls & goto delEdit
if "%type%" == "3" cls & goto addCopy
if "%type%" == "4" cls & goto delCopy
if "%type%" == "5" cls & goto addSwap
if "%type%" == "6" cls & goto delSwap
if "%type%" == "q" cls & exit

:addEdit
cls
set emacsDir=c:\emacs\bin

:inputPath
if not exist %emacsDir% (
set /p emacsDir=请输入emacs.exe所在的路径（如：C:\Emacs\bin），或按q返回菜单：
) else (
if not exist %emacsDir%\runemacs.exe (
set /p emacsDir=请输入emacs.exe所在的路径（如：C:\Emacs\bin），或按q返回菜单：
)
)
if "%emacsDir%" == "q" (
goto begin
)
if not exist %emacsDir% (
echo %emacsDir% 不存在，请重新输入！
goto inputPath
)
if not exist %emacsDir%\runemacs.exe (
echo 设置的路径%emacsDir%不正确，请重新输入。。。
goto inputPath
)
set emacsDir=%emacsDir:\=\\%

echo Windows Registry Editor Version 5.00> edit_with_emacs.reg
echo [HKEY_CLASSES_ROOT\*\shell\Edit In Emacs]>> edit_with_emacs.reg
echo @="">> edit_with_emacs.reg
echo [HKEY_CLASSES_ROOT\*\shell\Edit In Emacs\command]>> edit_with_emacs.reg
echo @="\"%emacsDir%\\emacsclientw.exe\" -a \"%emacsDir%\\runemacs.exe\" -n \"%%1\"">> edit_with_emacs.reg

echo [HKEY_CLASSES_ROOT\*\shell\Edit With New Emacs]>> edit_with_emacs.reg
echo @="">> edit_with_emacs.reg
echo [HKEY_CLASSES_ROOT\*\shell\Edit With New Emacs\command]>> edit_with_emacs.reg
echo @="\"%emacsDir%\\emacsclientw.exe\" -a \"%emacsDir%\\runemacs.exe\" -nc \"%%1\"">> edit_with_emacs.reg

regedit /S edit_with_emacs.reg
del edit_with_emacs.reg
echo 右键"Edit With Emacs"已经成功添加！
pause
goto begin

:delEdit
echo Windows Registry Editor Version 5.00> _edit_with_emacs.reg
echo [-HKEY_CLASSES_ROOT\*\shell\Edit In Emacs]>> _edit_with_emacs.reg
echo [-HKEY_CLASSES_ROOT\*\shell\Edit With New Emacs]>> _edit_with_emacs.reg
regedit /S _edit_with_emacs.reg
del _edit_with_emacs.reg

echo 右键"Edit With Emacs"已经成功删除！
pause
goto begin

:addCopy
echo Windows Registry Editor Version 5.00> copy_file_path.reg
echo [HKEY_CLASSES_ROOT\Directory\shell\copypath]>> copy_file_path.reg
echo @="复制文件夹路径">> copy_file_path.reg
echo [HKEY_CLASSES_ROOT\Directory\shell\copypath\command]>> copy_file_path.reg
echo @="mshta vbscript:clipboarddata.setdata(\"text\",\"%%1\")(close)">> copy_file_path.reg
echo [HKEY_CLASSES_ROOT\*\shell\copypath]>> copy_file_path.reg
echo @="复制文件路径">> copy_file_path.reg
echo [HKEY_CLASSES_ROOT\*\shell\copypath\command]>> copy_file_path.reg
echo @="mshta vbscript:clipboarddata.setdata(\"text\",\"%%1\")(close)">> copy_file_path.reg
regedit /S copy_file_path.reg
del copy_file_path.reg
echo 右键"复制文件路径"已经成功添加！
pause
goto begin

:delCopy
echo Windows Registry Editor Version 5.00> _copy_file_path.reg
echo [-HKEY_CLASSES_ROOT\Directory\shell\copypath]>> _copy_file_path.reg
echo [-HKEY_CLASSES_ROOT\*\shell\copypath]>> _copy_file_path.reg
regedit /S _copy_file_path.reg
del _copy_file_path.reg
echo 右键"复制文件路径"已经成功删除！
pause
goto begin

:addSwap
echo Windows Registry Editor Version 5.00> swap_key.reg
echo [HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout]>> swap_key.reg
echo "Scancode Map"=hex:00,00,00,00,00,00,00,00,02,00,00,00,1d,e0,3a,00,3a,00,1d,e0,00,00,00,00>> swap_key.reg
regedit /S swap_key.reg
del swap_key.reg
echo "交换CapsLock和右键ctrl"已经成功添加进注册表，不过需要重启电脑才能生效！！！
pause
goto begin

:delSwap
echo Windows Registry Editor Version 5.00> _swap_key.reg
echo [-HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout]>> _swap_key.reg
regedit /S _swap_key.reg
del _swap_key.reg
echo "交换CapsLock和右键ctrl"已经从注册表中删除，不过需要重启电脑才能生效！！！
pause
goto begin
