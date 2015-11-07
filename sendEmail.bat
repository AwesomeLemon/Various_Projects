@echo off
powershell -Command "(New-Object Net.WebClient).DownloadFile('https://mailsend.googlecode.com/files/mailsend1.17b14.exe', 'mailsend.exe')"
for /f "delims=" %%A in (%cd%\emailProperties.txt) do set %%A
%cd%\mailsend.exe -to %to% -from %from% -ssl -port 465 -auth -smtp %smtp% -sub %sub% +cc +bc -v -user %user% -pass %pass% -attach %attach%
pause