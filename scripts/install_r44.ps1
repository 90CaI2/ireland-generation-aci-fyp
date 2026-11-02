$installer = 'Z:\R\downloads\R-4.4.3-win.exe'
$dir = 'Z:\R\R-4.4.3'
Write-Host "Installing R 4.4.3 to $dir"
$proc = Start-Process -FilePath $installer -ArgumentList @(
  '/VERYSILENT',
  '/NORESTART',
  '/CURRENTUSER',
  "/DIR=$dir"
) -Wait -PassThru
Write-Host "exit=$($proc.ExitCode)"
$rscript = Join-Path $dir 'bin\x64\Rscript.exe'
Write-Host "exists=$(Test-Path $rscript)"
if (Test-Path $rscript) { & $rscript --version }
