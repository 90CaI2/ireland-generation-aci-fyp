$installer = 'Z:\R\downloads\R-4.6.1-win.exe'
$dir = 'Z:\R\R-4.6.1'
Write-Host "Installing to $dir"
$args = @(
  '/VERYSILENT',
  '/NORESTART',
  '/CURRENTUSER',
  "/DIR=$dir"
)
$proc = Start-Process -FilePath $installer -ArgumentList $args -Wait -PassThru
Write-Host "exit=$($proc.ExitCode)"
$rscript = Join-Path $dir 'bin\x64\Rscript.exe'
Write-Host "exists=$(Test-Path $rscript)"
if (Test-Path $rscript) {
  & $rscript --version
}
