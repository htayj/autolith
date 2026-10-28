param([Parameter(Mandatory=$true)][string]$Archive)
$ErrorActionPreference = 'Stop'
$name = [IO.Path]::GetFileNameWithoutExtension($Archive)
$sum = "$Archive.sha256"
$line = @(Get-Content $sum | Where-Object { $_.Trim() })
if ($line.Count -ne 1 -or $line[0] -notmatch '^([0-9A-Fa-f]{64})\s+\*?(.+)$' -or
    (Get-FileHash $Archive -Algorithm SHA256).Hash -ne $Matches[1]) {
  throw 'Checksum validation failed.'
}
$temporaryRoot = [IO.Path]::GetFullPath($env:RUNNER_TEMP).TrimEnd('\')
$tmp = Join-Path $temporaryRoot "validate-$PID"
try {
  Expand-Archive $Archive $tmp
  $root = Join-Path $tmp $name
  & (Join-Path $root 'bin\autolith.ps1') --autolith-release-probe
  if ($LASTEXITCODE) { throw 'Release probe failed.' }
  $probe = Join-Path $root 'libexec\autolith\script\windows-colorlisp.lisp'
  & (Join-Path $root 'runtime\sbcl.exe') --script $probe validate (Join-Path $root 'native\colorlisp.dll')
  if ($LASTEXITCODE) { throw 'Packaged ColorLisp highlighting failed.' }
} finally {
  if ((Split-Path -Parent ([IO.Path]::GetFullPath($tmp))) -ne $temporaryRoot) {
    throw 'Unexpected release validation cleanup path.'
  }
  # Windows can release a loaded DLL's mapping just after its process exits.
  for ($attempt = 1; $attempt -le 100 -and (Test-Path -LiteralPath $tmp); $attempt++) {
    try {
      Remove-Item -LiteralPath $tmp -Recurse -Force
      break
    } catch {
      if ($attempt -eq 100) { throw }
      Start-Sleep -Milliseconds 100
    }
  }
}
