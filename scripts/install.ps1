$ErrorActionPreference = "Stop"

$Repo = if ($env:OHSPEED_REPO) { $env:OHSPEED_REPO } else { "L0stInFades/ohspeed" }
$Version = if ($env:OHSPEED_VERSION) { $env:OHSPEED_VERSION } else { "latest" }
$InstallDir = if ($env:OHSPEED_INSTALL_DIR) {
  $env:OHSPEED_INSTALL_DIR
} else {
  Join-Path $env:LOCALAPPDATA "Programs\ohspeed"
}

switch ($env:PROCESSOR_ARCHITECTURE) {
  "AMD64" { $Arch = "amd64" }
  default {
    throw "unsupported architecture: $env:PROCESSOR_ARCHITECTURE"
  }
}

$Asset = "ohspeed-windows-$Arch.zip"
if ($Version -eq "latest") {
  $Url = "https://github.com/$Repo/releases/latest/download/$Asset"
} else {
  $Tag = if ($Version.StartsWith("v")) { $Version } else { "v$Version" }
  $Url = "https://github.com/$Repo/releases/download/$Tag/$Asset"
}

$Tmp = Join-Path ([System.IO.Path]::GetTempPath()) ([System.IO.Path]::GetRandomFileName())
New-Item -ItemType Directory -Path $Tmp | Out-Null

try {
  New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null

  $Archive = Join-Path $Tmp $Asset
  Write-Host "downloading $Url"
  try {
    Invoke-WebRequest -Uri $Url -OutFile $Archive
  } catch {
    Write-Error "failed to download $Asset from release $Version; check available releases at https://github.com/$Repo/releases"
    throw
  }

  Expand-Archive -Path $Archive -DestinationPath $Tmp -Force
  Copy-Item (Join-Path $Tmp "ohspeed.exe") $InstallDir -Force
  Get-ChildItem $Tmp -Filter "*.dll" -File | Copy-Item -Destination $InstallDir -Force

  Write-Host "installed: $(Join-Path $InstallDir "ohspeed.exe")"
  $pathEntries = $env:PATH -split ";"
  if ($pathEntries -notcontains $InstallDir) {
    Write-Host "add $InstallDir to PATH if it is not already there"
  }
}
finally {
  Remove-Item $Tmp -Recurse -Force -ErrorAction SilentlyContinue
}
