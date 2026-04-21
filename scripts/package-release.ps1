param(
  [string]$OutputDir = "dist",
  [string]$Platform = "windows",
  [string]$Arch = ""
)

$ErrorActionPreference = "Stop"

if ([string]::IsNullOrWhiteSpace($Arch)) {
  switch ($env:PROCESSOR_ARCHITECTURE) {
    "AMD64" { $Arch = "amd64" }
    "ARM64" { $Arch = "arm64" }
    default {
      throw "unsupported architecture: $env:PROCESSOR_ARCHITECTURE"
    }
  }
}

if ($Platform -ne "windows") {
  throw "package-release.ps1 only supports windows packages"
}

$asset = "ohspeed-$Platform-$Arch"
$binary = Join-Path "_build/install/default/bin" "ohspeed.exe"
if (-not (Test-Path $binary)) {
  throw "missing built executable: $binary"
}

$stage = Join-Path ([System.IO.Path]::GetTempPath()) ([System.IO.Path]::GetRandomFileName())
New-Item -ItemType Directory -Path $stage | Out-Null
New-Item -ItemType Directory -Path $OutputDir -Force | Out-Null

try {
  Copy-Item $binary (Join-Path $stage "ohspeed.exe")
  Copy-Item "README.md" $stage
  Copy-Item "LICENSE" $stage

  $skipDlls = @(
    "advapi32.dll",
    "bcrypt.dll",
    "crypt32.dll",
    "gdi32.dll",
    "iphlpapi.dll",
    "kernel32.dll",
    "msvcrt.dll",
    "ntdll.dll",
    "ole32.dll",
    "secur32.dll",
    "shell32.dll",
    "user32.dll",
    "ws2_32.dll"
  )

  function Get-DllNames([string]$Path) {
    $objdump = Get-Command "objdump" -ErrorAction SilentlyContinue
    if ($null -eq $objdump) {
      return @()
    }

    & $objdump.Source -p $Path 2>$null |
      Select-String "DLL Name:" |
      ForEach-Object { ($_.Line -replace "^.*DLL Name:\s*", "").Trim() } |
      Where-Object { $_ -ne "" }
  }

  function Resolve-Dll([string]$Name) {
    $candidates = @()
    if (Test-Path "_opam") {
      $candidates += Get-ChildItem "_opam" -Recurse -Filter $Name -ErrorAction SilentlyContinue
    }
    $where = & where.exe $Name 2>$null
    foreach ($path in $where) {
      if ($path -and (Test-Path $path)) {
        $candidates += Get-Item $path
      }
    }

    $candidates |
      Where-Object { $_.FullName -notmatch "\\Windows\\System32\\" } |
      Select-Object -First 1
  }

  $queue = New-Object System.Collections.Generic.Queue[string]
  $seen = New-Object System.Collections.Generic.HashSet[string]([System.StringComparer]::OrdinalIgnoreCase)
  $queue.Enqueue((Join-Path $stage "ohspeed.exe"))

  while ($queue.Count -gt 0) {
    $current = $queue.Dequeue()
    foreach ($dll in Get-DllNames $current) {
      if ($skipDlls -contains $dll.ToLowerInvariant()) {
        continue
      }
      if (-not $seen.Add($dll)) {
        continue
      }

      $resolved = Resolve-Dll $dll
      if ($null -eq $resolved) {
        Write-Warning "could not bundle runtime DLL: $dll"
        continue
      }

      $dest = Join-Path $stage $resolved.Name
      Copy-Item $resolved.FullName $dest -Force
      $queue.Enqueue($dest)
    }
  }

  $archive = Join-Path $OutputDir "$asset.zip"
  if (Test-Path $archive) {
    Remove-Item $archive -Force
  }
  Compress-Archive -Path (Join-Path $stage "*") -DestinationPath $archive

  $hash = (Get-FileHash -Algorithm SHA256 $archive).Hash.ToLowerInvariant()
  "$hash  $(Split-Path -Leaf $archive)" |
    Set-Content -Encoding ascii -NoNewline "$archive.sha256"
}
finally {
  Remove-Item $stage -Recurse -Force -ErrorAction SilentlyContinue
}
