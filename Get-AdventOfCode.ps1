[CmdletBinding()]
param (
    # Parameter help description
    [Parameter()]
    [int]
    $Day = [datetime]::Now.Day,
    [Parameter()]
    [int]
    $Year = [datetime]::Now.Year,
    [Parameter()]
    [Alias("PSPath")]
    [ValidateNotNullOrEmpty()]
    [string]
    $CookiePath = "cookie.txt"
)

$SessionValue = Get-Content $CookiePath
$session =  New-Object Microsoft.PowerShell.Commands.WebRequestSession
$cookie =  New-Object System.Net.Cookie('session',$SessionValue ,'/','.adventofcode.com')
$session.Cookies.Add($cookie)

if (-not (Test-Path $Year)) {
    mkdir $Year | Out-Null
}

$uri = "https://adventofcode.com/$Year/day/$Day/input"
$path = "./$Year/{0:00}.txt" -f $Day

$result = Invoke-WebRequest -ur $uri -WebSession $session -SkipHttpErrorCheck 

if ($result.StatusCode -eq 200) {
    Write-Host "Writing $uri to $path"
    Set-Content -Path $path ($result.Content)
} else {
    Write-Host "Could not download $uri"
}


