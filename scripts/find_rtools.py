import re
import urllib.request
from pathlib import Path

html = urllib.request.urlopen("https://cran.r-project.org/bin/windows/Rtools/").read().decode("utf-8", "replace")
links = sorted(set(re.findall(r'href="([^"]*rtools[^"]*)"', html, flags=re.I)))
print("page links:")
for link in links[:40]:
    print(link)

# Prefer installer exe links
exes = [link for link in links if link.lower().endswith(".exe")]
print("exes:", exes)

# Also probe common 4.5 / 4.6 paths
candidates = [
    "https://cran.r-project.org/bin/windows/Rtools/rtools45/files/rtools45.exe",
    "https://cran.r-project.org/bin/windows/Rtools/rtools46/files/rtools46.exe",
    "https://github.com/r-windows/rtools-installer/releases/latest",
]
for url in candidates:
    try:
        req = urllib.request.Request(url, method="HEAD")
        with urllib.request.urlopen(req, timeout=30) as resp:
            print("HEAD", resp.status, url, resp.geturl())
    except Exception as exc:
        print("FAIL", url, type(exc).__name__, exc)
