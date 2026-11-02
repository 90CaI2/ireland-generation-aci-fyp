import urllib.request
from pathlib import Path

downloads = Path(r"Z:\R\downloads")
downloads.mkdir(parents=True, exist_ok=True)

files = {
    "R-4.4.3-win.exe": "https://cran.r-project.org/bin/windows/base/old/4.4.3/R-4.4.3-win.exe",
    "opera_1.2.0.zip": "https://packagemanager.posit.co/cran/2025-01-15/bin/windows/contrib/4.4/opera_1.2.0.zip",
}

for name, url in files.items():
    out = downloads / name
    if out.exists() and out.stat().st_size > 1_000_000:
        print(f"skip existing {out} ({out.stat().st_size:,})")
        continue
    print(f"downloading {url}")
    urllib.request.urlretrieve(url, out)
    print(f"saved {out} ({out.stat().st_size:,})")
