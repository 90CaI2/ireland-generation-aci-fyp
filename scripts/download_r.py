import urllib.request
from pathlib import Path

url = "https://cran.r-project.org/bin/windows/base/R-4.6.1-win.exe"
out = Path(r"Z:\R\downloads\R-4.6.1-win.exe")
out.parent.mkdir(parents=True, exist_ok=True)
print(f"downloading {url}")
urllib.request.urlretrieve(url, out)
print(f"saved {out} ({out.stat().st_size:,} bytes)")
