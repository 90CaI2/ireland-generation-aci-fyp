import re
import urllib.request
from pathlib import Path

# Download Rtools45 to Z:
rtools_url = "https://cran.r-project.org/bin/windows/Rtools/rtools45/files/rtools45-6768-6492.exe"
rtools_out = Path(r"Z:\R\downloads\rtools45-6768-6492.exe")
print("downloading", rtools_url)
urllib.request.urlretrieve(rtools_url, rtools_out)
print("saved", rtools_out, rtools_out.stat().st_size)

# Probe opera archive / binaries
for url in [
    "https://cran.r-project.org/src/contrib/Archive/opera/",
    "https://cran.r-project.org/bin/windows/contrib/4.5/",
    "https://cran.r-project.org/bin/windows/contrib/4.4/",
]:
    try:
        html = urllib.request.urlopen(url).read().decode("utf-8", "replace")
        hits = re.findall(r'href="(opera[^"]+)"', html)
        print(url, hits[:10])
    except Exception as exc:
        print("FAIL", url, exc)
