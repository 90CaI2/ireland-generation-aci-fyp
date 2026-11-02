import re
import urllib.request

html = urllib.request.urlopen("https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html").read().decode("utf-8", "replace")
links = sorted(set(re.findall(r'href="([^"]+)"', html)))
for link in links:
    if any(tok in link.lower() for tok in ["exe", "files", "download", "installer"]):
        print(link)
print("---")
print(html[:2000])
