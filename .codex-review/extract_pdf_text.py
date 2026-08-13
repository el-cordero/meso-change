import sys
from pathlib import Path
from pypdf import PdfReader

source = Path(sys.argv[1])
target = Path(sys.argv[2])
reader = PdfReader(source)
with target.open("w", encoding="utf-8") as handle:
    for page_number, page in enumerate(reader.pages, start=1):
        handle.write(f"\n===== PAGE {page_number} =====\n")
        handle.write(page.extract_text() or "")
        handle.write("\n")
print(f"{len(reader.pages)} pages")
