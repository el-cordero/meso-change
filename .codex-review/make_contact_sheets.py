import math
import sys
from pathlib import Path
from PIL import Image, ImageDraw

source = Path(sys.argv[1])
target = Path(sys.argv[2])
target.mkdir(parents=True, exist_ok=True)
pages = sorted(source.glob("page-*.png"), key=lambda p: int(p.stem.split("-")[-1]))
per_sheet = 8
thumb_w = 420
for start in range(0, len(pages), per_sheet):
    batch = pages[start:start + per_sheet]
    thumbs = []
    for path in batch:
        image = Image.open(path).convert("RGB")
        ratio = thumb_w / image.width
        thumb = image.resize((thumb_w, int(image.height * ratio)))
        canvas = Image.new("RGB", (thumb.width, thumb.height + 28), "white")
        canvas.paste(thumb, (0, 28))
        ImageDraw.Draw(canvas).text((8, 6), path.stem, fill="black")
        thumbs.append(canvas)
    cols = 2
    rows = math.ceil(len(thumbs) / cols)
    gap = 16
    cell_h = max(t.height for t in thumbs)
    sheet = Image.new("RGB", (cols * thumb_w + (cols + 1) * gap, rows * cell_h + (rows + 1) * gap), "#888888")
    for i, thumb in enumerate(thumbs):
        x = gap + (i % cols) * (thumb_w + gap)
        y = gap + (i // cols) * (cell_h + gap)
        sheet.paste(thumb, (x, y))
    out = target / f"contact-{start + 1:02d}-{start + len(batch):02d}.jpg"
    sheet.save(out, quality=88)
    print(out)
