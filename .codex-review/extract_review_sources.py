import json
import re
import sys
import zipfile
from pathlib import Path

from lxml import etree
from pypdf import PdfReader


W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
NS = {"w": W}


def node_text(node, include_deleted=False):
    parts = []
    for child in node.iter():
        if child.tag == f"{{{W}}}delText" and not include_deleted:
            continue
        if child.tag in {f"{{{W}}}t", f"{{{W}}}delText"}:
            parts.append(child.text or "")
        elif child.tag == f"{{{W}}}tab":
            parts.append("\t")
        elif child.tag == f"{{{W}}}br":
            parts.append("\n")
    return re.sub(r"[ \t]+", " ", "".join(parts)).strip()


def main(docx_path, pdf_path, out_dir):
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)

    reader = PdfReader(pdf_path)
    page_records = []
    with (out / "pages.txt").open("w", encoding="utf-8") as handle:
        for index, page in enumerate(reader.pages, start=1):
            text = page.extract_text() or ""
            page_records.append({"page": index, "text": text})
            handle.write(f"\n===== PAGE {index} =====\n{text}\n")
    (out / "pages.json").write_text(json.dumps(page_records, indent=2), encoding="utf-8")

    with zipfile.ZipFile(docx_path) as archive:
        document = etree.fromstring(archive.read("word/document.xml"))
        comments = []
        if "word/comments.xml" in archive.namelist():
            comments_root = etree.fromstring(archive.read("word/comments.xml"))
            for comment in comments_root.xpath("//w:comment", namespaces=NS):
                comments.append({
                    "id": comment.get(f"{{{W}}}id"),
                    "author": comment.get(f"{{{W}}}author"),
                    "date": comment.get(f"{{{W}}}date"),
                    "text": node_text(comment, include_deleted=True),
                })

    records = []
    body = document.find("w:body", NS)
    for child in body:
        if child.tag == f"{{{W}}}p":
            visible = node_text(child)
            all_text = node_text(child, include_deleted=True)
            style_nodes = child.xpath("./w:pPr/w:pStyle", namespaces=NS)
            style = style_nodes[0].get(f"{{{W}}}val") if style_nodes else None
            records.append({"kind": "paragraph", "style": style, "visible": visible, "all": all_text})
        elif child.tag == f"{{{W}}}tbl":
            table_rows = []
            for row in child.xpath("./w:tr", namespaces=NS):
                table_rows.append([node_text(cell) for cell in row.xpath("./w:tc", namespaces=NS)])
            records.append({"kind": "table", "rows": table_rows})

    revisions = {
        "insertions": [node_text(n, include_deleted=True) for n in document.xpath("//w:ins", namespaces=NS)],
        "deletions": [node_text(n, include_deleted=True) for n in document.xpath("//w:del", namespaces=NS)],
    }
    (out / "document_structure.json").write_text(json.dumps(records, indent=2), encoding="utf-8")
    (out / "revisions.json").write_text(json.dumps(revisions, indent=2), encoding="utf-8")
    (out / "comments.json").write_text(json.dumps(comments, indent=2), encoding="utf-8")

    with (out / "document_structure.txt").open("w", encoding="utf-8") as handle:
        for i, record in enumerate(records, start=1):
            if record["kind"] == "paragraph":
                handle.write(f"P{i:04d} [{record['style'] or '-'}] {record['visible']}\n")
            else:
                handle.write(f"T{i:04d}\n")
                for j, row in enumerate(record["rows"], start=1):
                    handle.write(f"  R{j:02d}: " + " | ".join(row) + "\n")

    print(json.dumps({
        "pages": len(page_records),
        "body_records": len(records),
        "insertions": len(revisions["insertions"]),
        "deletions": len(revisions["deletions"]),
        "comments": len(comments),
    }))


if __name__ == "__main__":
    main(*sys.argv[1:4])
