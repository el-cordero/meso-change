import fs from "node:fs/promises";
import { FileBlob, SpreadsheetFile } from "@oai/artifact-tool";

const inputPath = process.argv[2];
const outputDir = process.argv[3];
await fs.mkdir(outputDir, { recursive: true });

const input = await FileBlob.load(inputPath);
const workbook = await SpreadsheetFile.importXlsx(input);

const overview = await workbook.inspect({
  kind: "workbook,sheet,table,region",
  maxChars: 30000,
  tableMaxRows: 100,
  tableMaxCols: 20,
  tableMaxCellChars: 1000,
});
await fs.writeFile(`${outputDir}/overview.ndjson`, overview.ndjson, "utf8");

const sheets = workbook.worksheets.items;
for (let i = 0; i < sheets.length; i += 1) {
  const sheet = sheets[i];
  const used = sheet.getUsedRange();
  const address = used?.address ?? "A1";
  const detail = await workbook.inspect({
    kind: "region,table,computedStyle",
    sheetId: sheet.name,
    range: address,
    maxChars: 100000,
    tableMaxRows: 200,
    tableMaxCols: 30,
    tableMaxCellChars: 3000,
  });
  await fs.writeFile(`${outputDir}/sheet-${i + 1}-${sheet.name.replace(/[^A-Za-z0-9_-]/g, "_")}.ndjson`, detail.ndjson, "utf8");
  const preview = await workbook.render({
    sheetName: sheet.name,
    autoCrop: "all",
    scale: 1.5,
    format: "png",
  });
  await fs.writeFile(`${outputDir}/sheet-${i + 1}-${sheet.name.replace(/[^A-Za-z0-9_-]/g, "_")}.png`, new Uint8Array(await preview.arrayBuffer()));
}

console.log(JSON.stringify({ sheets: sheets.map((s) => s.name) }));
