import { ensureDir, move, walk } from "jsr:@std/fs@1.0.1";

const template = `use proconio::input;

fn main() {
    input! {}
}
`;

async function processFiles(baseDir: string) {
    for await (const entry of walk(baseDir, { exts: [".rs"] })) {
        if (entry.isFile) {
            const filePath = entry.path;
            console.log(`\nProcessing ${filePath}`);

            const fileName = filePath.split("/").pop();
            console.log(`File name: ${fileName}`);

            const contestDir = filePath.split("/")[2];
            console.log(`Contest dir: ${contestDir}`);

            const name = fileName?.split(".")[0];
            console.log(`Name: ${name}`);

            if (fileName && fileName.endsWith(".rs")) {
                const fileContent = await Deno.readTextFile(filePath);
                if (fileContent.trim() !== template.trim()) {
                    const newFilePath =
                        `${contestDir}/${contestDir}_${name}.rs`;
                    console.log(`New file path: ${newFilePath}`);

                    await ensureDir(contestDir);
                    await move(filePath, newFilePath);
                    console.log(`Moved ${filePath} to ${newFilePath}`);
                }
            }
        }
    }
}

//await processFiles("./contests/1.42.0");
