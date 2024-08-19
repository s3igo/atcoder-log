import { ensureDir, move, walk } from "jsr:@std/fs@1.0.1";

const contestRoot = "./contests";

const template = `use proconio::input;

fn main() {
    input! {}
}
`;

async function _processFiles(baseDir: string) {
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
                        `${contestRoot}/${contestDir}/${contestDir}_${name}.rs`;
                    console.log(`New file path: ${newFilePath}`);

                    await ensureDir(`${contestRoot}/${contestDir}`);
                    await move(filePath, newFilePath);
                    console.log(`Moved ${filePath} to ${newFilePath}`);
                }
            }
        }
    }
}

//await processFiles(`${contestRoot}/1.42.0`);

const template2 = `use proconio::input;

fn main() {
    input!();
}
`;

async function _processFilesFor_1_70_0(baseDir: string) {
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

            if (
                fileName && fileName.endsWith(".rs") &&
                contestDir !== "tessoku-book" // Skip the tessoku-book directory
            ) {
                const fileContent = await Deno.readTextFile(filePath);
                if (
                    fileContent.trim() !== template.trim() &&
                    fileContent.trim() !== template2.trim()
                ) {
                    const newFilePath =
                        `${contestRoot}/${contestDir}/${contestDir}_${name}.rs`;
                    console.log(`New file path: ${newFilePath}`);

                    await ensureDir(`${contestRoot}/${contestDir}`);
                    await move(filePath, newFilePath);
                    console.log(`Moved ${filePath} to ${newFilePath}`);
                }
            }
        }
    }
}

//await processFilesFor_1_70_0(`${contestRoot}/1.70.0`);
