use std::path::Path;
use super::ExecutableSuggestion;

fn is_executable_elf(elf: &Path) -> bool {
    let mut readelf = std::process::Command::new("readelf")
        .arg("-l")
        .arg(&elf)
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to run readelf");
    let grep = std::process::Command::new("grep")
        .arg("-q")
        .arg(" INTERP ")
        .stdin(readelf.stdout.take().expect("std Command API is retarded"))
        .spawn()
        .expect("Failed to run grep")
        .wait()
        .expect("Failed to wait for grep to exit");
    let readelf = readelf
        .wait()
        .expect("Failed to wait for grep to exit");
    if !readelf.success() {
        panic!("readelf -l {} failed with exit status {}", elf.display(), readelf);
    }
    match grep.code().expect("grep was killed") {
        0 => true,
        1 => false,
        _ => panic!("grep -q ' INTERP ' failed with exit status {}", grep),
    }
}

fn has_debug_info(elf: &Path) -> bool {
    let mut objdump = std::process::Command::new("objdump")
        .arg("--syms")
        .arg(&elf)
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("Failed to run objdump");
    let grep = std::process::Command::new("grep")
        .arg("-q")
        .arg("\\.debug_info")
        .stdin(objdump.stdout.take().expect("std Command API is retarded"))
        .spawn()
        .expect("Failed to run grep")
        .wait()
        .expect("Failed to wait for grep to exit");
    let objdump = objdump
        .wait()
        .expect("Failed to wait for grep to exit");
    if !objdump.success() {
        panic!("objdump --syms {} failed with exit status {}", elf.display(), objdump);
    }
    match grep.code().expect("grep was killed") {
        0 => true,
        1 => false,
        _ => panic!("grep -q ' \\.debug_info ' failed with exit status {}", grep),
    }
}

pub fn suggest_executables(source_dir: &Path) -> Vec<ExecutableSuggestion> {
    let mut executables = Vec::new();
    for file in walkdir::WalkDir::new(source_dir) {
        let file = file.expect("failed to walk dir");
        if file.file_type().is_file() {
            let file_path = file.path();
            let output = std::process::Command::new("file")
                .arg(&file_path)
                .output()
                .expect("Failed to run file");
            let output = String::from_utf8(output.stdout).expect("file returned non-UTF-8 output");
            let is_elf = output.contains("ELF");
            if output.contains("executable") || (is_elf && is_executable_elf(&file_path)) {
                let path = file.file_name().to_os_string().into_string().expect("non-UTF-8 executable name");
                let suggestion = if is_elf {
                    ExecutableSuggestion {
                        path,
                        is_path_relative: true,
                        is_arch_dependent: true,
                        skip_debug_symbols: !has_debug_info(&file_path),
                        summary: None,
                        long_doc: None,
                        csproj: None,
                    }
                } else {
                    ExecutableSuggestion {
                        path,
                        is_path_relative: true,
                        is_arch_dependent: false,
                        skip_debug_symbols: false,
                        summary: None,
                        long_doc: None,
                        csproj: None,
                    }
                };

                executables.push(suggestion);
            }
        }
    }
    executables
}
