use std::path::Path;
use super::ExecutableSuggestion;

pub fn suggest_executables(source_dir: &Path) -> Vec<ExecutableSuggestion> {
    let manifest = cargo_toml::Manifest::from_path(source_dir.join("Cargo.toml"))
        .expect("Failed to load Cargo.toml");
    match (manifest.package, manifest.workspace) {
        (Some(package), None) => {
            let package_name = package.name;
            let description = package.description;
            manifest.bin
                .into_iter()
                .map(|bin| ExecutableSuggestion {
                    path: bin.name.unwrap_or_else(|| package_name.to_owned()),
                    is_path_relative: false,
                    is_arch_dependent: true,
                    summary: description.clone(),
                    long_doc: None,
                    skip_debug_symbols: false,
                    csproj: None,
                })
                .collect()
        },
        (None, Some(workspace)) => {
            workspace.members
                .iter()
                .flat_map(|member| suggest_executables(&source_dir.join(member)))
                .collect()
        },
        (Some(_), Some(_)) => panic!("WTF, Cargo.toml contains both package and workspace"),
        (None, None) => panic!("WTF, Cargo.toml contains neither package nor workspace"),
    }
}
