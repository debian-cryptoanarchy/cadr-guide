use std::io;
use std::path::Path;
use super::ExecutableSuggestion;

fn deserialize_ignore_any<'de, D: serde::Deserializer<'de>>(deserializer: D) -> Result<(), D::Error> {
    use serde::Deserialize;

    serde::de::IgnoredAny::deserialize(deserializer)?;
    Ok(())
}

pub fn suggest_executables(source_dir: &Path) -> Vec<ExecutableSuggestion> {
    use io::BufRead;

    #[derive(serde_derive::Deserialize)]
    struct PropertyGroup {
        #[serde(rename = "OutputType", default)]
        output_type: Option<String>,
        #[serde(rename = "Description", default)]
        description: Option<String>,
        #[serde(rename = "AssemblyTitle", default)]
        title: Option<String>,
    }

    #[derive(serde_derive::Deserialize)]
    enum Item {
        PropertyGroup(PropertyGroup),
        #[serde(other, deserialize_with = "deserialize_ignore_any")]
        Other,
    }

    #[derive(serde_derive::Deserialize)]
    struct CSProj {
        #[serde(rename = "$value")]
        items: Vec<Item>,
    }

    let mut executables = Vec::new();
    for file in std::fs::read_dir(&source_dir).expect("failed to list source directory") {
        let file = file.expect("Failed to get file in source directory");
        let path = file.path();
        if path.extension() == Some("sln".as_ref()) {
            let sln = std::fs::File::open(&path).expect("Failed to open solution file");
            let sln = io::BufReader::new(sln);
            for line in sln.lines() {
                let line = line.expect("failed to read sln");
                if line.starts_with("Project(") {
                    let eqpos = line.find('=').expect("Project entry in sln missing `=`");
                    let mut parts = line[(eqpos + 1)..].split(',');
                    let bin_name = parts.next().expect("Missing name");
                    let bin_name = &bin_name.trim()[1..];
                    let bin_name = &bin_name[..(bin_name.len() - 1)];
                    let path_with_end_quote = &parts.next().expect("missing path").trim()[1..];
                    let path = &path_with_end_quote[..(path_with_end_quote.len() - 1)];
                    if path.ends_with(".csproj") {
                        let csproj_rel_path = path.replace('\\', "/");
                        let path = source_dir.join(&csproj_rel_path);
                        let csproj_str = std::fs::read_to_string(&path)
                            .unwrap_or_else(|error| panic!("failed to read {}: {}", path.display(), error));

                        let csproj_trimmed = if csproj_str.starts_with("\u{feff}") {
                            &csproj_str[3..]
                        } else {
                            &csproj_str
                        }.trim();

                        let csproj = serde_xml_rs::from_reader::<_, CSProj>(csproj_trimmed.as_bytes())
                            .expect("failed to load csproj");
                        let is_exe = csproj.items.iter().any(|group| {
                            match group {
                                Item::PropertyGroup(PropertyGroup { output_type: Some(output_type), .. }) => output_type == "Exe",
                                _ => false,
                            }
                        });

                        let summary = csproj.items.iter().find_map(|item| {
                            match item {
                                Item::PropertyGroup(PropertyGroup { title, .. }) => title.as_ref(),
                                _ => None,
                            }
                        }).map(Clone::clone);

                        let long_doc = csproj.items.iter().find_map(|item| {
                            match item {
                                Item::PropertyGroup(PropertyGroup { description, .. }) => description.as_ref(),
                                _ => None,
                            }
                        }).map(Clone::clone);

                        let (summary, long_doc) = match (summary, long_doc) {
                            (None, Some(description)) => (Some(description), None),
                            tuple => tuple,
                        };

                        if is_exe {
                            let suggestion = ExecutableSuggestion {
                                path: bin_name.to_owned(),
                                is_path_relative: true,
                                is_in_destdir: true,
                                is_arch_dependent: true,
                                skip_debug_symbols: true,
                                summary,
                                long_doc,
                                csproj: Some(csproj_rel_path),
                            };

                            executables.push(suggestion);
                        }
                    }
                }
            }
        }
    }

    executables
}
