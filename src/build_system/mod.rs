use std::fmt;
use std::io;
use std::path::{Path, PathBuf};

mod cargo;
mod dotnet;
mod none;
mod npm;
mod python3;

pub enum BuildSystem {
    Cargo,
    Python3,
    DotNet,
    Npm,
    None,
}

impl fmt::Display for BuildSystem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self.identifier(), f)
    }
}

impl BuildSystem {
    pub fn identifier(&self) -> &str {
        match self {
            BuildSystem::Cargo => "cargo",
            BuildSystem::Python3 => "python3",
            BuildSystem::DotNet => "dotnet",
            BuildSystem::Npm => "npm",
            BuildSystem::None => "none",
        }
    }

    pub fn suggest_executables(&self, source_dir: &Path) -> Vec<ExecutableSuggestion> {
        match self {
            BuildSystem::Cargo => cargo::suggest_executables(source_dir),
            BuildSystem::None => none::suggest_executables(source_dir),
            BuildSystem::DotNet => dotnet::suggest_executables(source_dir),
            BuildSystem::Python3 => python3::suggest_executables(source_dir),
            BuildSystem::Npm => npm::suggest_executables(source_dir),
        }
    }

    pub fn dependencies_hint(&self) -> &str {
        match self {
            BuildSystem::Cargo => "",
            BuildSystem::Python3 => " or system-wide python packages",
            BuildSystem::DotNet => "",
            BuildSystem::Npm => "",
            BuildSystem::None => "",
        }
    }

    pub fn build_depends(&self) -> &'static [&'static str] {
        match self {
            BuildSystem::Cargo => &["cargo:native (>= 0.34.0)", "gcc-arm-linux-gnueabihf [armhf]", "gcc-aarch64-linux-gnu [arm64]", "libstd-rust-dev"],
            BuildSystem::Python3 => &["python3-all", "dh-python", "python3-setuptools"],
            BuildSystem::Npm => &["npm"],
            BuildSystem::None => &[],
            BuildSystem::DotNet => &["dotnet-sdk-3.1:native (<< 3.1.300-1) | dotnet-sdk-3.1:native (>= 3.1.301-1)"]
        }
    }
}

pub struct ExecutableSuggestion {
    pub path: String,
    pub is_path_relative: bool,
    pub is_in_destdir: bool,
    pub is_arch_dependent: bool,
    pub skip_debug_symbols: bool,
    pub summary: Option<String>,
    pub long_doc: Option<String>,
    pub csproj: Option<String>,
}

#[derive(Debug, thiserror::Error)]
#[error("failed to check if path `{path}` exists")]
pub struct ExistError {
    #[source]
    error: io::Error,
    path: PathBuf,
}

fn exists<P: AsRef<Path> + Into<PathBuf>>(path: P) -> Result<bool, ExistError> {
    match std::fs::metadata(&path) {
        Ok(_) => Ok(true),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(false),
        Err(error) => Err(ExistError { error, path: path.into(), }),
    }
}

pub fn scan(source_dir_path: &Path) -> Result<BuildSystem, ExistError> {
    let build_system = if exists(source_dir_path.join("Cargo.toml"))? {
        BuildSystem::Cargo
    } else if exists(source_dir_path.join("setup.py"))? {
        BuildSystem::Python3
    } else if exists(source_dir_path.join("package.json"))? {
        BuildSystem::Npm
    } else {
        let mut has_sln = false;
        for file in std::fs::read_dir(&source_dir_path).expect("failed to list source directory") {
            let file = file.expect("Failed to get file in source directory");
            if file.path().extension() == Some("sln".as_ref()) {
                has_sln = true;
                break;
            }
        }

        if has_sln {
            BuildSystem::DotNet
        } else {
            BuildSystem::None
        }
    };

    Ok(build_system)
}
