use random_stuff::result::MultilineTerminator;
use random_stuff::error::DisplayError;
use std::io;
use std::fmt;
use std::path::{Path, PathBuf};
use std::collections::{HashMap, HashSet};
use once_cell::sync::Lazy;

struct ChangelogRecord<'a> {
    package_name: &'a str,
    maintainer: &'a str,
    version: &'a str,
    messages: &'a [&'a str],
}

fn write_current_changelog_record<W: io::Write>(mut writer: W, record: &ChangelogRecord) -> io::Result<()> {
    use chrono::{DateTime, Utc};

    let now: DateTime<Utc> = Utc::now();

    writeln!(writer, "{} ({}-1) buster; urgency=medium", record.package_name, record.version)?;
    writeln!(writer)?;
    for message in record.messages {
        writeln!(writer, "  * {}", message)?;
    }
    writeln!(writer)?;
    writeln!(writer, " -- {}  {}", record.maintainer, now.to_rfc2822())?;

    Ok(())
}

#[derive(Debug)]
enum CheckOperation {
    Spawn,
    Wait,
}

impl fmt::Display for CheckOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CheckOperation::Spawn => write!(f, "run `which` command"),
            CheckOperation::Wait => write!(f, "wait for `which` command to exit"),
        }
    }
}

fn exists<P: AsRef<Path> + Into<PathBuf>>(path: P) -> Result<bool, ExistError> {
    match std::fs::metadata(&path) {
        Ok(_) => Ok(true),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(false),
        Err(error) => Err(ExistError { error, path: path.into(), }),
    }
}

#[derive(Debug, thiserror::Error)]
#[error("failed to check if path `{path}` exists")]
struct ExistError {
    #[source]
    error: io::Error,
    path: PathBuf,
}

#[derive(Debug, thiserror::Error)]
#[error("failed to {operation}")]
struct CheckErrorInner {
    #[source]
    error: io::Error,
    operation: CheckOperation,
}

#[derive(Debug, thiserror::Error)]
#[error("failed to check if program `{program}` exists")]
struct CheckError {
    #[source]
    error: CheckErrorInner,
    program: &'static str,
}

fn program_available(name: &'static str) -> Result<bool, CheckError> {
    use std::process::Stdio;

    Ok(std::process::Command::new("which")
        .arg(name)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|error| CheckError { program: name, error: CheckErrorInner { error, operation: CheckOperation::Spawn, }})?
        .wait()
        .map_err(|error| CheckError { program: name, error: CheckErrorInner { error, operation: CheckOperation::Wait, }})?
        .success())
}

fn mandatory_dep(packages: &mut Vec<&'static str>, program_name: &'static str, package_name: &'static str) -> Result<(), CheckError> {
    if !program_available(program_name)? {
        packages.push(package_name);
    }
    Ok(())
}

#[derive(Debug)]
struct DisplayDeps<T: AsRef<[&'static str]>>(T);

impl<T: AsRef<[&'static str]>> fmt::Display for DisplayDeps<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut zeroth = true;
        for dep in self.0.as_ref() {
            if !zeroth {
                write!(f, " ")?;
            } else {
                zeroth = false;
            }
            write!(f, "{}", dep)?;
        }
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
enum InstallErrorInner {
    #[error("failed to run apt-get")]
    Spawn(#[source] io::Error),
    #[error("failed to wait for apt-get to exit")]
    Wait(#[source] io::Error),
    #[error("apt-get failed with exit status {0}")]
    Apt(std::process::ExitStatus),
}

#[derive(Debug, thiserror::Error)]
#[error("failed to install dependencies {deps}")]
struct InstallError {
    deps: DisplayDeps<Vec<&'static str>>,
    #[source]
    error: InstallErrorInner,
}

fn install_deps(deps: Vec<&'static str>, sudo_command: &str) -> Result<(), InstallError> {
    let spawned = std::process::Command::new(sudo_command)
        .arg("apt-get")
        .arg("install")
        .args(&deps)
        .spawn();

    let mut child = match spawned {
        Ok(child) => child,
        Err(error) => return Err(InstallError { deps: DisplayDeps(deps), error: InstallErrorInner::Spawn(error), }),
    };
    match child.wait() {
        Ok(status) if status.success() => Ok(()),
        Ok(status) => Err(InstallError { deps: DisplayDeps(deps), error: InstallErrorInner::Apt(status), }),
        Err(error) => Err(InstallError { deps: DisplayDeps(deps), error: InstallErrorInner::Wait(error), }),
    }
}

fn package_exists(package_name: &str) -> bool {
    let result = std::process::Command::new("apt-cache")
        .arg("show")
        .arg(package_name)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()
        .expect("failed to run apt-cache show")
        .wait()
        .expect("failed to wait for apt-cache show to finish");

    result.success()
}

fn read_unique_package_name(readline: &mut rustyline::Editor<()>, our_packages: &mut HashSet<String>) -> String {
    loop {
        let package_name = readline.readline("Please enter the name of the package: ").unwrap();
        readline.add_history_entry(&package_name);
        if package_exists(&package_name) || our_packages.contains(&package_name) {
            println!("Package {} already exists, the name must be unique", package_name);
        } else {
            our_packages.insert(package_name.clone());
            break package_name;
        }
    }
}

#[derive(Eq, PartialEq)]
enum DownloadMethod {
    Git,
    Tarball,
}

enum DownloadMethodWithTemplate {
    Git { tag_template: String },
    Tarball,
}

enum GitVerify {
    Tag,
    Commit,
}

enum TarballVerify {
    Manifest { manifest_url: String, signature_url: String, },
    Signature { signature_url: String, },
}

enum DownloadAndVerify {
    Git { tag_template: String, verify: Option<GitVerify>, },
    Tarball { verify: Option<TarballVerify>, },
}

enum BuildSystem {
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
    fn identifier(&self) -> &str {
        match self {
            BuildSystem::Cargo => "cargo",
            BuildSystem::Python3 => "python3",
            BuildSystem::DotNet => "dotnet",
            BuildSystem::Npm => "npm",
            BuildSystem::None => "none",
        }
    }

    fn suggest_executables(&self, source_dir: &Path) -> Vec<ExecutableSuggestion> {
        match self {
            BuildSystem::Cargo => cargo_suggest_executables(source_dir),
            BuildSystem::None => search_built_executables(source_dir),
            BuildSystem::DotNet => dotnet_suggest_executables(source_dir),
            _ => Vec::new(),
        }
    }

    fn dependencies_hint(&self) -> &str {
        match self {
            BuildSystem::Cargo => "",
            BuildSystem::Python3 => " or system-wide python packages",
            BuildSystem::DotNet => "",
            BuildSystem::Npm => "",
            BuildSystem::None => "",
        }
    }
}

struct ExecutableSuggestion {
    path: String,
    is_path_relative: bool,
    is_arch_dependent: bool,
    skip_debug_symbols: bool,
    csproj: Option<String>,
}

fn cargo_suggest_executables(source_dir: &Path) -> Vec<ExecutableSuggestion> {
    let manifest = cargo_toml::Manifest::from_path(source_dir.join("Cargo.toml"))
        .expect("Failed to load Cargo.toml");
    match (manifest.package, manifest.workspace) {
        (Some(package), None) => {
            let package_name = package.name;
            manifest.bin
                .into_iter()
                .map(|bin| ExecutableSuggestion {
                    path: bin.name.unwrap_or_else(|| package_name.to_owned()),
                    is_path_relative: false,
                    is_arch_dependent: true,
                    skip_debug_symbols: false,
                    csproj: None,
                })
                .collect()
        },
        (None, Some(workspace)) => {
            workspace.members
                .iter()
                .flat_map(|member| cargo_suggest_executables(&source_dir.join(member)))
                .collect()
        },
        (Some(_), Some(_)) => panic!("WTF, Cargo.toml contains both package and workspace"),
        (None, None) => panic!("WTF, Cargo.toml contains neither package nor workspace"),
    }
}

fn deserialize_ignore_any<'de, D: serde::Deserializer<'de>>(deserializer: D) -> Result<(), D::Error> {
    use serde::Deserialize;

    serde::de::IgnoredAny::deserialize(deserializer)?;
    Ok(())
}

fn dotnet_suggest_executables(source_dir: &Path) -> Vec<ExecutableSuggestion> {
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

                        if is_exe {
                            let suggestion = ExecutableSuggestion {
                                path: bin_name.to_owned(),
                                is_path_relative: true,
                                is_arch_dependent: true,
                                skip_debug_symbols: true,
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

fn search_built_executables(source_dir: &Path) -> Vec<ExecutableSuggestion> {
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
                        csproj: None,
                    }
                } else {
                    ExecutableSuggestion {
                        path,
                        is_path_relative: true,
                        is_arch_dependent: false,
                        skip_debug_symbols: false,
                        csproj: None,
                    }
                };

                executables.push(suggestion);
            }
        }
    }
    executables
}

enum Architecture {
    Any,
    All,
}

impl Architecture {
    fn dir_name(&self) -> &str {
        match self {
            Architecture::Any => "lib",
            Architecture::All => "share",
        }
    }

    fn debian_id(&self) -> &str {
        match self {
            Architecture::Any => "any",
            Architecture::All => "all",
        }
    }
}

static EMPTY_MAP_VARIANTS: Lazy<MapVariants> = Lazy::new(|| MapVariants::new());

struct SharedPackage {
    name: String,
    summary: String,
    architecture: Architecture,
    files: Vec<String>,
    extra_groups: HashMap<String, ExtraGroup>,
}

impl SharedPackage {
    fn as_package(&self) -> Package<'_> {
        Package {
            name: &self.name,
            summary: &self.summary,
            architecture: Some(self.architecture.debian_id()),
            add_files: &self.files,
            add_links: &[],
            bin_package: None,
            binary: None,
            conf_param: None,
            conf_d: None,
            user: None,
            depends: &[],
            recommends: &[],
            databases: Databases {
                pgsql: None,
                mysql: None,
            },
            map_variants: &EMPTY_MAP_VARIANTS,
            extra_groups: &self.extra_groups,
            config: Default::default(),
        }
    }
}

struct ExecutablePackage {
    name: String,
    summary: String,
    architecture: Architecture,
    dependencies: Vec<String>,
    files: Vec<String>,
    links: Vec<String>,
    recommends: Vec<&'static str>,
    extra_groups: HashMap<String, ExtraGroup>,
}

impl ExecutablePackage {
    fn as_package(&self) -> Package<'_> {
        Package {
            name: &self.name,
            summary: &self.summary,
            architecture: Some(self.architecture.debian_id()),
            add_files: &self.files,
            add_links: &self.links,
            bin_package: None,
            binary: None,
            conf_param: None,
            conf_d: None,
            user: None,
            depends: &self.dependencies,
            recommends: &self.recommends,
            databases: Databases {
                pgsql: None,
                mysql: None,
            },
            map_variants: &EMPTY_MAP_VARIANTS,
            extra_groups: &self.extra_groups,
            config: Default::default(),
        }
    }
}

#[derive(serde_derive::Serialize)]
struct ExtraGroup {
    create: bool,
}

fn deref_neg(x: &bool) -> bool {
    !*x
}

#[derive(serde_derive::Serialize)]
struct InternalVar {
    #[serde(rename = "type")]
    ty: &'static str,
    summary: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    default: Option<String>,
    priority: &'static str,
    #[serde(skip_serializing_if = "Clone::clone")]
    store: bool,
    #[serde(skip_serializing_if = "deref_neg")]
    ignore_empty: bool,
}

#[derive(serde_derive::Serialize)]
struct ExternalVar {
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
    #[serde(skip_serializing_if = "Clone::clone")]
    store: bool,
    #[serde(skip_serializing_if = "deref_neg")]
    ignore_empty: bool,
}

#[derive(serde_derive::Serialize)]
struct HiddenVar {
    #[serde(rename = "type")]
    ty: &'static str,
    #[serde(skip_serializing_if = "Clone::clone")]
    store: bool,
    #[serde(skip_serializing_if = "deref_neg")]
    ignore_empty: bool,
    #[serde(flatten)]
    val: HiddenVarVal,
}

#[derive(serde_derive::Serialize)]
#[serde(rename_all = "snake_case")]
enum HiddenVarVal {
    Constant(String),
    //Script(String),
    Template(String),
}

struct MapHttp {
    default_mainnet_port: String,
    default_regtest_port: String,
    default_mainnet_root_path: String,
    default_regtest_root_path: String,
}

struct ServicePackage {
    name: String,
    summary: String,
    executable: String,
    executable_package: String,
    service_type: ServiceType,
    depends: Vec<String>,
    recommends: Vec<&'static str>,
    config_input: Option<ConfigInput>,
    config: Config,
    main_conf_file_name: String,
    database: Option<(DatabaseType, String)>,
    extra_groups: HashMap<String, ExtraGroup>,
    map_variants: MapVariants,
}

impl ServicePackage {
    fn as_package(&self) -> Package<'_> {
        let (pgsql, mysql) = match &self.database {
            Some((DatabaseType::PostgreSQL, template)) => (Some(DatabaseSpec { template, }), None),
            Some((DatabaseType::MySQL, template)) => (None, Some(DatabaseSpec { template, })),
            None => (None, None),
        };

        let mut config = HashMap::with_capacity(1);
        config.insert(&*self.main_conf_file_name, &self.config);

        Package {
            name: &self.name,
            summary: &self.summary,
            architecture: None,
            add_files: &[],
            add_links: &[],
            bin_package: Some(&self.executable_package),
            binary: Some(&self.executable),
            conf_param: self.config_input.as_ref().and_then(|conf| conf.as_file_param()),
            conf_d: self.config_input.as_ref().and_then(|conf| conf.as_conf_d()),
            user: Some(UserSpec {
                group: true,
                create: CreateUser {
                    home: true,
                }
            }),
            depends: &self.depends,
            recommends: &self.recommends,
            databases: Databases {
                pgsql,
                mysql,
            },
            map_variants: &self.map_variants,
            extra_groups: &self.extra_groups,
            config,
        }
    }
}

#[derive(Eq, PartialEq, serde_derive::Serialize)]
#[serde(rename_all = "snake_case")]
enum ConfFormat {
    Plain,
    Toml,
    Yaml,
    Json,
    #[serde(rename = "plain")]
    Env,
}

enum ConfigInput {
    File(String),
    Dir(String),
}

impl ConfigInput {
    fn as_file_param(&self) -> Option<&str> {
        match self {
            ConfigInput::File(param) => Some(param),
            ConfigInput::Dir(_) => None,
        }
    }

    fn as_conf_d(&self) -> Option<ConfD<'_>> {
        match self {
            ConfigInput::File(_) => None,
            ConfigInput::Dir(param) => Some(ConfD {
                name: "conf.d",
                param: &param,
            }),
        }
    }
}

enum ServiceType {
    Simple,
    Forking,
    Notify,
    Dbus,
}

enum LndMacaroon {
    Admin,
    InvoiceAndReadOnly,
    Invoice,
    ReadOnly,
}

enum LndRpcProto {
    Grpc,
    Rest,
}

impl LndRpcProto {
    fn port_var_name(&self) -> &str {
        match self {
            LndRpcProto::Grpc => "grpc_port",
            LndRpcProto::Rest => "rest_port",
        }
    }
}

enum DatabaseType {
    PostgreSQL,
    MySQL,
}

enum DefaultVal {
    Single(String),
    PerNetwork(String, String),
}

impl DefaultVal {
    fn template(&self, name: &str) -> String {
        match self {
            DefaultVal::Single(val) => val.clone(),
            DefaultVal::PerNetwork(_, _) => name.to_owned(),
        }
    }
}

fn read_default_port(readline: &mut rustyline::Editor<()>, no_variants: bool) -> rustyline::Result<DefaultVal> {
    if no_variants {
        Ok(DefaultVal::Single(read_port(readline, "Please enter the default listening port number: ")?))
    } else {
        let mainnet_default = read_port(readline, "Please enter the default listening port number for mainnet instance: ")?;
        loop {
            let regtest_default = read_port(readline, "Please enter the default listening port number for regtest instance - must be different from mainnet: ")?;
            if mainnet_default != regtest_default {
                break Ok(DefaultVal::PerNetwork(mainnet_default, regtest_default));
            }

            println!("The port numbers must be different for each network!");
        }
    }
}

fn read_yes_no(readline: &mut rustyline::Editor<()>, message: &str) -> rustyline::Result<bool> {
    Ok(loop {
        match &*readline.readline(message)? {
            "y" => break true,
            "n" => break false,
            unknown => println!("I can't understand your answer: {}", unknown),
        }
    })
}

fn read_yes_no_custom<T>(readline: &mut rustyline::Editor<()>, message: &str, yes: T, no: T) -> rustyline::Result<T> {
    Ok(loop {
        match &*readline.readline(message)? {
            "y" => break yes,
            "n" => break no,
            unknown => println!("I can't understand your answer: {}", unknown),
        }
    })
}

fn read_port(readline: &mut rustyline::Editor<()>, message: &str) -> rustyline::Result<String> {
    Ok(loop {
        let port = readline.readline(message)?;
        readline.add_history_entry(&port);
        match port.parse::<u16>() {
            Ok(0) => println!("Port must not be 0"),
            Ok(_) => break port,
            Err(error) => println!("invalid port number {}: {}", port, error.join_sources(": ")),
        }
    })
}

fn read_root_path(readline: &mut rustyline::Editor<()>, message: &str) -> rustyline::Result<String> {
    Ok(loop {
        let root_path = readline.readline(message)?;
        readline.add_history_entry(&root_path);
        if root_path.starts_with('/') && root_path.len() >= 2 {
            break root_path;
        }

        println!("Root path must be non-empty and begin with /");
    })
}

#[derive(serde_derive::Serialize)]
struct BuildRules<'a> {
    pkg_name_upper: &'a str,
    pkg_name_snake: &'a str,
    source_name: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    clone_url: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    git_tag: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    fingerprint: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    verify_tag: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    verify_commit: Option<bool>,
    build_system: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    unpack: Option<Unpack<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    shasums: Option<ShaSums<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    dotnet_csproj: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    dotnet_build_name: Option<String>,
}

#[derive(serde_derive::Serialize)]
struct Unpack<'a> {
    url: &'a str,
    file_name: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    rename: Option<&'a str>,
}

#[derive(serde_derive::Serialize)]
struct ShaSums<'a> {
    url: &'a str,
    detached_sig: &'a str,
}

#[derive(serde_derive::Serialize)]
struct SourcePackage<'a> {
    name: &'a str,
    section: &'a str,
    #[serde(skip_serializing_if = "<[&str]>::is_empty")]
    variants: &'a [&'a str],
    build_depends: &'a [&'a str],
    packages: &'a [&'a str],
    #[serde(skip_serializing_if = "deref_neg")]
    skip_debug_symbols: bool,
}

#[derive(serde_derive::Serialize)]
struct Config {
    format: ConfFormat,
    ivars: HashMap<String, InternalVar>,
    evars: HashMap<String, HashMap<String, ExternalVar>>,
    hvars: HashMap<String, HiddenVar>,
}

#[derive(serde_derive::Serialize)]
struct Package<'a> {
    name: &'a str,
    summary: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    architecture: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    bin_package: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    binary: Option<&'a str>,
    #[serde(skip_serializing_if = "<[String]>::is_empty")]
    depends: &'a [String],
    #[serde(skip_serializing_if = "<[&str]>::is_empty")]
    recommends: &'a [&'a str],
    #[serde(skip_serializing_if = "<[String]>::is_empty")]
    add_files: &'a [String],
    #[serde(skip_serializing_if = "<[String]>::is_empty")]
    add_links: &'a [String],
    #[serde(skip_serializing_if = "Option::is_none")]
    conf_param: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    conf_d: Option<ConfD<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    user: Option<UserSpec>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    extra_groups: &'a HashMap<String, ExtraGroup>,
    #[serde(skip_serializing_if = "Databases::is_none")]
    databases: Databases<'a>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    map_variants: &'a MapVariants,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    config: HashMap<&'a str, &'a Config>,
}

type MapVariants = HashMap<String, MapVar>;

#[derive(serde_derive::Serialize)]
struct MapVar {
    mainnet: String,
    regtest: String,
}

#[derive(serde_derive::Serialize)]
struct UserSpec {
    group: bool,
    create: CreateUser,
}

#[derive(serde_derive::Serialize)]
struct CreateUser {
    home: bool,
}

#[derive(serde_derive::Serialize)]
struct ConfD<'a> {
    name: &'a str,
    param: &'a str,
}

#[derive(serde_derive::Serialize)]
struct Databases<'a> {
    #[serde(skip_serializing_if = "Option::is_none")]
    pgsql: Option<DatabaseSpec<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    mysql: Option<DatabaseSpec<'a>>,
}

impl<'a> Databases<'a> {
    fn is_none(&self) -> bool {
        self.pgsql.is_none() && self.mysql.is_none()
    }
}

#[derive(serde_derive::Serialize)]
struct DatabaseSpec<'a> {
    template: &'a str,
}

fn add_cargo_bin_to_path(previous_path: Option<std::ffi::OsString>) {
    let mut home: PathBuf = std::env::var_os("HOME").unwrap_or_else(|| {
        println!("HOME not set - unable to add ~/.cargo/env to environment variable PATH");
        std::process::exit(1);
    })
    .into();
    home.push(".cargo/bin");

    let val = match previous_path {
        Some(mut path) => {
            path.reserve(home.as_os_str().len() + 1);
            path.push(":");
            path.push(home);
            path
        },
        None => home.into(),
    };
    std::env::set_var("PATH", val);
}

fn cargo_install(args: &[&str]) -> io::Result<()> {
    let status = std::process::Command::new("cargo")
        .arg("install")
        .arg("-f")
        .args(args)
        .spawn()?
        .wait()?;

    if !status.success() {
        panic!("cargo failed with exit status {}", status);
    }
    Ok(())
}

fn clear_and_show_text(text: &str) {
    let height = terminal_size::terminal_size().map(|(_, height)| height.0).unwrap_or(24);
    let lines_count = text.lines().count();
    let scroll_lines = usize::from(height).saturating_sub(lines_count);
    for _ in 0..scroll_lines {
        println!();
    }
    for line in text.lines() {
        println!("{}", line.trim());
    }
}

struct HistoryAutoSave {
    readline: rustyline::Editor::<()>,
    save_path: PathBuf,
}

impl HistoryAutoSave {
    fn load(mut readline: rustyline::Editor::<()>, path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        match readline.load_history(&path) {
            Ok(()) => (),
            Err(rustyline::error::ReadlineError::Io(error)) if error.kind() == std::io::ErrorKind::NotFound => (),
            Err(error) => panic!("Can't read history {}", error),
        }

        HistoryAutoSave {
            readline,
            save_path: path,
        }
    }
}

impl Drop for HistoryAutoSave {
    fn drop(&mut self) {
        let dir = self.save_path.parent().unwrap_or("/".as_ref());
        if let Err(error) = std::fs::create_dir_all(&dir) {
            eprintln!("Failed to create directory {}: {}", dir.display(), error);
        }
        if let Err(error) = self.readline.save_history(&self.save_path) {
            eprintln!("Failed to save history to file {}: {}", self.save_path.display(), error);
        }
    }
}

impl std::ops::Deref for HistoryAutoSave {
    type Target = rustyline::Editor::<()>;

    fn deref<'a>(&'a self) -> &'a Self::Target {
        &self.readline
    }
}

impl std::ops::DerefMut for HistoryAutoSave {
    fn deref_mut<'a>(&'a mut self) -> &'a mut Self::Target {
        &mut self.readline
    }
}

fn atomic_write<R, E: From<io::Error>, F: FnOnce(&mut std::fs::File) -> Result<R, E>>(path: &str, fun: F) -> Result<R, E> {
    use std::io::Write;

    let tmp_path = format!("{}.tmp", path);
    let mut file = std::fs::File::create(&tmp_path)?;

    let result = fun(&mut file)?;

    file.flush()?;
    file.sync_data()?;
    drop(file);

    std::fs::rename(&tmp_path, path)?;

    Ok(result)
}

#[derive(Debug)]
enum SerError<T: std::error::Error + 'static> {
    Io(io::Error),
    Ser(T),
}

impl<T: std::error::Error + 'static> fmt::Display for SerError<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SerError::Io(_) => write!(f, "failed to write"),
            SerError::Ser(_) => write!(f, "failed to serialize"),
        }
    }
}

impl<T: std::error::Error + 'static> std::error::Error for SerError<T> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            SerError::Io(error) => Some(error),
            SerError::Ser(error) => Some(error),
        }
    }
}

impl<T: std::error::Error + 'static> From<io::Error> for SerError<T> {
    fn from(value: io::Error) -> Self {
        SerError::Io(value)
    }
}

impl From<toml::ser::Error> for SerError<toml::ser::Error> {
    fn from(value: toml::ser::Error) -> Self {
        SerError::Ser(value)
    }
}

impl From<serde_yaml::Error> for SerError<serde_yaml::Error> {
    fn from(value: serde_yaml::Error) -> Self {
        SerError::Ser(value)
    }
}

fn atomic_serialize_toml<T: serde::Serialize>(path: &str, data: T) -> Result<(), SerError<toml::ser::Error>> {
    use std::io::Write;

    atomic_write::<_, SerError<_>, _>(path, |file| {
        let toml_bytes = toml::to_vec(&data)?;
        file.write_all(&toml_bytes)?;
        Ok(())
    })
}

fn main() -> MultilineTerminator {
    let mut save_path = dirs_next::data_dir().expect("can't determine data dir");
    save_path.push("cadr-guide/history");

    let readline_config = rustyline::config::Builder::new()
        .max_history_size(10000)
        //.auto_add_history(true)
        .build();
    let readline = rustyline::Editor::<()>::with_config(readline_config);
    let mut readline = HistoryAutoSave::load(readline, save_path);
    if !program_available("apt-get")? {
        eprintln!("apt-get NOT present, are you running this on Debian?");
        // Pointless to continue
        std::process::exit(1);
    }

    clear_and_show_text("
    Welcome to Cryptoanarchy Debian Repository packaging guide!
    This is an interactive program that will help you with steps
    needed to add a new package.
    
    In this version you will be adding the package directly to the
    repository. Detached packages will be supported in the future.
    
    Please keep in mind this version is experimental! Nothing horrible
    should happen, some things just may not work.
    
    So, let's get started. I'm going to scan your system now to check
    If you have all required dependencies.
    ");

    let mut deps = Vec::new();

    mandatory_dep(&mut deps, "git", "git")?;
    mandatory_dep(&mut deps, "make", "make")?;
    mandatory_dep(&mut deps, "wget", "wget")?;
    mandatory_dep(&mut deps, "grep", "grep")?;
    mandatory_dep(&mut deps, "mustache", "ruby-mustache")?;
    mandatory_dep(&mut deps, "sha256sum", "coreutils")?;
    mandatory_dep(&mut deps, "gpg", "gnupg")?;
    mandatory_dep(&mut deps, "tar", "tar")?;
    mandatory_dep(&mut deps, "dpkg-buildpackage", "devscripts")?;

    let has_debcrafter = program_available("debcrafter")?;
    let has_cfg_me = program_available("cfg_me")?;

    let (debcrafter_needs_install, cfg_me_needs_install) = if !has_debcrafter || !has_cfg_me {
        use std::env::VarError;

        match std::env::var("PATH") {
            Ok(path) if path.contains("/.cargo/bin") => (!has_debcrafter, !has_cfg_me),
            Ok(path) => {
                add_cargo_bin_to_path(Some(path.into()));
                (!program_available("debcrafter")?, !program_available("cfg_me")?)
            },
            Err(VarError::NotPresent) => {
                add_cargo_bin_to_path(None);
                (!program_available("debcrafter")?, !program_available("cfg_me")?)
            },
            Err(VarError::NotUnicode(_)) => {
                println!("Your PATH environment variable contains non-UTF-8 characters. This is currently unsupported.");
                println!("Feel free to submit a PR!");
                std::process::exit(1);
            },
        }
    } else {
        (false, false)
    };
    if debcrafter_needs_install || cfg_me_needs_install {
        let info = match (debcrafter_needs_install, cfg_me_needs_install) {
            (true, true) => "Debcrafter and cfg_me are",
            (true, false) => "Debcrafter is",
            (false, true) => "cfg_me is",
            (false, false) => unreachable!("we're in an if that checks this"),
        };
        println!("{} NOT installed, I will make sure to add appropriate dependencies.", info);
        mandatory_dep(&mut deps, "cargo", "cargo")?;
    }

    if !deps.is_empty() {
        let sudo_command = match (program_available("sudo")?, program_available("doas")?) {
            (_, true) => "doas",
            (true, false) => "sudo",
            (false, false) => {
                println!("You have neither sudo nor doas installed, can't install the dependencies.");
                println!("Please install them as root using apt install {}", DisplayDeps(&deps));
                println!("Then re-run this program");
                std::process::exit(1);
            },
        };

        println!("I'm going to install the dependencies using {} apt-get install {}", sudo_command, DisplayDeps(&deps));
        readline.readline("Hit Enter to continue or Ctrl+C to abort: ")?;
        println!();

        install_deps(deps, sudo_command)?;
        println!();
        println!();
    }

    if debcrafter_needs_install || cfg_me_needs_install {
        let info = match (debcrafter_needs_install, cfg_me_needs_install) {
            (true, true) => "debcrafter and cfg_me",
            (true, false) => "debcrafter",
            (false, true) => "cfg_me",
            (false, false) => unreachable!("we're in an if that checks this"),
        };
        println!("I'm going to install {} now.", info);
        println!();
        println!("WARNING: THIS IS NOT GPG-VERIFIED YET!");
        println!("On top of trusting me, you are also trusting GitHub/crates.io to not compromise the source code");
        println!("and certificate authorities to not issue malicious certificate and MITM the connection.");
        println!();
        println!("Feel free to send a PR to fix this!");
        println!("Do you want to continue?");
        println!();
        readline.readline("Hit Enter to continue or Ctrl+C to abort: ")?;

        if debcrafter_needs_install {
            cargo_install(&["--git", "https://github.com/Kixunil/debcrafter"])?;
        }
        if cfg_me_needs_install {
            cargo_install(&["cfg_me"])?;
        }
    }

    if !Path::new("build_rules").is_dir() || !Path::new("pkg_specs").is_dir() {
        println!("You don't seem to be in the directory containing Cryptoanarchy Debian Repository Builder");
        println!("Please cd into it and re-run this program. If you want to try this out without modifying");
        println!("actual repository, create build_rules and pkg_specs directories in a temporary directory");
        println!("and cd into that temporary directory.");
        std::process::exit(1);
    }

    let maintainer = match std::env::var("DEBEMAIL") {
        Ok(maintainer) => maintainer,
        Err(std::env::VarError::NotUnicode(_)) => panic!("DEBEMAIL is not UTF-8"),
        Err(std::env::VarError::NotPresent) => {
            clear_and_show_text("
                Who are you?
                
                You need to enter your name and e-mail address in this format:
                Satoshi Nakamoto <satoshi@bitcoin.org>
                
                This will be used in changelog to identify who is responsible for the package.
                You can set DEBEMAIL variable to avoid this question here and in other programs
                next time.
            ");

            let maintainer = readline.readline("Please enter your name and e-mail address: ")?;
            readline.add_history_entry(&maintainer);
            maintainer
        },
    };

    println!("What is the name of your source package? Valid names may only contain lower-case letters, and dashes.");
    let source_package_name = readline.readline("Please enter the package name: ")?;
    readline.add_history_entry(&source_package_name);
    println!();

    println!("You have to specify the version number of the application you want to package.");
    println!("The version number can only contain numbers separated by dots.");
    println!("Do NOT prefix it with v or something, do NOT add suffixes like -beta.");

    let version = readline.readline("Please enter the version number: ")?;
    readline.add_history_entry(&version);
    println!();

    println!("How do you want to download the source code of your application?");
    println!("The available methods are:");
    println!();
    println!("1. git clone");
    println!("2. wget and unpack the tarball");
    println!();
    let method = loop {
        let line = readline.readline("Please select a method to use (enter the number): ")?;
        match &*line {
            "1" => break DownloadMethod::Git,
            "2" => break DownloadMethod::Tarball,
            unknown => println!("Unknown method {}", unknown),
        }
    };
    println!();

    if method == DownloadMethod::Tarball {
        println!("In case of tarball download, the URL(s) must contain the version number.");
        println!("This allows very easy upgrades. All existing software has version number in the URL.");
        println!("Please replace the version number with ${{VERSION}} in all relevant places in the URL.");
    }

    let url = loop {
        let url = readline.readline("Please enter the URL of the source code: ")?;
        readline.add_history_entry(&url);
        if method == DownloadMethod::Git || url.contains("${VERSION}") {
            break url;
        }

        println!("${{VERSION}} is missing in the URL");
    };
    println!();

    let method = match method {
        DownloadMethod::Git => {
            println!("You need to specify template for naming tags. Tag names must contain the version.");
            println!("Replace the version in tag with  ${{VERSION}}. The most popular tag template is");
            println!(" v${{VERSION}} so it is the default if you leave the template empty.");

            let tag_template = loop {
                let template = readline.readline("Please enter the tag template: ")?;
                if template.is_empty() {
                    break "v${VERSION}".to_owned();
                }
                readline.add_history_entry(&template);
                if template.contains("${VERSION}") {
                    break template;
                }
                println!("${{VERSION}} is missing in the tag template");
            };
            println!();

            DownloadMethodWithTemplate::Git { tag_template, }
        },
        DownloadMethod::Tarball => {
            if !url.ends_with(".tar.gz") && !url.ends_with(".tar.bz2") && !url.ends_with(".tar.xz") {
                println!("Error: unknown tarball extension. Supported extensions are: ");
                println!(".tar.gz");
                println!(".tar.bz2");
                println!(".tar.xz");
                std::process::exit(1);
            }
            DownloadMethodWithTemplate::Tarball
        },
    };

    println!("It's highly recommended that you verify the source code using GPG.");
    println!("This program can do it automatically for you, just enter the fingerprint of GPG key.");
    println!("Leave the line blank to skip verification - NOT recommended for production!");
    let fingerprint = readline.readline("Please enter the PGP fingerprint or hit Enter: ")?;
    readline.add_history_entry(&fingerprint);
    println!();
    let download_and_verify = match (method, fingerprint.is_empty()) {
        (DownloadMethodWithTemplate::Git { tag_template }, true) => DownloadAndVerify::Git { tag_template, verify: None, },
        (DownloadMethodWithTemplate::Tarball, true) => DownloadAndVerify::Tarball { verify: None, },
        (DownloadMethodWithTemplate::Git { tag_template }, false) => {
            println!("How do you want to verify signature?");
            println!();
            println!("1. verify-tag (prefer if possible)");
            println!("2. verify-commit");
            println!();
            let verify = loop {
                let line = readline.readline("Please select a verification method to use (enter the number): ")?;
                match &*line {
                    "1" => break GitVerify::Tag,
                    "2" => break GitVerify::Commit,
                    unknown => println!("Unknown verification method {}", unknown),
                }
            };
            println!();
            DownloadAndVerify::Git { tag_template, verify: Some(verify), }
        },
        (DownloadMethodWithTemplate::Tarball, false) => {
            println!("There are two ways tarballs can be verified:");
            println!();
            println!("1. Directly");
            println!("2. Using manifest");
            println!();
            println!("Direct verification means there is a signature file available that signs the tarball itself.");
            println!("If the tarball is named foo.tar.gz then signature file is usually named foo.tar.gz.asc");
            println!("or foo.tar.gz.sig. Manifest verification method works by having a separate file containing");
            println!("SHA256 sums of the released files (tarballs), and a separate signature file that signs the");
            println!("SHA256 sums file instead.");
            println!();

            let signature_url = loop {
                let url = readline.readline("Please enter the URL of signature file (direct or one signing the manifest): ")?;
                readline.add_history_entry(&url);
                if url.contains("${VERSION}") {
                    break url;
                }

                println!("${{VERSION}} is missing in the URL");
            };
            println!();
            println!("If you want to use manifest verification you need to enter its URL.");

            let manifest_url = loop {
                let url = readline.readline("Enter manifest URL or leave blank for direct verification: ")?;
                if url.is_empty() {
                    break url;
                }
                readline.add_history_entry(&url);
                if url.contains("${VERSION}") {
                    break url;
                }

                println!("${{VERSION}} is missing in the URL");
            };
            println!();

            let verify = if manifest_url.is_empty() {
                TarballVerify::Signature { signature_url, }
            } else {
                TarballVerify::Manifest { manifest_url, signature_url, }
            };

            DownloadAndVerify::Tarball { verify: Some(verify), }
        },
    };

    let tmp_dir = mktemp::Temp::new_dir().expect("Failed to create temporary directory");
    let mut keyring = PathBuf::new();
    println!("Created temporary working directory {}", tmp_dir.display());
    if !fingerprint.is_empty() {
        println!("Downloading gpg key");
        keyring = tmp_dir.join(&fingerprint);
        std::fs::create_dir_all(&keyring)?;
        let status = std::process::Command::new("gpg")
            .arg("--keyserver")
            .arg("hkp://keyserver.ubuntu.com")
            .arg("--recv-keys")
            .arg(&fingerprint)
            .env("GNUPGHOME", &keyring)
            .spawn()
            .expect("failed to run gpg --recv-keys")
            .wait()
            .expect("failed to wait for gpg --recv-keys to exit");
            if !status.success() {
                panic!("GNUPGHOME={} gpg --keyserver hkp://keyserver.ubuntu.com --recv-keys {} failed with exit status {}", keyring.display(), fingerprint, status);
            }
    }
    println!("Downloading source");
    let source_dir_path = match &download_and_verify {
        DownloadAndVerify::Git { tag_template, verify } => {
            let source_dir_path = tmp_dir.join("source");
            let tag = tag_template.replace("${VERSION}", &version);
            let status = std::process::Command::new("git")
                .arg("clone")
                .arg("-b")
                .arg(&tag)
                .arg(&url)
                .arg(&source_dir_path)
                .spawn()
                .expect("Failed to run git clone")
                .wait()
                .expect("Failed to wait for git clone to exit");
            if !status.success() {
                panic!("git clone -b {} {} {} failed with exit status {}", tag, url, source_dir_path.display(), status);
            }

            match verify {
                Some(GitVerify::Tag) => {
                    println!("Running GNUPGHOME={} git verify-tag {}", keyring.display(), tag);

                    let status = std::process::Command::new("git")
                        .arg("verify-tag")
                        .arg(&tag)
                        .current_dir(&source_dir_path)
                        .env("GNUPGHOME", &keyring)
                        .spawn()
                        .expect("Failed to run git verify-tag")
                        .wait()
                        .expect("Failed to wait for git verify-tag to exit");
                    if !status.success() {
                        panic!("git verify-tag {} failed with exit status {}", tag, status);
                    }
                },
                Some(GitVerify::Commit) => {
                    let output = std::process::Command::new("git")
                        .arg("rev-list")
                        .arg("-n")
                        .arg("1")
                        .arg(&tag)
                        .current_dir(&source_dir_path)
                        .output()
                        .expect("Failed to run git rev-list");
                    if !output.status.success() {
                        panic!("git verify-tag {} failed with exit status {}", tag, status);
                    }
                    let mut commit = String::from_utf8(output.stdout).expect("failed to decode output of git rev-list as UTF-8");
                    if commit.ends_with('\n') {
                        commit.pop();
                    }
                    let status = std::process::Command::new("git")
                        .arg("verify-commit")
                        .arg(&commit)
                        .current_dir(&source_dir_path)
                        .env("GNUPGHOME", &keyring)
                        .spawn()
                        .expect("Failed to run git verify-commit")
                        .wait()
                        .expect("Failed to wait for git verify-commit to exit");
                    if !status.success() {
                        panic!("git verify-commit {} failed with exit status {}", commit, status);
                    }
                },
                None => (),
            }

            source_dir_path
        },
        DownloadAndVerify::Tarball { verify, } => {
            let url = url.replace("${VERSION}", &version);
            let archive_name_pos = url.rfind('/').expect("Invalid URL: missing /") + 1;
            let archive_name = &url[archive_name_pos..];

            let status = std::process::Command::new("wget")
                .arg(&url)
                .current_dir(&tmp_dir)
                .spawn()
                .expect("Failed to run wget")
                .wait()
                .expect("Failed to wait for wget to exit");
            if !status.success() {
                panic!("wget {} failed with exit status {}", url, status);
            }

            if let Some(TarballVerify::Manifest { manifest_url, .. }) = &verify {
                let manifest_url = manifest_url.replace("${VERSION}", &version);
                let manifest_name_pos = manifest_url.rfind('/').expect("Invalid URL: missing /") + 1;
                let manifest_name = &manifest_url[manifest_name_pos..];

                let status = std::process::Command::new("wget")
                    .arg(&manifest_url)
                    .current_dir(&tmp_dir)
                    .spawn()
                    .expect("Failed to run wget")
                    .wait()
                    .expect("Failed to wait for wget to exit");
                if !status.success() {
                    panic!("wget {} failed with exit status {}", manifest_url, status);
                }

                let regex = format!("^[a-z0-9]{{64}} {}$", archive_name);
                let mut grep = std::process::Command::new("grep")
                    .arg("-E")
                    .arg(&regex)
                    .arg(&manifest_name)
                    .current_dir(&tmp_dir)
                    .stdout(std::process::Stdio::piped())
                    .spawn()
                    .expect("Failed to run wget");
                let sha256sum = std::process::Command::new("sha256sum")
                    .arg("-c")
                    .arg("-")
                    .current_dir(&tmp_dir)
                    .stdin(grep.stdout.take().expect("std Command API is retarded"))
                    .spawn()
                    .expect("Failed to run sha256sum")
                    .wait()
                    .expect("Failed to wait for sha256sum to exit");
                if !sha256sum.success() {
                    panic!("sha256sum -c - failed with exit status {}", status);
                }
                let grep = grep
                    .wait()
                    .expect("Failed to wait for grep to exit");
                if !grep.success() {
                    panic!("grep -E {} {} failed with exit status {}", regex, manifest_name, status);
                }
            }

            match verify {
                Some(TarballVerify::Manifest { signature_url, .. }) | Some(TarballVerify::Signature { signature_url }) => {
                    let signature_url = signature_url.replace("${VERSION}", &version);
                    let signature_name_pos = url.rfind('/').expect("Invalid URL: missing /") + 1;
                    let signature_name = &url[signature_name_pos..];
                    let status = std::process::Command::new("wget")
                        .arg(&signature_url)
                        .current_dir(&tmp_dir)
                        .spawn()
                        .expect("Failed to run wget")
                        .wait()
                        .expect("Failed to wait for wget to exit");
                    if !status.success() {
                        panic!("wget {} failed with exit status {}", signature_url, status);
                    }

                    let status = std::process::Command::new("gpgv")
                        .arg(&signature_name)
                        .env("GNUPGHOME", &keyring)
                        .current_dir(&tmp_dir)
                        .spawn()
                        .expect("Failed to run gpgv")
                        .wait()
                        .expect("Failed to wait for gpgv to exit");
                    if !status.success() {
                        panic!("gpgv {} failed with exit status {}", signature_name, status);
                    }
                },
                None => (),
            }

            println!("Unpacking {}", archive_name);
            let opt = if archive_name.ends_with(".tar.gz") {
                "xzf"
            } else if archive_name.ends_with(".tar.bz2") {
                "xjf"
            } else if archive_name.ends_with(".tar.xz") {
                "xJf"
            } else {
                unreachable!();
            };


            let status = std::process::Command::new("tar")
                .arg(opt)
                .arg(&archive_name)
                .current_dir(&tmp_dir)
                .spawn()
                .expect("Failed to run tar")
                .wait()
                .expect("Failed to wait for tar to exit");
            if !status.success() {
                panic!("tar {} {} failed with exit status {}", opt, archive_name, status);
            }

            let mut source_dir_path = None;
            for file in std::fs::read_dir(&tmp_dir).expect("failed to list temporary directory") {
                let file = file.expect("failed to get file in temporary directory");
                if file.metadata().expect("Failed to get metadata of the file").is_dir() {
                    source_dir_path = Some(file.path());
                }
            }
            source_dir_path.expect("source code contains no directory")
        },
    };

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

    println!("Detected build system: {}", build_system);
    if let BuildSystem::None = build_system {
        readline.readline("Hit Enter to continue or Ctrl+C to abort: ")?;
    }
    clear_and_show_text("
    Great progress! Most formalities should be resolved, we will focus on the meat now.
    Before you begin, you need to take a while to think about what parts the application
    is made of. Does it contain multiple executables? Libraries or data shared between them?
    Are they independent or is it pointless to have one without the other?
    
    Example: bitcoin package contains bitcoind and bitcoin-cli binaries. bitcoin-cli can
    connect to a remote server so it does NOT require bitcoind. bitcoind can be controlled
    from various other applications like btc-rpc-explorer, so it doesn't require bitcoin-cli.
    As another example, selfhost-dashboard has a binary and also shared data package. The binary
    will not work without data package and data package is pointless without binary package.
    
    Also, please note that there are two kinds of packages: architecture-dependent and
    architecture-independent. Architecture-dependent packages can work only on correct CPU.
    Native binaries and .so libraries are always architecture-dependent. Python scripts,
    images, HTML documents... are architecture-independent.
    But please be careful: sometimes an application written in architecture-independent
    language, such as Python, may contain native libraries and is thus architecture-dependent!
    When in doubt better assume architecture-dependent - it will cause less damage if guessed
    wrong.
    
    How things relate in the application you're trying to package?
    
    Please take a minute to think about this.
    
    
    Once you figured it out, let's add shared packages - libraries or data. Enter \"a\" (without
    quotes) to add a new shared package. Enter \"c\" to finish adding shared packages and proceed to
    next step.
    ");

    let mut shared_packages = Vec::new();
    let mut executable_packages = Vec::new();
    let mut services = Vec::new();
    let mut our_packages = HashSet::new();
    let mut dotnet_csproj = None;
    let mut dotnet_build_name = None;

    loop {
        println!();
        let action = readline.readline("What do you want to do? (a - add shared package, c - continue): ")?;
        if action == "c" {
            break;
        }
        if action != "a" {
            println!("Unknown action {}, please try again", action);
            continue;
        }

        println!();
        println!("Please note the naming conventions for shared packages.");
        match build_system {
            BuildSystem::Python3 => {
                println!("Python packages always start with python3-. E.g. python3-lnpbp-testkit");
            },
            _ => {
                println!("Library packages begin with lib prefix. E.g. librocksdb. Data packages end with -data or -common.");
                println!("E.g. selfhost-dashboard-data, nginx-common");
            },
        }
        let package_name = read_unique_package_name(&mut readline, &mut our_packages);
        let summary = readline.readline("Please write a short (one-line) description of the package: ")?;
        readline.add_history_entry(&summary);
        let architecture = read_yes_no_custom(&mut readline, "Is this package architecture-dependent? y for yes, n for no: ", Architecture::Any, Architecture::All)?;

        println!();
        println!("Now let's add the files that will be created after build to the package.");
        println!("All paths you enter will be relative to the build directory containing source code of");
        println!("your package and they will be placed into /usr/{}/{}/ when the package is installed.", architecture.dir_name(), package_name);
        println!("You can also add whole directories.");

        let mut files = Vec::new();
        loop {
            let file = readline.readline("Enter a file to add or leave blank to finish adding files: ")?;
            if file.is_empty() {
                break;
            }
            readline.add_history_entry(&file);
            files.push(file);
        }

        println!();
        println!("This should be enough for a shared package in most cases. You can tweak it manually if");
        println!("there's something special.");
        println!();
        println!("Done adding shared package {}", package_name);
        println!();

        shared_packages.push(SharedPackage {
            summary,
            name: package_name,
            architecture,
            files,
            extra_groups: Default::default(),
        });
    }
    clear_and_show_text("
    Now we're going to add packages containing executables. Those can be run from command line
    or provide a service. They are usually the most important kind of package. Enter \"a\"
    (without quotes) to add a new executable package. Enter \"c\" to finish adding executable
    packages and proceed to next step.
    ");
    let mut skip_debug_symbols = false;
    let mut suggested_service_binaries = Vec::new();
    let mut suggested_executables = build_system.suggest_executables(&source_dir_path);
    loop {
        println!();
        let action = readline.readline("What do you want to do? (a - add executable package, c - continue): ")?;
        if action == "c" {
            break;
        }
        if action != "a" {
            println!("Unknown action {}, please try again", action);
            continue;
        }

        let mut links = Vec::new();

        let package_name = read_unique_package_name(&mut readline, &mut our_packages);
        let summary = readline.readline("Please write a short (one-line) description of the package: ")?;
        readline.add_history_entry(&summary);
        println!();
        let suggested_executables_was_empty = suggested_executables.is_empty();

        let mut was_arch_dep = false;
        if suggested_executables.is_empty() {
            println!("I couldn't find any executable automatically, you will have to fill it manually.");
        } else {
            println!("I have found some executables automatically. Do you want to add them?");
        }

        let mut files = Vec::new();
        let mut exec_count = 0usize;
        while !suggested_executables.is_empty() {
            println!();
            for (i, ExecutableSuggestion { path, .. }) in suggested_executables.iter().enumerate() {
                println!(" {}. {}", i + 1, path);
            }
            println!();
            let choice = readline.readline("Enter the number of executable or leave the line empty to continue: ")?;
            if choice.is_empty() {
                break;
            }
            let choice = match choice.parse::<usize>() {
                Ok(choice) => choice,
                Err(error) => {
                    println!("{} is not an integer ({}), try again", choice, error);
                    continue;
                },
            };
            if choice < 1 || choice > suggested_executables.len() {
                println!("number {} out of range", choice);
                continue;
            }
            let choice = choice - 1;

            let executable = suggested_executables.remove(choice);
            was_arch_dep |= executable.is_arch_dependent;
            skip_debug_symbols |= executable.skip_debug_symbols;

            let path = match executable.csproj {
                Some(csproj) => {
                    assert!(dotnet_csproj.is_none(), "Oops, we don't support multiple dotnet executables yet");

                    let dest_path = if executable.path.contains(|c| c >= 'A' && c <= 'Z') {
                        let lower = executable.path.to_ascii_lowercase();

                        println!();
                        println!("The executable has non-standard name - it contains upper case letters.");
                        println!("Executables usually contain lower-case letters and dashes only");
                        println!("I suggest to rename it to {}", lower);
                        println!();
                        let new_name = readline.readline("Enter the new name for executable or leave empty to accept suggestion: ")?;
                        if new_name.is_empty() {
                            lower
                        } else {
                            new_name
                        }
                    } else {
                        executable.path.clone()
                    };

                    links.push(format!("/usr/lib/{}/{} /usr/bin/{}", package_name, executable.path, dest_path));
                    dotnet_csproj = Some(csproj);
                    dotnet_build_name = Some(package_name.clone());
                    files.push(format!("/usr/lib/{}", package_name));
                    dest_path
                },
                None => {
                    let filespec = if executable.is_path_relative {
                        format!("{} /usr/bin", executable.path)
                    } else {
                        format!("/usr/bin/{}", executable.path)
                    };

                    files.push(filespec);

                    executable.path
                },
            };

            println!();
            println!("Does the executable ({}) provide a long-running background service?", path);
            println!("An example of such service is bitcoind - it runs in the background all the time.");
            let service = read_yes_no(&mut readline, "Is this executable a service? y for yes, n for no: ")?;

            if service {
                suggested_service_binaries.push((path.clone(), package_name.clone()));
                println!();
                println!("OK, we will get back to configuring specifics of a service later.");
                println!("Let's finish this step first.");
            }

            exec_count += 1;
        }

        if !suggested_executables_was_empty {
            println!();
            println!("Do you want to add more executables that were not found automatically?");
        }

        let mut has_manual = false;
        loop {
            println!();
            let executable = readline.readline("Enter the path to the executable or leave the line empty to continue: ")?;
            if executable.is_empty() {
                break;
            }
            readline.add_history_entry(&executable);
            println!();
            println!("Does the executable ({}) provide a long-running background service?", executable);
            println!("An example of such service is bitcoind - it runs in the background all the time.");
            let service = read_yes_no(&mut readline, "Is this executable a service? y for yes, n for no: ")?;

            if service {
                suggested_service_binaries.push((executable.clone(), package_name.clone()));
                println!();
                println!("OK, we will get back to configuring specifics of a service later.");
                println!("Let's finish this step first.");
            }

            if executable.starts_with("/usr/bin/") {
                files.push(executable);
            } else {
                files.push(format!("{} /usr/bin", executable));
            };
            exec_count += 1;
            has_manual = true;
        }

        println!();
        println!("If there are additional files required by the executable{} you can add them now.", if exec_count == 1 { "" } else { "s" });
        println!("All paths you enter will be relative to the build directory containing source code of");
        println!("your package.");

        loop {
            let file = readline.readline("Enter a file to add or leave blank to finish adding files: ")?;
            if file.is_empty() {
                break;
            }
            readline.add_history_entry(&file);
            files.push(file);
        }

        let architecture = match (was_arch_dep, has_manual, files.is_empty()) {
            (false, true, false) => {
                read_yes_no_custom(&mut readline, "Was any of the executables or files you added architecture-dependent? y for yes, n for no: ", Architecture::Any, Architecture::All)?
            },
            (false, true, true) => {
                read_yes_no_custom(&mut readline, "Was any of the executables you added architecture-dependent? y for yes, n for no: ", Architecture::Any, Architecture::All)?
            },
            (false, false, false) => {
                read_yes_no_custom(&mut readline, "Was any of the files you added architecture-dependent? y for yes, n for no: ", Architecture::Any, Architecture::All)?
            },
            (false, false, true) => {
                Architecture::All
            },
            (true, _, _) => {
                Architecture::Any
            },
        };

        println!();

        let or_your_own = if shared_packages.is_empty() {
            ""
        } else {
            " or your own packages"
        };

        if exec_count == 1 {
            // TODO: when completion is implemented: note that tab completion works
            println!("Does the executable depend on system packages{}? You can add them now.", or_your_own);
        } else {
            println!("Do the executables depend on system packages{}? You can add them now.", or_your_own);
        }
        println!("Don't worry about system native libraries{} - they will be auto-detected.", build_system.dependencies_hint());
        println!("You can specify minimum version like this: foo (>= 1.0)");
        println!("or maximum version (open range) like this: foo (<< 2.0)");
        println!("If you need both min and max, enter the dependency twice.");
        println!("Alternative dependencies can be specified too: foo | bar");
        if !shared_packages.is_empty() {
            println!();
            println!("Don't forget you have these shared packages:");
            println!();
            for package in &shared_packages {
                println!(" {}", package.name);
            }
            println!();
            println!("You can add them too.");
        }

        let mut dependencies = Vec::new();
        loop {
            let dependency = readline.readline("Enter dependency or leave empty to continue: ")?;
            if dependency.is_empty() {
                break;
            }
            readline.add_history_entry(&dependency);
            dependencies.push(dependency);
        }

        println!();
        println!("This should be enough for an executable package in most cases. You can tweak it manually later if");
        println!("there's something special.");
        println!();
        println!("Done adding executable package {}", package_name);
        println!();

        executable_packages.push(ExecutablePackage {
            name: package_name,
            summary,
            architecture,
            files,
            links,
            dependencies,
            // TODO: fill
            recommends: Default::default(),
            extra_groups: Default::default(),
        });
    }

    let mut has_variants = false;
    if !suggested_service_binaries.is_empty() {
        for (executable, executable_package) in suggested_service_binaries {
            let mut map_variants = HashMap::new();
            let mut recommends = Vec::new();
            let mut depends = Vec::new();
            let mut ivars = HashMap::new();
            let mut evars = <HashMap<_, HashMap<_, _>>>::new();
            let mut hvars = HashMap::new();

            clear_and_show_text("");
            println!("We will setup a *service* for executable {} now.", executable);
            println!();

            println!("The usual naming convention of service packages is executable-system");
            println!("So that would be {}-system in your case but you may choose another name.", executable_package);
            println!("Obviously, the name must not conflict with any other Debian package (official or our).");
            println!("Please note that -mainnet and -regtest suffixes are auto-generated so do NOT enter them now.");
            // TODO default
            let mut package_name = read_unique_package_name(&mut readline, &mut our_packages);
            let summary = readline.readline("Please write a short (one-line) description of the service: ")?;
            readline.add_history_entry(&summary);

            println!();
            println!("Each service needs a main configuration file. They are usually named executable.conf");
            println!("So that would be {}.conf in your case so it is the default but you may choose another name.", executable);
            let main_conf_file_name = {
                let conf_file = readline.readline("Please enter the name of the main configuration file or leave empty to accept default: ")?;
                if conf_file.is_empty() {
                    format!("{}.conf", executable)
                } else {
                    readline.add_history_entry(&conf_file);
                    conf_file
                }
            };

            println!();
            println!("What kind of configuration format does this executable use? Supported formats are:");
            println!();
            println!(" 1. plain - key=value format, as found in e.g. bitcoind, no ecaping, newlines not allowed in values");
            println!(" 2. toml");
            println!(" 3. yaml");
            println!(" 4. json");
            println!(" 5. environment variables - like plain, but it'll be parsed and passed in environment");
            println!();
            println!("Please use environment variables only if the application doesn't support anything else");

            let format = loop {
                let line = readline.readline("Please select a configuration format to use (enter the number): ")?;
                match &*line {
                    "1" => break ConfFormat::Plain,
                    "2" => break ConfFormat::Toml,
                    "3" => break ConfFormat::Yaml,
                    "4" => break ConfFormat::Json,
                    "5" => break ConfFormat::Env,
                    unknown => println!("Unknown configuration format {}", unknown),
                }
            };

            let conf_d = if format != ConfFormat::Env {
                println!();
                println!("Some applications support loading configuration from directories. In such case each configuration");
                println!("file from the directory (sometimes filtered by an extension) is loaded. This can make it easier");
                println!("to manage the configuration. The directory names often end with .d to indicate such case.");
                println!();
                println!("Does this executable support such configuration with a command line switch to select the direcotry?");

                let conf_d = loop {
                    match &*readline.readline("y for yes, n for no: ")? {
                        "y" => break true,
                        "n" => break false,
                        unknown => println!("I can't understand your answer: {}", unknown),
                    }
                };

                if conf_d {
                    let conf_d_param = readline.readline("Please enter the command line switch needed for setting the configuration direcotry: ")?;
                    readline.add_history_entry(&conf_d_param);
                    Some(conf_d_param)
                } else {
                    None
                }
            } else {
                None
            };

            let config_input = match conf_d {
                Some(conf_d) => Some(ConfigInput::Dir(conf_d)),
                None if format == ConfFormat::Env => None,
                None => {
                    let conf_param = readline.readline("Please enter the command line switch needed for setting the configuration file: ")?;
                    readline.add_history_entry(&conf_param);
                    Some(ConfigInput::File(conf_param))
                }
            };

            println!();
            println!("Some services fork (daemonize) themselves to the background when they start, some are configurable.");
            println!("It is highly recommended to NOT fork but maybe this executable doesn't support it?");
            println!();
            let always_forks = read_yes_no(&mut readline, "Does this executable ALWAYS fork? y for yes, n for no: ")?;

            let service_type = if !always_forks {
                println!();
                println!("Some services are able to notify systemd that they started, some aren't. There are various means to do that:");
                println!();
                println!("1. none (unsupported)");
                println!("2. using sd_notify() function");
                println!("3. using dbus");
                println!();
                loop {
                    match &*readline.readline("Notification method supported by this executable: ")? {
                        "1" => break ServiceType::Simple,
                        "2" => break ServiceType::Notify,
                        "3" => break ServiceType::Dbus,
                        unknown => println!("I can't understand your answer: {}", unknown),
                    }
                }
            } else {
                ServiceType::Forking
            };

            let uses_bitcoin = read_yes_no(&mut readline, "Does this application work with Bitcoin network (directly or indirectly e.g. via lightning)?  y for yes, n for no: ")?;

            let no_variants = if uses_bitcoin {
                println!();
                println!("Can a SINGLE RUNNING INSTANCE of this service handle multiple bitcoin networks (mainnet, regtest...)?");
                println!("Your application MUST support at least mainnet and regtest anyway, the question is not about whether");
                println!("two running services can support different networks. As an example, bitcoind can only support one per");
                println!("service but a single RTL (Ride the Lightning) instance supports multiple nodes with different networks.");
                println!("In other words, does your application resemble RTL in this sense?");
                println!();

                read_yes_no(&mut readline, "Does this service work with multiple networks at the same time? y for yes, n for no: ")?
            } else {
                true
            };

            if !no_variants {
                println!();
                println!("OK, I will generate these packages for you: {}-mainnet, {}-regtest", package_name, package_name);
            }

            let mut extra_groups = HashMap::new();

            if uses_bitcoin {
                println!();
                println!("Some applications connect to bitcoind directly some indirectly - e.g. via LND.");
                println!();
                let needs_bitcoind = read_yes_no(&mut readline, "Does this service connect to bitcoind DIRECTLY? y for yes, n for no: ")?;

                let needs_bitcoind_full_access = if needs_bitcoind {
                    println!();
                    println!("Most applications using bitcoind only need a few harmless RPC methods to work. It is best if they");
                    println!("don't need to send dangerous requests such as invalidateblock or wallet commands but some of them");
                    println!("legitimately need higher privileges.");
                    println!();
                    let full_acess = read_yes_no(&mut readline, "Does this service need full access to bitcoind? y for yes, n for no: ")?;

                    println!();
                    let (port_var_name, port_package) = if full_acess {
                        println!("Applications using full access to bitcoind are required to use cookie authentication.");
                        let cookie_path_conf_key = readline.readline("Enter the name (key) of the field used in configuration *file* to set the cookie path: ")?;
                        hvars.insert(cookie_path_conf_key, HiddenVar { ty: "string", store: true, ignore_empty: false, val: HiddenVarVal::Template("/var/run/bitcoin-{variant}/cookie".to_owned()), });
                        ("rpcport", "bitcoin-@variant")
                    } else {
                        println!("Applications using read-only access to bitcoind are required to use user:password authentication.");
                        println!("If you need cookie authentication please file an issue or create a file containing public:public");
                        println!("and use that one.");

                        let join_user_pass = read_yes_no(&mut readline, "Does this executable require user:password in a single field in configuration file? y for yes, n for no: ")?;
                        if join_user_pass {
                            let user_pass = readline.readline("Enter the name (key) of the field in configuration file to set the user name and password: ")?;
                            readline.add_history_entry(&user_pass);
                            hvars.insert(user_pass, HiddenVar { ty: "string", store: true, ignore_empty: false, val: HiddenVarVal::Constant("public:public".to_owned()), });
                        } else {
                            let user = readline.readline("Enter the name (key) of the field in configuration file to set the user name: ")?;
                            readline.add_history_entry(&user);
                            let password = readline.readline("Enter the name (key) of the field in configuration file to set the password: ")?;
                            readline.add_history_entry(&password);
                            hvars.insert(user, HiddenVar { ty: "string", store: true, ignore_empty: false, val: HiddenVarVal::Constant("public".to_owned()), });
                            hvars.insert(password, HiddenVar { ty: "string", store: true, ignore_empty: false, val: HiddenVarVal::Constant("public".to_owned()), });
                        }
                        ("bind_port", "bitcoin-rpc-proxy-@variant")
                    };

                    let join_host_port = read_yes_no(&mut readline, "Does this service require host and port to be specified in a SINGLE OPTION as host:port (that means NOT two options - one for each)? y for yes, n for no: ")?;
                    if join_host_port {
                        let host_port_key = readline.readline("Enter the name (key) of the field in configuration file to set host:port of bitcoind: ")?;
                        readline.add_history_entry(&host_port_key);
                        evars.entry(port_package.to_owned()).or_insert_with(Default::default).insert(port_var_name.to_owned(), ExternalVar { store: false, name: None, ignore_empty: false, });
                        hvars.insert(host_port_key, HiddenVar { ty: "string", store: true, ignore_empty: false, val: HiddenVarVal::Template(format!("127.0.0.1:{{{}/{}}}", port_package, port_var_name)), });
                    } else {
                        let host_key = readline.readline("Enter the name (key) of the field in configuration file to set host of bitcoind: ")?;
                        readline.add_history_entry(&host_key);
                        let port_key = readline.readline("Enter the name (key) of the field in configuration file to set port of bitcoind: ")?;
                        readline.add_history_entry(&port_key);
                        evars.entry(port_package.to_owned()).or_insert_with(Default::default).insert(port_var_name.to_owned(), ExternalVar { store: true, name: Some(port_key), ignore_empty: false, });
                        hvars.insert(host_key, HiddenVar { ty: "string", store: true, ignore_empty: false, val: HiddenVarVal::Constant("127.0.0.1".to_owned()), });
                    }

                    full_acess
                } else {
                    false
                };

                if needs_bitcoind_full_access {
                    if !no_variants {
                        extra_groups.insert("bitcoin-{variant}".to_owned(), ExtraGroup { create: false, });
                    } else {
                        unimplemented!("variant-less version of bitcoind connection not implemented");
                    }
                }

                let needs_lnd = read_yes_no(&mut readline, "Does this service connect to lnd? y for yes, n for no: ")?;
                if needs_lnd {
                    println!();
                    println!("There are several different macaroons that you can use. Please select the most restrictive");
                    println!("macaroon to ensure high security. E.g. do NOT use admin macaroon if this service only creates");
                    println!("invoices and doesn't manage the node. The available macaroons are:");
                    println!();
                    println!("1. readonly.macaroon");
                    println!("2. invoice.macaroon");
                    println!("3. invoice+readonly.macaroon (union of previous two)");
                    println!("4. admin.macaroon");

                    let macaroon = loop {
                            match &*readline.readline("Which macaroon does this service need?: ")? {
                            "1" => break LndMacaroon::ReadOnly,
                            "2" => break LndMacaroon::Invoice,
                            "3" => break LndMacaroon::InvoiceAndReadOnly,
                            "4" => break LndMacaroon::Admin,
                            unknown => println!("I can't understand your answer: {}", unknown),
                        }
                    };
                    println!();
                    let macaroon_config = readline.readline("Please enter the name of field in configuration file to set to set macaroon path: ")?;
                    readline.add_history_entry(&macaroon_config);
                    let tls_cert_config = readline.readline("Please enter the name of field in configuration file to set to set TLS cert path: ")?;
                    readline.add_history_entry(&tls_cert_config);
                    println!("There are two possible protocols for connecting to LND:");
                    println!();
                    println!("1. GRPC (better)");
                    println!("2. REST");
                    println!();
                    let lnd_rpc_proto = loop {
                        match &*readline.readline("Please select the protocol used by this service: ")? {
                            "1" => break LndRpcProto::Grpc,
                            "2" => break LndRpcProto::Rest,
                            unknown => println!("I can't understand your answer: {}", unknown),
                        }
                    };

                    let port_package = "lnd-system-@variant";
                    let port_var_name = lnd_rpc_proto.port_var_name();

                    let join_host_port = read_yes_no(&mut readline, "Does this service require host and port to be specified in a SINGLE OPTION as host:port (that means NOT two options - one for each)? y for yes, n for no: ")?;
                    if join_host_port {
                        let host_port_key = readline.readline("Enter the name (key) of the field in configuration file to set host:port of lnd: ")?;
                        readline.add_history_entry(&host_port_key);
                        evars.entry(port_package.to_owned()).or_insert_with(Default::default).insert(port_var_name.to_owned(), ExternalVar { store: false, name: None, ignore_empty: false, });
                        hvars.insert(host_port_key, HiddenVar { ty: "string", store: true, ignore_empty: false, val: HiddenVarVal::Template(format!("127.0.0.1:{{{}/{}}}", port_package, port_var_name)), });
                    } else {
                        let host_key = readline.readline("Enter the name (key) of the field in configuration file to set host of lnd: ")?;
                        readline.add_history_entry(&host_key);
                        let port_key = readline.readline("Enter the name (key) of the field in configuration file to set port of lnd: ")?;
                        readline.add_history_entry(&port_key);
                        evars.entry(port_package.to_owned()).or_insert_with(Default::default).insert(port_var_name.to_owned(), ExternalVar { store: true, name: Some(port_key), ignore_empty: false, });
                        hvars.insert(host_key, HiddenVar { ty: "string", store: true, ignore_empty: false, val: HiddenVarVal::Constant("127.0.0.1".to_owned()), });
                    }

                    if no_variants {
                        unimplemented!("variant-less version of lnd connection not implemented");
                    }
                    match macaroon {
                        LndMacaroon::Admin => {
                            evars.entry("lnd-system-@variant".to_owned()).or_insert_with(Default::default).insert("adminmacaroonpath".to_owned(), ExternalVar { store: true, name: Some(macaroon_config), ignore_empty: false, });
                            extra_groups.insert("lnd-system-{variant}".to_owned(), ExtraGroup { create: false, });
                        },
                        LndMacaroon::InvoiceAndReadOnly => {
                            hvars.insert(macaroon_config, HiddenVar { ty: "string", store: true, ignore_empty: false, val: HiddenVarVal::Template("/var/lib/lnd-system-{variant}/invoice/invoice+readonly.macaroon".to_owned()), });
                            extra_groups.insert("lnd-system-{variant}-invoice".to_owned(), ExtraGroup { create: false, });
                            extra_groups.insert("lnd-system-{variant}-readonly".to_owned(), ExtraGroup { create: false, });
                            depends.push("lnd-genmacaroon-system-{variant}".to_owned());
                        },
                        LndMacaroon::Invoice => {
                            evars.entry("lnd-system-@variant".to_owned()).or_insert_with(Default::default).insert("invoicemacaroonpath".to_owned(), ExternalVar { store: true, name: Some(macaroon_config), ignore_empty: false, });
                            extra_groups.insert("lnd-system-{variant}-invoice".to_owned(), ExtraGroup { create: false, });
                        },
                        LndMacaroon::ReadOnly => {
                            evars.entry("lnd-system-@variant".to_owned()).or_insert_with(Default::default).insert("readonlymacaroonpath".to_owned(), ExternalVar { store: true, name: Some(macaroon_config), ignore_empty: false, });
                            extra_groups.insert("lnd-system-{variant}-readonly".to_owned(), ExtraGroup { create: false, });
                        },
                    }

                    evars.entry("lnd-system-@variant".to_owned()).or_insert_with(Default::default).insert("tlscertpath".to_owned(), ExternalVar { store: true, name: Some(tls_cert_config), ignore_empty: false, });
                }

                if !no_variants {
                    println!();
                    println!("Does the service need to know the type of the network (mainnet/regtest)?");
                    let network_param = readline.readline("Enter the name (key) of the field in configuration file to set network type or leave blank for none: ")?;
                    if !network_param.is_empty() {
                        readline.add_history_entry(&network_param);
                        hvars.insert(network_param, HiddenVar { ty: "string", store: true, ignore_empty: false, val: HiddenVarVal::Template("{variant}".to_owned()), });
                    }
                }
            }

            println!();
            println!("It is possible to integrate the service with a database out-of-the-box. These are the supported options:");
            println!();
            println!("1. None - no database integration");
            println!("2. PostgreSQL");
            println!("3. MySQL - warning, NOT tested!");
            println!();
            println!("If the service supports both databases, please select PostgreSQL.");

            let database = loop {
                match &*readline.readline("Which database does this service use ?: ")? {
                    "1" => break None,
                    "2" => break Some(DatabaseType::PostgreSQL),
                    "3" => break Some(DatabaseType::MySQL),
                    unknown => println!("I can't understand your answer: {}", unknown),
                }
            };

            let database = if let Some(database) = database {
                println!();
                println!("Some services configure their databases using a single string (URI), some use multiple key-value pairs.");
                let db_single_string = read_yes_no(&mut readline, "Does this service configure the database using a single string? y for yes, n for no: ")?;
                Some((database, if db_single_string {
                    println!();
                    println!("You will need to enter a template for database configuration string. All occurences of placeholders");
                    println!("will be replaced by their respective values. The available placeholders are:");
                    println!();
                    println!(" _DBC_DBUSER_ - database username");
                    println!(" _DBC_DBPASS_ - database password");
                    println!(" _DBC_DBSERVER_ - hostname of the database server");
                    println!(" _DBC_DBPORT_ - port of the database server");
                    println!(" _DBC_DBNAME_ - name of the database to connect to");
                    println!();
                    println!("Note that the placeholders need to start and end with underscores as shown above.");

                    let template = readline.readline("Enter the template: ")?;
                    readline.add_history_entry(&template);
                    template
                } else {
                    unimplemented!();
                    /*
                    let db_user = readline.readline("Enter the name (key) of the field in configuration file to set database user: ")?;
                    let db_pass = readline.readline("Enter the name (key) of the field in configuration file to set database password: ")?;
                    let db_host = readline.readline("Enter the name (key) of the field in configuration file to set database hostname: ")?;
                    let db_port = readline.readline("Enter the name (key) of the field in configuration file to set database port: ")?;
                    let db_name = readline.readline("Enter the name (key) of the field in configuration file to set database name: ")?;
                    let db_type = readline.readline("Enter the name (key) of the field in configuration file to set database type (leave empty if not needed): ")?;
                    if !db_type.is_empty() {
                        let db_type_value = readline.readline("Enter the value for database type: ")?;
                    }
                    String::new()
                    */
                }))
            } else {
                None
            };

            let has_web_gui = read_yes_no(&mut readline, "Does this service have a web GUI (like e.g. BTCPayServer)? y for yes, n for no: ")?;
            let provides_http = if has_web_gui {
                true
            } else {
                read_yes_no(&mut readline, "Does this service provide a PUBLIC HTTP interface (API) anyway? y for yes, n for no: ")?
            };
            if provides_http {
                recommends.push("selfhost (>= 1.0)");
                recommends.push("selfhost (<< 2.0)");
                let port = readline.readline("Enter the name (key) of the field in configuration file to set HTTP listen port: ")?;
                readline.add_history_entry(&port);
                println!();
                println!("All HTTP services require configuring a root path - a prefix for all URL paths. E.g. the root path");
                println!("of BTCPayServer is /btcpay so that it can be accessed as example.com/btcpay.");
                let root_path = readline.readline("Enter the name (key) of the field in configuration file to set root path: ")?;
                readline.add_history_entry(&root_path);
            
                let has_sso = if has_web_gui {
                    println!();
                    println!("Some services, such as ThunderHub, provide a simple SSO. It works by backend generating a single-use");
                    println!("secret link that instantly logs in the user when visited.");
                    read_yes_no(&mut readline, "Does this service provide such SSO? y for yes, n for no: ")?
                } else {
                    false
                };
                let default_port = read_default_port(&mut readline, no_variants)?;
                let default_root_path = if no_variants {
                    DefaultVal::Single(read_root_path(&mut readline, "Please enter the default root path: ")?)
                } else {
                    let mainnet_default = read_root_path(&mut readline, "Please enter the default root path for mainnet instance: ")?;
                    loop {
                        let regtest_default = read_root_path(&mut readline, "Please enter the default root path for regtest instance - must be different from mainnet, usually ends with -rt: ")?;
                        if mainnet_default != regtest_default {
                            break DefaultVal::PerNetwork(mainnet_default, regtest_default);
                        }

                        println!("The root paths must be different for each network!");
                    }
                };

                let port_summary = if no_variants {
                    format!("HTTP bind port of service {}", package_name)
                } else {
                    format!("HTTP bind port of service {} - {{variant}}", package_name)
                };

                let root_path_summary = if no_variants {
                    format!("HTTP root path of service {}", package_name)
                } else {
                    format!("HTTP root path of service {} - {{variant}}", package_name)
                };

                ivars.insert(port.clone(), InternalVar { ty: "bind_port", summary: port_summary, default: Some(default_port.template("{default_http_port}")), priority: "low", store: true, ignore_empty: false, });
                ivars.insert(root_path.clone(), InternalVar { ty: "string", summary: root_path_summary, default: Some(default_root_path.template("{default_root_path}")), priority: "medium", store: true, ignore_empty: false, });

                if let (DefaultVal::PerNetwork(port_mainnet, port_regtest), DefaultVal::PerNetwork(root_path_mainnet, root_path_regtest)) = (default_port, default_root_path) {
                    map_variants.insert("default_http_port".to_owned(), MapVar { mainnet: port_mainnet, regtest: port_regtest, });
                    map_variants.insert("default_root_path".to_owned(), MapVar { mainnet: root_path_mainnet, regtest: root_path_regtest, });
                }
            };

            println!();
            println!("Does the service listen on additional ports?");
            loop {
                let port_param = readline.readline("Enter the name (key) of the field in configuration file to set an additional listening port or leave blank for none: ")?;
                if port_param.is_empty() {
                    break;
                }
                readline.add_history_entry(&port_param);
                println!();
                println!("Please write a short, readable description of the port. A good description should contain");
                println!("the name of the service and interface provided. Example: Electrs Json RPC port to listen on");
                println!();
                let summary = readline.readline("Enter summary describing this port: ")?;
                readline.add_history_entry(&summary);
                let default_port = read_default_port(&mut readline, no_variants)?;
                let mut mapping_name = format!("{{default_{}}}", port_param);
                ivars.insert(port_param, InternalVar { ty: "bind_port", summary, default: Some(default_port.template(&mapping_name)), priority: "low", store: true, ignore_empty: false, });
                mapping_name.pop();
                mapping_name.remove(0);
                if let DefaultVal::PerNetwork(mainnet, regtest) = default_port {
                    map_variants.insert(mapping_name, MapVar { mainnet, regtest, });
                }
                println!();
            }

            let executable = if executable.starts_with("/usr/bin/") {
                executable
            } else if let Some(pos) = executable.rfind('/') {
                format!("/usr/bin/{}", &executable[(pos + 1)..])
            } else {
                format!("/usr/bin/{}", executable)
            };

            if !no_variants {
                has_variants = true;
                package_name.push_str("-@variant");
            }

            let config = Config {
                format, 
                ivars,
                evars,
                hvars,
            };

            let service = ServicePackage {
                name: package_name,
                summary,
                executable,
                executable_package,
                service_type,
                depends,
                recommends,
                config,
                main_conf_file_name,
                config_input,
                database,
                extra_groups,
                map_variants,
            };
            services.push(service);
        }
    }
    clear_and_show_text("Generating files...");
    let pkg_name_snake = source_package_name.replace('-', "_");
    let pkg_name_upper = pkg_name_snake.to_ascii_uppercase();

    let mut clone_url = None;
    let mut git_tag = None;
    let fingerprint = if fingerprint.is_empty() { None } else { Some(&*fingerprint) };
    let mut verify_tag = None;
    let mut verify_commit = None;
    let mut shasums = None;
    let mut unpack = None;
    let replaced_url;
    let replaced_signature_url;
    let replaced_manifest_url;

    match &download_and_verify {
        DownloadAndVerify::Git { tag_template, verify } => {
            clone_url = Some(&*url);
            let replacement = format!("$({}_VERSION)", pkg_name_upper);
            git_tag = Some(tag_template.replace("${VERSION}", &replacement));
            match verify {
                Some(GitVerify::Tag) => verify_tag = Some(true),
                Some(GitVerify::Commit) => verify_commit = Some(true),
                None => (),
            }
        },
        DownloadAndVerify::Tarball { verify, } => {
            let replacement = format!("$({}_VERSION)", pkg_name_upper);
            replaced_url = url.replace("${VERSION}", &replacement);
            let url = &*replaced_url;
            unpack = Some(Unpack {
                url,
                file_name: &url[(url.rfind('/').unwrap() + 1)..],
                rename: None,
            });
            match verify {
                Some(TarballVerify::Manifest { manifest_url, signature_url }) => {
                    replaced_signature_url = signature_url.replace("${VERSION}", &replacement);
                    replaced_manifest_url = manifest_url.replace("${VERSION}", &replacement);

                    shasums = Some(ShaSums {
                        url: &replaced_manifest_url,
                        detached_sig: &replaced_signature_url
                    })
                },
                Some(TarballVerify::Signature { signature_url, }) => {},
                None => (),
            }
        },
    }

    let build_rules = BuildRules {
        pkg_name_upper: &pkg_name_upper,
        pkg_name_snake: &pkg_name_snake,
        source_name: &source_package_name,
        clone_url,
        git_tag: git_tag.as_ref().map(AsRef::as_ref),
        fingerprint,
        verify_tag,
        verify_commit,
        build_system: build_system.identifier(),
        unpack,
        shasums,
        dotnet_csproj,
        dotnet_build_name,
    };

    let build_rules_path = format!("build_rules/{}.yaml", source_package_name);
    println!("Generating {} which will contain download and build instructions...", build_rules_path);

    atomic_write::<_, SerError<_>, _>(&build_rules_path, |file| serde_yaml::to_writer(file, &build_rules).map_err(Into::into))?;

    for package in &shared_packages {
        let spec_path = format!("pkg_specs/{}.sps", package.name);
        println!("Generating {} which will contain package specification...", spec_path);

        atomic_serialize_toml(&spec_path, &package.as_package())?;
    }

    for package in &executable_packages {
        let spec_path = format!("pkg_specs/{}.sps", package.name);
        println!("Generating {} which will contain package specification...", spec_path);

        atomic_serialize_toml(&spec_path, &package.as_package())?;
    }

    for package in &services {
        let spec_path = format!("pkg_specs/{}.sps", package.name);
        println!("Generating {} which will contain package specification...", spec_path);

        atomic_serialize_toml(&spec_path, &package.as_package())?;
    }

    let source_spec_path = format!("pkg_specs/{}.sss", source_package_name);
    println!("Generating {} which will contain source specification...", source_spec_path);

    let packages = shared_packages.iter().map(|package| &*package.name)
        .chain(executable_packages.iter().map(|package| &*package.name))
        .chain(services.iter().map(|package| &*package.name))
        .collect::<Vec<_>>();

    let variants = if has_variants {
        &["mainnet", "regtest"] as &[_]
    } else {
        &[]
    };

    let build_depends: &[_] = match build_system {
        BuildSystem::Cargo => &["cargo:native (>= 0.34.0)", "gcc-arm-linux-gnueabihf [armhf]", "gcc-aarch64-linux-gnu [arm64]", "libstd-rust-dev"],
        BuildSystem::Python3 => &["python3-all", "dh-python", "python3-setuptools"],
        BuildSystem::Npm => &["npm"],
        BuildSystem::None => &[],
        BuildSystem::DotNet => &["dotnet-sdk-3.1:native (<< 3.1.300-1) | dotnet-sdk-3.1:native (>= 3.1.301-1)"]
    };

    let source_spec = SourcePackage {
        name: &source_package_name,
        section: "net",
        variants,
        build_depends,
        packages: &packages,
        skip_debug_symbols,
    };

    atomic_serialize_toml(&source_spec_path, &source_spec)?;

    let record = ChangelogRecord {
        package_name: &source_package_name,
        maintainer: &maintainer,
        version: &version,
        messages: &["Created package"],
    };

    let changelog_path = format!("pkg_specs/{}.changelog", source_package_name);
    println!("Generating {} which will contain changelog...", changelog_path);

    atomic_write(&changelog_path, |file| {
        write_current_changelog_record(file, &record)
    })?;

    println!("Done! Try build it now!");
    println!();

    Ok(())
}
