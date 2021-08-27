# Cryptoanarchy Debian Repository packaging guide

WIP interactive step-by-step tutorial/guide/wizard that walks you through the packaging process.

## About

[Cryptoanarchy Debian Repository](https://github.com/debian-cryptoanarchy/cryptoanarchy-deb-repo-builder)
achieves high packaging quality thanks to use of [debcrafter](https://github.com/Kixunil/debcrafter) and
the declarative approach but the downside is steep learning curve.
This project aims to fix that by walking you through the process step-by-step with explanations,
focusing at one topic at a time.
The result is prepared skeleton for your package with important details filled in.

Note that this is WIP, so many things are unimplemented and details in the files are missing.
However, it should do ~80% of the work.
Manually fixing the rest should be quick and easy.
If you're stuck feel free to create a PR from the state of your repository and I will try to help you out.

## Usage

### Pre-usage checklist

To avoid disappointment you should make sure these things are true before you begin:

- [ ] You have Debian 10 Buster system available (VM is OK)
- [ ] Proper newer version of Rust is required. 1.48 in Bullseye will work, default in Buster need to be [updated](https://rustup.rs/).
- [ ] All services of the project support configuration via file (one of: plain key=value, toml, yaml, json) or environment variables.
- [ ] Path to config file is NOT hard-coded and can be specified on command line.
- [ ] If the service provides public HTTP API (as a webapp or headless server) root path (prefix to go at the beginning of each path)
      can be configured.
- [ ] All bind ports are configurable.
- [ ] The source code or built binaries can be downloaded using `git clone` or by getting a tarball.
- [ ] If the service interacts with bitcoin directly or indirectly it supports `regtest`.
- [ ] If the service connects to `bitcoind` it supports cookie files and also simple user & password authentication; the port can be configured.
- [ ] If the service connects to `lnd` then port, macaroon and TLS certificate can be configured.

### Compiling the guide

0. You need Rust to compile this. Just `apt install cargo`.
1. Make sure to set `PATH=$PATH:~/.cargo/bin`.
2. `git clone https://github.com/debian-cryptoanarchy/cadr-guide`
3. `cd cadr-guide`
4. `cargo install --locked --path .` (notice the dot at the end).

### Using the guide

0. Make sure to set `PATH=$PATH:~/.cargo/bin`.
1. `git clone https://github.com/debian-cryptoanarchy/cryptoanarchy-deb-repo-builder`
2. `cd cryptoanarchy-deb-repo-builder`
3. `cadr-guide`
4. Follow the instructions on the screen.

### Upgrading the guide

0. Go back to this project directory and run `git pull origin master`.
1. `cargo install -f --locked --path .`
