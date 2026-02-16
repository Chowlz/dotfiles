# dotfiles

My personal dotfiles, managed with nix flakes.

## Configurations

### Personal
* nixos:
  * `nixos-wsl`

### Work (defined by `work-ref`)
* nixos:
  * `laptop`
* home-manager:
  * `ubuntu`

## Setup

### Home-manager (for personal or work)

1.  In the VM/linux machine, pull this repo, `cd` into it

1.  (Work-only) Create a `work` directory to hold work-specific nix configuration. Use `work-ref` as
    a reference.

1.  At the project root directory, run:

    ```bash
    ./setup home-manager --configuration <configuration name>
    ```

### Nix-darwin

1.  Install nix from determinate systems

1.  In a terminal, run:

    ```bash
    ./setup nix-darwin
    ```

### NixOS-WSL

1.  Ensure 1Password beta is installed to use the 1Password SSH agent with WSL

1.  In PowerShell, run:

    ```powershell
    wsl --import NixOS .\NixOS\ nixos-wsl.tar.gz --version 2
    ```

1.  In NixOs, pull this repo, `cd` into it, and run:

    ```bash
    sudo nix-channel --update
    nix-shell -p git --command "git -c core.sshCommand=ssh.exe clone git@github.com:Chowlz/dotfiles.git ~/.dotfiles"
    nix-shell -p git --command "cd ~/.dotfiles && git config user.email \"mail@charlescruz.dev\""
    nix-shell -p git --command "cd ~/.dotfiles && git config user.name \"Charles Cruz\""
    ./setup nixos --configuration <configuration name>
    ```

1.  Shutdown NixOS and restart it in PowerShell, if desired:

    ```powershell
    wsl --terminate NixOS
    # Optional
    wsl -d NixOS
    ```

#### Updating

```bash
./setup nixos --configuration <configuration name>
```

Some changes may require a restart. In a powershell:

```bash
wsl --terminate NixOS
```

### NixOS-WSL for work

Most likely the VPN at work will require certificate installation updating things with the internet.

1.  Start NixOS in one window and start `wsl-vpnkit` as a wsl distro if needed to reach the internet
    through the VPN (some VPNs mess up the routes for WSL).

1.  Get corporate root certificates into wsl.

1.  In NixOS, run:

    ```bash
    sudo -i
    $ export NIX_SSL_CERT_FILE=/cert/file/location
    $ nix-channel --update
    ```

1.  Add certificates as text in an array into `security.pki.certificates` in
    `/etc/nixos/configuration.nix`.

1.  In NixOS, run:

    ```bash
    sudo nixos-rebuild switch
    ```

1.  Shutdown NixOS and restart it in PowerShell, if desired:

    ```powershell
    wsl --terminate NixOS
    # Optional
    wsl -d NixOS
    ```

1.  Create a `work` directory to hold work-specific nix configuration. Use `work-ref` as a
    reference.

1.  Afterwards, follow the above [instructions for setting up NixOS-WSL](#nixos-wsl) and updating
    it when needed.

## Common Errors

### No such file or directory for new files

```bash
error: getting status of '/nix/store/*': No such file or directory
```

`nixos-rebuild` requires new files/directories are staged/committed in git.

### Insufficient permission for adding an object to repository database .git/objects

```bash
fatal: cannot create an empty blob in the object database
error: program 'git' failed with exit code 128
```

A file or folder was commited to git using elevated permissions and now git can't modify those
objects. Recursively force ownership of all files under .git:

```bash
sudo chown -R $(whoami) ~/.dotfiles/.git/*
```
