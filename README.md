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

### Nix-darwin

Need to have a mac again to test ¯\_(ツ)_/¯

### NixOS-WSL

1. Ensure 1Password beta is installed to use the 1Password SSH agent with WSL

2. In PowerShell: `wsl --import NixOS .\NixOS\ nixos-wsl.tar.gz --version 2`

3. In NixOs:

```
sudo nix-channel --update

nix-shell -p git --command "git -c core.sshCommand=ssh.exe clone git@github.com:Chowlz/dotfiles.git ~/.dotfiles"

nix-shell -p git --command "cd ~/.dotfiles && git config user.email \"mail@charlescruz.dev\""

nix-shell -p git --command "cd ~/.dotfiles && git config user.name \"Charles Cruz\""
```

4. Run the setup script:

```
~/.dotfiles/setup.sh --type nixos --configuration <configuration name>
```

5. In PowerShell: `wsl --terminate NixOS`

6. Enjoy!

#### Updating

```
# From ~/.dotfiles/config/nixpkgs
sudo nixos-rebuild switch --flake path:$(pwd)#nixos-wsl
```

Some changes may require a restart. In a powershell:

```
wsl --terminate NixOS
```

### NixOS-WSL for work

Most likely the VPN at work will require certificate installation updating things with the internet.

1. Start NixOS in one window and start `wsl-vpnkit` as a wsl distro if needed to reach the internet
through the VPN (some VPNs mess up the routes for WSL).

2. Get root certificate into wsl.

3. `sudo -i` and `export NIX_SSL_CERT_FILE=/cert/file/location` in order to `nix-channel --update`.

4. Add certificates as text in an array into `security.pki.certificates` in
`/etc/nixos/configuration.nix`.

5. `sudo nixos-rebuild switch` and then restart wsl with `wsl --terminate NixOS`.

6. Create a work `work` directory to hold work-specific nix configuration. Use `work-ref` as a
reference.

7. Afterwards, follow the above [instructions for setting up NixOS-WSL](#nixos-wsl).

#### Updating

Use `sudo nixos-rebuild switch --flake path:$(pwd)#<configuration name>`.

### Home-manager for work

Run `./setup.sh --type home-manager --configuration <configuration name>` to setup dotfiles only.

## Common Errors

### No such file or directory for new files

`error: getting status of '/nix/store/*': No such file or directory`

`nixos-rebuild` requires new files/directories are staged/committed in git.

### Insufficient permission for adding an object to repository database .git/objects

```
fatal: cannot create an empty blob in the object database
error: program 'git' failed with exit code 128
```

A file or folder was commited to git using elevated permissions and now git can't modify those
objects. Recursively force ownership of all files under .git:

`sudo chown -R $(whoami) ~/.dotfiles/.git/*`