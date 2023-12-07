# dotfiles
My personal dotfiles, managed with nix flakes.

## Nix

### Nix-darwin

Need to have a mac again to test ¯\_(ツ)_/¯

### Nixos-wsl

#### Setup

1. Ensure 1Password beta is installed to use the 1Password SSH agent with WSL
2. In PowerShell: `wsl --import NixOS .\NixOS\ nixos-wsl.tar.gz --version 2`
3. In NixOs:
```
sudo nix-channel --update
nix-shell -p git --command \
  "git -c core.sshCommand=ssh.exe clone git@github.com:Chowlz/dotfiles.git ~/.dotfiles"
nix-shell -p git --command \
  "cd ~/.dotfiles && git config user.email \"mail@charlescruz.dev\""
nix-shell -p git --command \
  "cd ~/.dotfiles && git config user.name \"Charles Cruz\""
~/.dotfiles/setup.sh

```
4. In PowerShell: `wsl --terminate NixOS`
5. Enjoy!

#### Updating

```
# From any directory as nixos user
sudo nixos-rebuild switch --flake ~/.dotfiles/config/nixpkgs/.#nixos-wsl
# From ~/.doffiles/config/nixpkgs
sudo nixos-rebuild switch --flake .#nixos-wsl
```

Some changes may require a restart. In a powershell:

```
wsl --terminate NixOs
```

## Common Errors

### No such file or directory for new files

`error: getting status of '/nix/store/*': No such file or directory`

`nixos-rebuild` requires new files/directories are staged/committed in git.

### Insufficient permission for adding an object to repository database .git/objects

```
fatal: cannot create an empty blob in the object databas
error: program 'git' failed with exit code 128
```

A file or folder was commited to git using elevated permissions and now git can't modify those
objects. Recursively force ownership of all files under .git:

`sudo chown -R $(whoami) ~/.dotfiles/.git/*`