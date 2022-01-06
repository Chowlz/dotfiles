# dotfiles
My personal dotfiles

## Setup nix:
```
# Install nix package manager

# Add channel for home-manager
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager

# If using macOS:
nix-channel --add https://nixos.org/channels/nixpkgs-20.09-darwin nixpkgs-stable

# Otherwise:
nix-channel --add https://nixos.org/channels/nixpkgs-20.09 nixpkgs-stable

# Update channels
nix-channel --update

# Install home-manager
nix-shell '<home-manager>' -A install
```

## Clone the repo:
```
# Change DOTFILES_DIR/create soft link (ln -s <dotfiles> ~/.dotfiles) if necessary
DOTFILES_DIR=~/.dotfiles

# Clone this repo
git clone git@github.com:Chowlz/dotfiles.git $DOTFILES_DIR

# Ensure that commits for the future are with the correct email
cd $DOTFILES_DIR
git config user.email "mail@charlescruz.dev"
```

## Bootstrap:
```
~/.dotfiles/setup.sh
```

## Fixing emacs on MacOs if version has changed:
- Go to "Applications" in finder
- Remove "Emacs.app"
- Open "Emacs.app" in ~/Applications
- Select "Options" > "Show in Finder" in the dock
- Copy "Emacs.app" from the nix store folder into the Applications folder via drag-and-drop
