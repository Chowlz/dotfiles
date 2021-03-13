# dotfiles
My personal dotfiles

## Setup nix:
```
# Install nix package manager

# Add channels for home-manager and replace nixpkgs alias from nixpkgs-unstable to nixos-unstable
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --add https://nixos.org/channels/nixos-unstable nixpkgs
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

## Setup base and bootstrap:
```
ln -s $DOTFILES_DIR/tag-base/config/nixpkgs ~/.config/nixpkgs
home-manager switch
rm ~/.config/nixpkgs

RCRC=$DOTFILES_DIR/rcrc rcup
$DOTFILES_DIR/tag-base/setup.sh
```
