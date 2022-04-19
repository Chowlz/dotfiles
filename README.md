# dotfiles
My personal dotfiles

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

## Setup homebrew (Mac only):
Used to install the following
- Rectangle
- Spotify
- Emacs 28.1
```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
export PATH="/opt/homebrew/bin:$PATH"
brew install --cask rectangle
brew install --cask spotify
brew tap railwaycat/emacsmacport
brew install emacs-mac
```

## Setup nix darwin (Mac only):
```
curl -L https://nixos.org/nix/install | sh
nix-build https://github.com/LnL7/nix-darwin/archive master.tar.gz -A installer
./result/bin/darwin-installer

# Check build
darwin-rebuild build -I darwin-config=$HOME/.config/nixpkgs/darwin-configuration.nix

# Confirm changes
darwin-rebuild switch-I darwin-config=$HOME/.config/nixpkgs/darwin-configuration.nix

# Remove old .nixpkgs
rm -fr .nixpkgs
```

## Setup home-manager:
```
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-shell '<home-manager>' -A install
```

## Link gui apps (Mac only):
```
ln -s /opt/homebrew/opt/emacs-mac/Emacs.app /Applications
ln -s /run/current-system/Applications/iTerm2.app /Applications
```

## Bootstrap:
```
~/.dotfiles/setup.sh
```
