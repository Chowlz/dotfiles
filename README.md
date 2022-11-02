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

## Bootstrap:
```
~/.dotfiles/setup.sh
```

## Updating nixpkgs on MacOs
```
sudo -i sh -c 'nix-channel --update && nix-env -iA nixpkgs.nix && launchctl remove org.nixos.nix-daemon && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist'
```
