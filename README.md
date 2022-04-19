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
