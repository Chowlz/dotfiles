# dotfiles
My personal dotfiles

## Clone the repo:
```
# Change DOTFILES_DIR and create soft link (ln -s <dotfiles> ~/.dotfiles) if necessary
DOTFILES_DIR=~/.dotfiles
# Clone this repo
git clone git@github.com:Chowlz/dotfiles.git $DOTFILES_DIR
```

## Setup base and bootstrap:
```
RCRC=$DOTFILES_DIR/rcrc rcup
$DOTFILES_DIR/tag-base/setup.sh
```
