{ lib, pkgs, config, ... }:

with lib;
let
    cfg = config.modules.neovim;
in {
  options.modules.neovim = {
    enable = mkEnableOption "neovim";

    package = mkOption {
      type = types.nullOr types.package;
      default = pkgs.neovim;
      defaultText = literalExample "pkgs.neovim";
      example = literalExample "pkgs.neovim";
      description = "The neovim package to install. May be used to change the version." ;
    };
  };

  config = mkIf cfg.enable {
    home.packages = if (cfg.package != null) then [ cfg.package ] else [ ];

    xdg.configFile."nvim/init.vim".text = ''
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      " ~/.config/nvim/init.vim
      " Managed by nix
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      ""
      "" Settings
      ""

      " General
      set nocompatible        " For lots of cool vim settings (must be set first)
      set title               " Set title of terminal
      set showcmd             " Show information about the current command in use
      set nu                  " Set line numbers
      set belloff=all         " No visual/audio bell (e.g. sound from pressing down at EOF)
      set ruler               " Set ruler in bottom-left (may be set already)
      set history=9999        " Big history
      set virtualedit=onemore " Allow one more character in normal mode
      syntax enable           " Turn on syntax highlighting (may be set already)

      " Spaces, indentation, backspace, etc.
      set smartindent         " Make it smarter!
      set ai                  " Auto indent
      set bs=2                " Makes backspace like it should be (may be set already)

      " General text searching
      set ic                  " Ignore case in search
      set incsearch           " Incremental search
      set hlsearch            " Highlight search results
      set smartcase           " Ignore case when lowercase

      " Make autocompletion better
      set wildmenu
      set wildmode=longest,list,full

      ""
      "" File specific setings/tricks
      ""
      " Filetype
      filetype on             " Enable file type detection (may be set already)
      filetype plugin on      " Load plugin for specific file types

      " All files
      set tabstop=2       " Make tabs 2 columns wide (when pressing <Tab>)
      set shiftwidth=2    " Make identations 2 columns wide (e.g. using i>{ )
      set expandtab       " Expand tabs to appropriate amount of spaces
      set textwidth=100   " Wrap text after 100 characters

      highlight TodoWord ctermbg=darkred ctermfg=white
      match TodoWord /TODO/

      highlight DebugWord ctermbg=darkmagenta ctermfg=white
      match DebugWord /DEBUG/

      " Highlight anything over 100 characters
      highlight OverLength ctermbg=darkred ctermfg=white guibg=#FFD9D9
      match OverLength /\%>100v.\+/

      " Remove trailing whitespace in files
      au BufReadPost,BufWrite * if ! &bin | silent! %s/\s\+$//ge | endif
    '';
  };
}