set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim

" Needed for line 24
set rtp+=/usr/local/opt/fzf

call vundle#begin()

" Vundle manages itself
Plugin 'VundleVim/Vundle.vim'

" UI Stuff
Plugin 'itchyny/lightline.vim'

" Colorschemes
Plugin 'flazz/vim-colorschemes'

" Parenthesis things
Plugin 'tpope/vim-surround'
Plugin 'jiangmiao/auto-pairs'

" Navigation
Plugin 'junegunn/fzf.vim'
Plugin 'scrooloose/nerdtree'
Plugin 't9md/vim-choosewin'

" Code help
Plugin 'Valloric/YouCompleteMe'

" VimWiki
Plugin 'vimwiki/vimwiki'

call vundle#end()
filetype plugin indent on
