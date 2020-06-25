" Needed for line 15
set rtp+=~/.fzf
set rtp+=~/.local/nvim/plugged/LanguageClient-neovim
" WHYYY
" set rtp+=~/.local/lib/python3.6/site-packages

call plug#begin('~/.local/nvim/plugged')

" Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
" Use with languageclient
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'roxma/nvim-yarp'
" Plug 'roxma/vim-hug-neovim-rpc'

Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree'
Plug 't9md/vim-choosewin'
Plug 'flazz/vim-colorschemes'

" Parenthesis things
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
"Plug '$HOME/.fzf'
Plug 'junegunn/fzf.vim'

call plug#end()

" You have git for this
set noswapfile


" UI
colorscheme monokai-chris

syntax on
set expandtab
set shiftwidth=4


" NerdTree Stuff
cmap tree NERDTree .
let NERDTreeShowHidden=1 " show hidden files in nerdtree

" Window Nav
map WW :ChooseWin<CR>
map WJ <C-w>h
map WK <C-w>l
map tr :ChooseWin<CR>a

" COC.VIM STUFF BELOW

" source ~/.config/nvim/init.lspCoc.vim
source ~/.config/nvim/init.lspLangClient.vim


" Fzf stuff
map ; :Files .<CR>


" Terminal escaping
tnoremap <Esc> <C-\><C-n>
