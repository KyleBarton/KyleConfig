source ~/.vim/plugins.vim
source ~/.vim/wiki.vim

" Maps :Nerttree on the working directory to :tr
cmap tree NERDTree .

set nocompatible
filetype on
syntax on
set backspace=indent,eol,start
colorscheme Slate

" 
set tabstop=4
set shiftwidth=4

" Lets NERDTree show hidden files
let NERDTreeShowHidden=1

" Keeps lastline up and running
set laststatus=2

" This makes the cursor thin in insert mode, see http://vim.wikia.com/wiki/Change_cursor_shape_in_different_modes
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_SR = "\<Esc>]50;CursorShape=2\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"
set number

set switchbuf=useopen
set hidden

" map NT :Window <CR> zsh <CR>
map ; :Files<CR>
" map tr <C-w>h
" map L <C-w>l

" experimental mappings
map n; :vsp<CR>:Files<CR>
map N; :sp<CR>:Files<CR>

" window nav stuff
map WW :ChooseWin<CR>
map WJ <C-w>h
map WK <C-w>l
map tr :ChooseWin<CR>a
