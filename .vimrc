set nocompatible
let mapleader=" "
set number relativenumber
syntax on
filetype plugin on


if filereadable(expand("~/.vim/autoload/plug.vim"))
endif

call plug#begin('~/.vim/plugged')
Plug 'sheerun/vim-polyglot'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'vim-pandoc/vim-pandoc'
Plug 'rwxrob/vim-pandoc-syntax-simple'
"Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'morhetz/gruvbox'
"#Plug 'vimwiki/vimwiki'
Plug 'dhruvasagar/vim-table-mode'
Plug 'junegunn/goyo.vim'
Plug 'itchyny/lightline.vim'
call plug#end()

" Vim lightline plugin
set laststatus=2
if !has('gui_running')
    set t_Co=256
endif


autocmd BufRead,BufNewFile *.tex set filetype=tex
autocmd BufRead,BufNewFile /tmp/calcurse*,~/.calcurse/notes/* set filetype=markdown

"Compile and open output
map <leader>G :w! \| !comp <c-r>%<CR><CR>
map <leader>o :!opout <c-r>%<CR><CR>

"Enable and disable auto comment

map <leader>c :setlocal formatoptions-=cro<CR>
map <leader>C :setlocal formatoptions=cro<CR>

"Enable spell checking
map <leader>s :setlocal spell! spelllang=en_us<CR>
map <leader>S :setlocal nospell!<CR>
" Center screen when entering insert mode
autocmd InsertEnter * norm zz

" Remove trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e

"Shortcutting split navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

"Shortcut split opening
nnoremap <leader>h :split<Space>
nnoremap <leader>v :vsplit<Space>

"Alias replace all to S
"nnoremap S :%s//gI<Left><Left><Left>

set background=dark
set hidden
set hlsearch
set scrolloff=8
set colorcolumn=80
set signcolumn=yes
set lazyredraw

"Set tab behavior
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent

"Set search case
set ignorecase
set smartcase
set encoding=utf-8
set incsearch

"Autocompletion
set wildmode=longest,list,full

"Fix splitting
set splitbelow splitright

let g:gruvbox_guisp_fallback="bg"
autocmd vimenter * ++nested colorscheme gruvbox
set background=dark
"let g:vimwiki_list = [{'path': '~/vimwiki/', 'syntax': 'markdown', 'ext': '.md'}]
