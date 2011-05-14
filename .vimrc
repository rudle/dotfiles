autocmd!

if has('syntax') && (&t_Co > 2)
  syntax on
endif

set t_Co=256
colorscheme lucius

let loaded_matchparen=1
let IspellLang = 'british'
let PersonalDict = '~/.ispell_' . IspellLang

set autoindent
set binary
set cursorline cursorcolumn
set expandtab
set formatoptions-=t
set hidden
set history=50
set ignorecase
set laststatus=2
set list
set listchars=tab:»·,trail:·
set nobackup
set nomodeline
set noswapfile
set nowritebackup
set shiftround
set shiftwidth=2
set shortmess+=r
set showcmd
set showmatch
set showmode
set smartcase
set smartindent
set statusline=[%n]\ %<%.99f\ %h%w%m%r%{exists('*CapsLockStatusline')?CapsLockStatusline():''}%y%=%-16(\ %l,%c-%v\ %)%P
set sw=2
set tabstop=2
set textwidth=79
set visualbell t_vb=
set wildmode=list:longest
set wrap

call pathogen#runtime_append_all_bundles()
filetype on

autocmd BufNewFile,BufRead *.txt set filetype=human
autocmd FileType mail,human set formatoptions+=t textwidth=72
autocmd FileType c,cpp,slang set cindent
autocmd FileType c set formatoptions+=ro
autocmd FileType html set formatoptions+=tl
autocmd FileType make set noexpandtab shiftwidth=8
autocmd FileType ruby set omnifunc=rubycomplete#Complete
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
au FileType * setl fo-=cro

noremap Y y$
noremap <Ins> 2<C-Y>
noremap <Del> 2<C-E>
nnoremap <F6> <C-W>w
nnoremap <S-F6> <C-W>W
nnoremap \tl :set invlist list?<CR>
nnoremap \th :set invhls hls?<CR>

nmap <C-N><C-T> :RN <CR>
nmap <C-W><C-N> :vnew <CR>

noremap <C-N> :NERDTreeToggle <CR>

let g:allml_global_maps = 1

let mapleader = ","

map <Leader>t :tabnew<cr>
map <Leader>tc :tabclose<cr>
map <F5> {!}fmt<CR>
nnoremap <Leader>h :LustyJuggler <CR>
nnoremap <Leader>, :FufBuffer <CR>
nnoremap <Leader>. :FufFile **/<CR>
nnoremap <space> :
nnoremap L :bnext <CR>
nnoremap H :bprevious <CR>
