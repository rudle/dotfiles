autocmd!

:nmap <C-N><C-N> :set invnumber <CR>

if has('syntax') && (&t_Co > 2)
  syntax on
endif

set history=50

set shortmess+=r

set showmode
set showcmd

" when using list, keep tabs at their full width and display `arrows':
" (Character 187 is a right double-chevron, and 183 a mid-dot.)

" have the mouse enabled all the time:
"set mouse=a

" don't have files trying to override this .vimrc:
set nomodeline


" * Text Formatting -- General

" don't make it look like there are line breaks where there aren't:

" use indents of 2 spaces, and have them copied down lines:
set shiftwidth=2
set shiftround
set expandtab
set autoindent

" normally don't automatically format `text' as it is typed, IE only do this
" with comments, at 79 characters:
set formatoptions-=t
set textwidth=79

" get rid of the default style of C comments, and define a style with two stars
" at the start of `middle' rows which (looks nicer and) avoids asterisks used
" for bullet lists being treated like C comments; then define a bullet list
" style for single stars (like already is for hyphens):
set comments-=s1:/*,mb:*,ex:*/
set comments+=s:/*,mb:**,ex:*/
set comments+=fb:*

" treat lines starting with a quote mark as comments (for `Vim' files, such as
" this very one!), and colons as well so that reformatting usenet messages from
" `Tin' users works OK:
set comments+=b:\"
set comments+=n::


" * Text Formatting -- Specific File Formats

call pathogen#runtime_append_all_bundles()
filetype on

autocmd BufNewFile,BufRead *.txt set filetype=human

" in human-language files, automatically format everything at 72 chars:
autocmd FileType mail,human set formatoptions+=t textwidth=72

" for C-like programming, have automatic indentation:
autocmd FileType c,cpp,slang set cindent

" for actual C (not C++) programming where comments have explicit end
" characters, if starting a new line in the middle of a comment automatically
" insert the comment leader characters:
autocmd FileType c set formatoptions+=ro

" for Perl programming, have things in braces indenting themselves:
autocmd FileType perl set smartindent

" for HTML, generally format text, but if a long line has been created leave it
" alone when editing:
autocmd FileType html set formatoptions+=tl

" in makefiles, don't expand tabs to spaces, since actual tab characters are
" needed, and have indentation at 8 chars to be sure that all indents are tabs
" (despite the mappings later):
autocmd FileType make set noexpandtab shiftwidth=8


set ignorecase
set smartcase

let IspellLang = 'british'
let PersonalDict = '~/.ispell_' . IspellLang


" scroll the window (but leaving the cursor in the same place) by a couple of
" lines up/down with <Ins>/<Del> (like in `Lynx'):
noremap <Ins> 2<C-Y>
noremap <Del> 2<C-E>
" [<Ins> by default is like i, and <Del> like x.]

" use <F6> to cycle through split windows (and <Shift>+<F6> to cycle backwards,
" where possible):
nnoremap <F6> <C-W>w
nnoremap <S-F6> <C-W>W

" have Y behave analogously to D and C rather than to dd and cc (which is
" already done by yy):
noremap Y y$



nnoremap \tl :set invlist list?<CR>
nnoremap \th :set invhls hls?<CR>


  function ToggleFold()
    if foldlevel('.') == 0
        " No fold exists at the current line,
        " so create a fold based on indentation

        let l_min = line('.')   " the current line number
         let l_max = line('$')   " the last line number
         let i_min = indent('.') " the indentation of the current line
         let l = l_min + 1
"
         " Search downward for the last line whose indentation > i_min
"
         while l <= l_max
            " if this line is not blank ...
            if strlen(getline(l)) > 0 && getline(l) !~ '^\s*$'
               if indent(l) <= i_min
                  " we've gone too far
                  let l = l - 1    " backtrack one line
                  break
               endif
            endif
            let l = l + 1
         endwhile
"
         " Clamp l to the last line
         if l > l_max
            let l = l_max
         endif
"
         " Backtrack to the last non-blank line
         while l > l_min
            if strlen(getline(l)) > 0 && getline(l) !~ '^\s*$'
               break
            endif
            let l = l - 1
         endwhile
"
         "execute "normal i" . l_min . "," . l . " fold"   " print debug info
"
         if l > l_min
            " Create the fold from l_min to l
            execute l_min . "," . l . " fold"
         endif
      else
         " Delete the fold on the current line
         normal zd
      endif
   endfunction

  function! Percent()
    let byte = line2byte( line( "." ) ) + col( "." ) - 1
    let size = (line2byte( line( "$" ) + 1 ) - 1)
    " return byte . " " . size . " " . (byte * 100) / size
    return (byte * 100) / size
  endfunction


  set t_Co=256
  colorscheme lucius
  let loaded_matchparen=1

  map tt :tabnew<cr>
  map tc :tabclose<cr>
  map tm :tabmove
  set sw=2
  set tabstop=2
  set expandtab
  set smartindent
  nmap <C-N><C-T> :RN <CR>
  nmap <C-W><C-N> :vnew <CR>


  "nnoremap <C-p> :FufFile <C-r>=fnamemodify(getcwd(), ':p')<CR><CR>

  noremap <C-N> :NERDTreeToggle <CR>
  
  let g:allml_global_maps = 1

  nnoremap <silent> <F3> :Rgrep <CR>

  set showmatch

  set nobackup
  set nowritebackup
  set noswapfile

  au FileType * setl fo-=cro

  let mapleader = ","

  nnoremap <Leader>h :LustyJuggler <CR>
  nnoremap <Leader>, :FufBuffer <CR>
  nnoremap <Leader>. :FufFile **/<CR>

  nnoremap L :bnext <CR>
  nnoremap H :bprevious <CR>

  nnoremap <space> :


  set listchars=tab:»·,trail:·
  set list

  set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\%{Percent()}%%

  set laststatus=2
  set visualbell t_vb=

  nnoremap : q:i
  nnoremap / q/i
  nnoremap ? q?i

  set hidden

set wildmode=list:longest

map <F5> {!}fmt<CR>

autocmd FileType ruby set omnifunc=rubycomplete#Complete
autocmd FileType css set omnifunc=csscomplete#CompleteCSS

let bufmru_switchkey = "<Leader>t"

set cursorline cursorcolumn

set nowrap
