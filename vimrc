colorscheme desert
map <F1> :set fo-=a<Enter>
map <F2> :set fo+=a<Enter>
:hi CursorLine   cterm=NONE ctermbg=darkred ctermfg=white guibg=darkred guifg=white
:hi CursorColumn cterm=NONE ctermbg=darkred ctermfg=white guibg=darkred guifg=white
:nnoremap <Leader>c :set cursorline! cursorcolumn!<CR>

set tabstop=8
set expandtab
set softtabstop=4
set shiftwidth=4
set smarttab
set shiftround
set nojoinspaces
set comments+=:--\ 

set fo+=ro
set autoindent
set ignorecase
set cursorline
set cursorcolumn
