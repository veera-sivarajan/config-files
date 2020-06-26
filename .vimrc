filetype on
autocmd FileType python nnoremap <buffer> <F1> :exec '!clear; python3' shellescape(@%, 1)<cr>
set number
set noswapfile 
set tags=tags
set sts=2
set tabstop=2 shiftwidth=2 expandtab
set autoindent
set shellcmdflag=-ic
