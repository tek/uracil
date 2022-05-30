let s:repo = fnamemodify(expand('<sfile>'), ":p:h:h")
let r = jobstart(
      \ ['nix', 'run', '.#uracil'],
      \ { 'rpc': v:true, 'cwd': s:repo,
      \ 'on_stderr': { j, d, e -> chansend(2, d) } }
      \ )
