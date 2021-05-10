let s:dir = fnamemodify(expand('<sfile>'), ':p:h:h')
let g:chromatin_rplugins = get(g:, 'chromatin_rplugins', []) + [{ 'name': 'uracil', 'spec': 'flake:path:' . s:dir, 'dev': v:true, 'debug': get(g:, 'uracil_debug', 0) }]
