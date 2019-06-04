# Intro

**uracil** is a [Neovim] plugin written in [Haskell] and powered by [ribosome] and [nvim-hs].

It provides comfortable access to registers for yanking and pasting.
The plugin consumes yank autocmd event data and provides several lists of snippets to the user.

# Install

There are two alternative methods for integrating this plugin into your Neovim.
In both cases, the plugin will bootstrap your machine with [stack] and install itself on startup, as well as rebuild
when the repository is updated.

## nvim-hs

The basic variant is to use the built-in package management facility of the rplugin provider.
In this case, you have to include [nvim-hs.vim]:

```vim
Plug 'neovimhaskell/nvim-hs.vim'
Plug 'tek/uracil'
```

## chromatin

The author maintains a manager for [ribosome]-based plugins called [chromatin] that has additional features.
You will be presented with a terminal buffer containing the output of the installation routines whenever something is
built.

```vim
Plug 'tek/chromatin'
Plug 'tek/uracil'
```

# Settings

## `g:uracil_paste_timeout`

The time in seconds after which the current [paste](#urapaste) is terminated.
Default is `1`.

# Commands

## `UraYankMenu`

Displays an interactive scratch buffer with the current yank history.
Several actions can be performed through mappings:

* `p` insert the yank after the cursor position. Whether the paste is line-wise or character-wise depends on how it was
  yanked.
* `P` insert the yank before the cursor position.
* `y` load the yank into to `"` register.

## `UraPaste`

Paste the most recently yanked text, basically like the regular `p` or using `p` in the yank menu.

Repeatedly calling this function cycles through the yank history, each time calling `undo` and pasting the next entry.
A floating window containing the yank history is displayed at the cursor.

After the time configured by [`g:uracil_paste_timeout`](#guracil-paste-timeout) has passed, the window is hidden and
the currently pasted entry is reset, so that a subsequent paste starts from the top.

[Neovim]: https://github.com/neovim/neovim
[Haskell]: https://www.haskell.org
[ribosome]: https://github.com/tek/ribosome
[chromatin]: https://github.com/tek/chromatin
[nvim-hs]: https://github.com/neovimhaskell/nvim-hs
[nvim-hs.vim]: https://github.com/neovimhaskell/nvim-hs.vim
[stack]: https://docs.haskellstack.org/en/stable/README
