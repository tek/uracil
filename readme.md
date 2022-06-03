# Intro

**Uracil** is a [Neovim] plugin written in [Haskell] with the plugin framework [Ribosome].

It provides comfortable access to registers for yanking and pasting.
The plugin consumes yank autocmd event data and provides several lists of snippets to the user.

# Install

The plugin can be loaded by specifying the Github repo to a package manager like any other, but in order to start it,
the [Nix] build tools needs to be available.
Nix will fetch Uracil's dependencies and build its sources when the plugin is loaded.

# Recommended Mappings

```vim
nnoremap p <cmd>call UraPaste()<cr>
xnoremap p <cmd>call UraPaste()<cr>
nnoremap P <cmd>call UraPpaste()<cr>
xnoremap P <cmd>call UraPpaste()<cr>
```

# Settings

## `g:uracil_paste_timeout`

The time in seconds after which the current [paste](#urapaste) is terminated.
Default is `1`.

## `g:uracil_skip_yank`

Some commands from other plugins might use the unnamed register to store
temporary data, causing the next paste to contain that data.
Setting this variable to `1` before running that command, for example from
withing a wrapper function for a mapping, will cause that yank to be ignored.
After the yank, the variable will be reset to `0`.

# Commands

## Operator Filters

Some commands have alternative forms that take an argument that is used to
filter the yanks by which operator was used for the triggering commands.

For example, to start a paste session only for deleted and changed text:

```vim
UraPasteFor dc
```

Possible operators are `d`, `c`, `y` and `x`.
`x` is technically not an operator but `d` with the register type `character`,
but for convenience it is converted for the check.

## `UraYankMenu`

Displays an interactive scratch buffer with the current yank history.
Several actions can be performed through mappings:

* `p` insert the yank after the cursor position. Whether the paste is line-wise
  or character-wise depends on how it was yanked.
* `P` insert the yank before the cursor position.
* `y` load the yank into to `"` register.

### `UraYankMenuFor`

Same as `UraYankMenu`, but takes one argument that specifies the
[operator filter][#operator-filters].

## `UraPaste`

Paste the most recently yanked text, basically like the regular `p` or using
`p` in the yank menu.

Repeatedly calling this function cycles through the yank history, each time
calling `undo` and pasting the next entry. A floating window containing the
yank history is displayed at the cursor.

After the time configured by [`g:uracil_paste_timeout`](#guracil-paste-timeout)
has passed or the cursor was moved, the window is hidden and the currently
pasted entry is reset, so that a subsequent paste starts from the top.
The pasted history entry will be moved to the beginning of the list.

**Note** that in order for this to work properly in visual mode, you will have
to use the `<cmd>` pseudokey in the mapping:

```vim
xnoremap p <cmd>call UraPaste()<cr>
```

Using `:` here would start command line mode, which would interfere with the
visual mode detection in the plugin.

### `UraPasteFor`

Same as `UraPaste`, but takes one argument that specifies the
[operator filter][#operator-filters].

## `UraPpaste`

Identical to [`UraPaste`](#urapaste), but uses `P`.

Same as `UraPpaste`, but takes one argument that specifies the
[operator filter][#operator-filters].

## `UraDiag`

Displays various information about the current state in a scratch buffer:

* yank history
* errors

[Neovim]: https://github.com/neovim/neovim
[Haskell]: https://www.haskell.org
[Ribosome]: https://github.com/tek/ribosome
[nix]: https://nixos.org/learn.html
