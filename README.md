# spatial-window.el

Jump to Emacs windows by pressing keys that match their spatial position on your keyboard.

![Demo](demo.gif)

## Installation

Requires Emacs 28.1+ and [posframe](https://github.com/tumashu/posframe).

### Elpaca

```elisp
(use-package spatial-window
  :ensure (:host github :repo "lewang/spatial-window")
  :bind ("M-o" . spatial-window-select))
```

### Manual

```elisp
(require 'spatial-window)
(global-set-key (kbd "M-o") #'spatial-window-select)
```

## Usage

Press your keybinding to show overlays in each window displaying which keys will select it. Press a key to jump to
that window.

```
+------------------+------------------+
|  q w e r t       |       y u i o p  |
|  a s d f g       |       h j k l ;  |
|  z x c v b       |       n m , . /  |
+------------------+------------------+
```

### Action Modifiers

During selection, the current window is highlighted. Uppercase keys switch the action mode:

| Key | Action | Description |
|-----|--------|-------------|
| `K` | Kill | Current window pre-selected; `RET` to delete, or pick another |
| `S` | Swap | Press a layout key to swap buffers with current window |
| `F` | Focus | Current window highlighted; `RET` to focus, or select another |

`RET` always confirms: in kill/focus it acts on the current selection, otherwise it exits.

Kill mode pre-selects the current window so `K` `RET` is a quick single-window kill. Selecting a different window
replaces the pre-selection automatically — you don't need to deselect the original first. After that first pick,
further presses toggle windows on/off to build a multi-kill set.

After focusing, call `spatial-window-unfocus` to restore the previous layout.

### History Navigation

Actions that modify window layout (kill, swap, focus) automatically save the previous configuration. While
the overlay is showing, use arrow keys to browse history:

| Key | Action |
|-----|--------|
| `←` | Undo — restore an older window configuration |
| `→` | Redo — return toward the current configuration |

The prompt shows what each direction will do, e.g. `[←] Undo kill [→] Redo swap <1/2>`.

Press `C-g` while browsing to cancel and return to your original layout.

History is kept per-tab when `tab-bar-mode` is active, otherwise per-frame. Up to
`spatial-window-history-max` entries are retained (default 20).

Unlike `winner-mode`, spatial-window does not listen to hooks or track every window change globally. Only
actions performed through spatial-window (kill, swap, focus) save history, so undo/redo is predictable —
you're reversing your own deliberate actions, not unrelated side effects from other packages or commands.

### Edge Extension Keys

The number row and keys adjacent to the right edge of the layout act as
extension keys — pressing them triggers the same window as the nearest
layout key. This covers the margin of error when your finger overshoots
while reaching toward a corner or edge of the keyboard.

For QWERTY:

| Extension key(s) | Maps to |
|-------------------|---------|
| `` ` ``, `1` | `q` |
| `2`–`9` | `w`–`o` (key directly below) |
| `0`, `-`, `=`, `[`, `]` | `p` |
| `'` | `;` |

This is intentionally *not* a four-row layout. The number row is not
farther to reach than the letters — it is the same reach, just upward.
The purpose is purely to absorb overshoot, not to add spatial resolution.

## Customization

`M-x customize-group RET spatial-window RET`

### Keyboard Layout

Presets: `qwerty` (default), `dvorak`, `colemak`

```elisp
(customize-set-variable 'spatial-window-keyboard-layout 'dvorak)
```

Or define a custom layout:

```elisp
(customize-set-variable 'spatial-window-keyboard-layout
                        '(("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
                          ("q" "w" "e" "r" "t" "y" "u" "i" "o" "p")
                          ("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")))
```

### Overlay Delay

If you know your target window immediately, you can act before overlays render. Set a delay so overlays only
appear if you hesitate:

```elisp
(customize-set-variable 'spatial-window-overlay-delay 0.3) ;; seconds, or nil for immediate
```

## Alternatives

| Package                                                   | Approach                                                                           |
|-----------------------------------------------------------|------------------------------------------------------------------------------------|
| [ace-window](https://github.com/abo-abo/ace-window)       | Labels windows with sequential chars (1,2,3 or a,b,c). Read label, then press key. |
| [winum](https://github.com/deb0ch/emacs-winum)            | Numbers windows 1-9 in mode-line. Press number to switch.                          |
| [switch-window](https://github.com/dimitri/switch-window) | Large overlay numbers. Visual but requires reading.                                |

**spatial-window**: No labels to read. Look at window → fingers know the key. Keyboard position = screen position.

## License

GPL-3.0
