# spatial-window.el

Jump to Emacs windows by pressing keys that match their spatial position on your keyboard.

## Installation

Requires Emacs 27.1+ and [posframe](https://github.com/tumashu/posframe).

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

## Customization

`M-x customize-group RET spatial-window RET`

### Keyboard Layout

Presets: `qwerty` (default), `dvorak`, `colemak`

```elisp
(setq spatial-window-keyboard-layout 'dvorak)
```

Or define a custom layout:

```elisp
(setq spatial-window-keyboard-layout
      '(("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
        ("q" "w" "e" "r" "t" "y" "u" "i" "o" "p")
        ("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")))
```

## License

GPL-3.0
