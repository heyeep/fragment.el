# fragment.el

Reponsive grid layout for Emacs text buffers.

## Description

fragment.el lets you create grids in emacs buffers. This was inspired
by the [tui.el](https://github.com/ebpa/tui.el) package

## Installation

```elisp
(add-to-list 'load-path "/path/to/fragment.el")
(require 'fragment)
(require 'fragment-mode)
```

## Basic usage

```elisp
;; Enable grid mode
(fragment-grid-mode 1)

;; Create a 3x3 grid
(setq my-grid (fragment-create-grid 3 3))

;; Put text in cells
(fragment-cell-set-content (fragment-grid-get my-grid 0 0) "Name")
(fragment-cell-set-content (fragment-grid-get my-grid 0 1) "Age")
(fragment-cell-set-content (fragment-grid-get my-grid 1 0) "Milk")
(fragment-cell-set-content (fragment-grid-get my-grid 1 1) "30")
```

## Commands

When `fragment-grid-mode` is on:

- `C-c C-g n` - Create new grid
- `C-c C-g c` - Edit cell content at cursor
- `C-c C-g r` - Resize grid manually
- `C-c C-g i` - Show grid info

## Usage

### Creating Grids
- `(fragment-create-grid rows cols)` - Create and display grid
- `(fragment-grid-get grid row col)` - Get cell at position

### Cell
- `(fragment-cell-set-content cell text)` - set content
- `(oset cell min-width 5)` - minimum width
- `(oset cell max-width 20)` - maximum width
- `(oset cell background-color "#FF0000")` - background color
- `(oset cell foreground-color "white")` - text color

## Example

```elisp
(let ((grid (fragment-create-grid 2 3)))
  ;; Create a simple table
  (fragment-cell-set-content (fragment-grid-get grid 0 0) "Name")
  (fragment-cell-set-content (fragment-grid-get grid 0 1) "Age")
  (fragment-cell-set-content (fragment-grid-get grid 0 2) "City")
  (fragment-cell-set-content (fragment-grid-get grid 1 0) "Alice")
  (fragment-cell-set-content (fragment-grid-get grid 1 1) "30")
  (fragment-cell-set-content (fragment-grid-get grid 1 2) "NYC")

;; Set cell attributes
  (oset (fragment-grid-get grid 0 0) background-color "#FF6B6B")
  (oset (fragment-grid-get grid 0 0) foreground-color "white"))
```

