;;; jump-tree.el --- Treat position history as a tree  -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017  Free Software Foundation, Inc

;; Author: Wen Yang <yangwen0228@foxmail.com>
;; Maintainer: Wen Yang <yangwen0228@foxmail.com>
;; Version: 0.1.0
;; Package-Version: 20170803.1
;; Keywords: convenience, files, jump, tree
;; URL:
;; Repository:

;; This file is part of Emacs.
;;
;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Emacs has a powerful position system. Unlike the standard jump-prev/jump-next
;; system in most software, it allows you to jump to the position of anywhere
;; you have gone.
;;
;; Both the loss of data with standard jump-prev/jump-next, and the confusion of
;; Emacs' position, stem from trying to treat position history as a linear
;; sequence of changes. It's not. The `jump-tree-mode' provided by this package
;; replaces Emacs' position system with a system that treats position history as
;; what it is: a branching tree of changes. This simple idea allows the more
;; intuitive behaviour of the standard jump-prev/jump-next system to be combined
;; with the power of never losing any history.
;;
;; This package is inspired by undo-tree and jumplist, and copy a lot of codes
;; from them.
;;
;; Installation
;; ============
;;
;; This package has only been tested with Emacs versions 24 and CVS. It should
;; work in Emacs versions 22 and 23 too, but will not work without
;; modifications in earlier versions of Emacs.
;;
;; To install `jump-tree-mode', make sure this file is saved in a directory in
;; your `load-path', and add the line:
;;
;;   (require 'jump-tree)
;;
;; to your .emacs file. Byte-compiling jump-tree.el is recommended (e.g. using
;; "M-x byte-compile-file" from within emacs).
;;
;; If you want to replace the standard Emacs' position system with the
;; `jump-tree-mode' system in all buffers, you can enable it globally by
;; adding:
;;
;;   (global-jump-tree-mode)
;;
;; to your .emacs file.
;;
;;
;; Quick-Start
;; ===========
;;
;; If you're the kind of person who likes to jump in the car and drive,
;; without bothering to first figure out whether the button on the left dips
;; the headlights or operates the ejector seat (after all, you'll soon figure
;; it out when you push it), then here's the minimum you need to know:
;;
;; `jump-tree-mode' and `global-jump-tree-mode'
;;   Enable jump-tree mode (either in the current buffer or globally).
;;
;; M-, (`jump-tree-jump-prev')
;;   Jump-Prev changes.
;;
;; C-? (`jump-tree-jump-next')
;;   Jump-Next changes.
;;
;; `jump-tree-switch-branch'
;;   Switch jump-tree branch.
;;   (What does this mean? Better press the button and see!)
;;
;; C-x u  (`jump-tree-visualize')
;;   Visualize the position tree.
;;   (Better try pressing this button too!)
;;
;; C-x r u  (`jump-tree-save-state-to-register')
;;   Save current buffer state to register.
;;
;; C-x r U  (`jump-tree-restore-state-from-register')
;;   Restore buffer state from register.
;;
;;
;;
;; In the jump-tree visualizer:
;;
;; <up>  p  C-p  (`jump-tree-visualize-jump-prev')
;;   Jump-Prev changes.
;;
;; <down>  n  C-n  (`jump-tree-visualize-jump-next')
;;   Jump-Next changes.
;;
;; <left>  b  C-b  (`jump-tree-visualize-switch-branch-left')
;;   Switch to previous jump-tree branch.
;;
;; <right>  f  C-f  (`jump-tree-visualize-switch-branch-right')
;;   Switch to next jump-tree branch.
;;
;; C-<up>  M-{  (`jump-tree-visualize-jump-prev-to-x')
;;   Jump-Prev changes up to last branch point.
;;
;; <down>  n  C-n  (`jump-tree-visualize-jump-next')
;;   Jump-Next changes.
;;
;; <mouse-1>  (`jump-tree-visualizer-mouse-set')
;;   Set state to node at mouse click.
;;
;; t  (`jump-tree-visualizer-toggle-timestamps')
;;   Toggle display of time-stamps.
;;
;; d  (`jump-tree-visualizer-toggle-diff')
;;   Toggle diff display.
;;
;; s  (`jump-tree-visualizer-selection-mode')
;;   Toggle keyboard selection mode.
;;
;; q  (`jump-tree-visualizer-quit')
;;   Quit jump-tree-visualizer.
;;
;; C-q  (`jump-tree-visualizer-abort')
;;   Abort jump-tree-visualizer.
;;
;; ,  <
;;   Scroll left.
;;
;; .  >
;;   Scroll right.
;;
;; <pgup>  M-v
;;   Scroll up.
;;
;; <pgdown>  C-v
;;   Scroll down.
;;
;;
;;
;; In visualizer selection mode:
;;
;; <up>  p  C-p  (`jump-tree-visualizer-select-previous')
;;   Select previous node.
;;
;; <down>  n  C-n  (`jump-tree-visualizer-select-next')
;;   Select next node.
;;
;; <left>  b  C-b  (`jump-tree-visualizer-select-left')
;;   Select left sibling node.
;;
;; <right>  f  C-f  (`jump-tree-visualizer-select-right')
;;   Select right sibling node.
;;
;; <pgup>  M-v
;;   Select node 10 above.
;;
;; <pgdown>  C-v
;;   Select node 10 below.
;;
;; <enter>  (`jump-tree-visualizer-set')
;;   Set state to selected node and exit selection mode.
;;
;; s  (`jump-tree-visualizer-mode')
;;   Exit selection mode.
;;
;; t  (`jump-tree-visualizer-toggle-timestamps')
;;   Toggle display of time-stamps.
;;
;; q  (`jump-tree-visualizer-quit')
;;   Quit jump-tree-visualizer.
;;
;; C-q  (`jump-tree-visualizer-abort')
;;   Abort jump-tree-visualizer.
;;
;; ,  <
;;   Scroll left.
;;
;; .  >
;;   Scroll right.
;;
;;
;;
;; Persistent position history:
;;
;; Note: Requires Emacs version 24.3 or higher.
;;
;; Jump-Prev Systems
;; ============
;;
;; To understand the different position systems, it's easiest to consider an
;; example. Imagine you make a few edits in a buffer. As you edit, you
;; accumulate a history of changes, which we might visualize as a string of
;; past buffer states, growing downwards:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o  (first edit)
;;                                |
;;                                |
;;                                o  (second edit)
;;                                |
;;                                |
;;                                x  (current buffer state)
;;
;;
;; Now imagine that you position the last two changes. We can visualize this as
;; rewinding the current state back two steps:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                x  (current buffer state)
;;                                |
;;                                |
;;                                o
;;                                |
;;                                |
;;                                o
;;
;;
;; However, this isn't a good representation of what Emacs' position system
;; does. Instead, it treats the jump-prevs as *new* changes to the buffer, and adds
;; them to the history:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o  (first edit)
;;                                |
;;                                |
;;                                o  (second edit)
;;                                |
;;                                |
;;                                x  (buffer state before position)
;;                                |
;;                                |
;;                                o  (first position)
;;                                |
;;                                |
;;                                x  (second position)
;;
;;
;; Actually, since the buffer returns to a previous state after an position,
;; perhaps a better way to visualize it is to imagine the string of changes
;; turning back on itself:
;;
;;        (initial buffer state)  o
;;                                |
;;                                |
;;                  (first edit)  o  x  (second position)
;;                                |  |
;;                                |  |
;;                 (second edit)  o  o  (first position)
;;                                | /
;;                                |/
;;                                o  (buffer state before position)
;;
;; Treating jump-prevs as new changes might seem a strange thing to do. But the
;; advantage becomes clear as soon as we imagine what happens when you edit
;; the buffer again. Since you've jump-prevne a couple of changes, new edits will
;; branch off from the buffer state that you've rewound to. Conceptually, it
;; looks like this:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o
;;                                |\
;;                                | \
;;                                o  x  (new edit)
;;                                |
;;                                |
;;                                o
;;
;; The standard jump-prev/jump-next system only lets you go backwards and forwards
;; linearly. So as soon as you make that new edit, it discards the old
;; branch. Emacs' position just keeps adding changes to the end of the string. So
;; the position history in the two systems now looks like this:
;;
;;            Jump-Prev/Jump-Next:                      Emacs' position
;;
;;               o                                o
;;               |                                |
;;               |                                |
;;               o                                o  o
;;               .\                               |  |\
;;               . \                              |  | \
;;               .  x  (new edit)                 o  o  |
;;   (discarded  .                                | /   |
;;     branch)   .                                |/    |
;;               .                                o     |
;;                                                      |
;;                                                      |
;;                                                      x  (new edit)
;;
;; Now, what if you change your mind about those jump-prevs, and decide you did
;; like those other changes you'd made after all? With the standard jump-prev/jump-next
;; system, you're lost. There's no way to recover them, because that branch
;; was discarded when you made the new edit.
;;
;; However, in Emacs' position system, those old buffer states are still there in
;; the position history. You just have to rewind back through the new edit, and
;; back through the changes made by the jump-prevs, until you reach them. Of
;; course, since Emacs treats jump-prevs (even jump-prevs of jump-prevs!) as new changes,
;; you're really weaving backwards and forwards through the history, all the
;; time adding new changes to the end of the string as you go:
;;
;;                       o
;;                       |
;;                       |
;;                       o  o     o  (position new edit)
;;                       |  |\    |\
;;                       |  | \   | \
;;                       o  o  |  |  o  (position the position)
;;                       | /   |  |  |
;;                       |/    |  |  |
;;      (trying to get   o     |  |  x  (position the position)
;;       to this state)        | /
;;                             |/
;;                             o
;;
;; So far, this is still reasonably intuitive to use. It doesn't behave so
;; differently to standard jump-prev/jump-next, except that by going back far enough you
;; can access changes that would be lost in standard jump-prev/jump-next.
;;
;; However, imagine that after jump-preving as just described, you decide you
;; actually want to rewind right back to the initial state. If you're lucky,
;; and haven't invoked any command since the last position, you can just keep on
;; jump-preving until you get back to the start:
;;
;;      (trying to get   o              x  (got there!)
;;       to this state)  |              |
;;                       |              |
;;                       o  o     o     o  (keep jump-preving)
;;                       |  |\    |\    |
;;                       |  | \   | \   |
;;                       o  o  |  |  o  o  (keep jump-preving)
;;                       | /   |  |  | /
;;                       |/    |  |  |/
;;      (already undid   o     |  |  o  (got this far)
;;       to this state)        | /
;;                             |/
;;                             o
;;
;; But if you're unlucky, and you happen to have moved the point (say) after
;; getting to the state labelled "got this far", then you've "broken the position
;; chain". Hold on to something solid, because things are about to get
;; hairy. If you try to position now, Emacs thinks you're trying to position the
;; jump-prevs! So to get back to the initial state you now have to rewind through
;; *all* the changes, including the jump-prevs you just did:
;;
;;      (trying to get   o                          x  (finally got there!)
;;       to this state)  |                          |
;;                       |                          |
;;                       o  o     o     o     o     o
;;                       |  |\    |\    |\    |\    |
;;                       |  | \   | \   | \   | \   |
;;                       o  o  |  |  o  o  |  |  o  o
;;                       | /   |  |  | /   |  |  | /
;;                       |/    |  |  |/    |  |  |/
;;      (already undid   o     |  |  o<.   |  |  o
;;       to this state)        | /     :   | /
;;                             |/      :   |/
;;                             o       :   o
;;                                     :
;;                             (got this far, but
;;                              broke the position chain)
;;
;; Confused?
;;
;; In practice you can just hold down the position key until you reach the buffer
;; state that you want. But whatever you do, don't move around in the buffer
;; to *check* that you've got back to where you want! Because you'll break the
;; position chain, and then you'll have to traverse the entire string of jump-prevs
;; again, just to get back to the point at which you broke the
;; chain.
;;
;;
;; So what does `jump-tree-mode' do? Remember the diagram we drew to represent
;; the history we've been discussing (make a few edits, position a couple of them,
;; and edit again)? The diagram that conceptually represented our position
;; history, before we started discussing specific position systems? It looked like
;; this:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o
;;                                |\
;;                                | \
;;                                o  x  (current state)
;;                                |
;;                                |
;;                                o
;;
;; Well, that's *exactly* what the position history looks like to
;; `jump-tree-mode'.  It doesn't discard the old branch (as standard jump-prev/jump-next
;; does), nor does it treat jump-prevs as new changes to be added to the end of a
;; linear string of buffer states (as Emacs' position does). It just keeps track
;; of the tree of branching changes that make up the entire position history.
;;
;; If you position from this point, you'll rewind back up the tree to the previous
;; state:
;;
;;                                o
;;                                |
;;                                |
;;                                x  (position)
;;                                |\
;;                                | \
;;                                o  o
;;                                |
;;                                |
;;                                o
;;
;; If you were to position again, you'd rewind back to the initial state. If on
;; the other hand you jump-next the change, you'll end up back at the bottom of the
;; most recent branch:
;;
;;                                o  (position takes you here)
;;                                |
;;                                |
;;                                o  (start here)
;;                                |\
;;                                | \
;;                                o  x  (jump-next takes you here)
;;                                |
;;                                |
;;                                o
;;
;; So far, this is just like the standard jump-prev/jump-next system. But what if you
;; want to return to a buffer state located on a previous branch of the
;; history? Since `jump-tree-mode' keeps the entire history, you simply need
;; to tell it to switch to a different branch, and then jump-next the changes you
;; want:
;;
;;                                o
;;                                |
;;                                |
;;                                o  (start here, but switch
;;                                |\  to the other branch)
;;                                | \
;;                        (jump-next)  o  o
;;                                |
;;                                |
;;                        (jump-next)  x
;;
;; Now you're on the other branch, if you position and jump-next changes you'll stay on
;; that branch, moving up and down through the buffer states located on that
;; branch. Until you decide to switch branches again, of course.
;;
;; Real position trees might have multiple branches and sub-branches:
;;
;;                                o
;;                            ____|______
;;                           /           \
;;                          o             o
;;                      ____|__         __|
;;                     /    |  \       /   \
;;                    o     o   o     o     x
;;                    |               |
;;                   / \             / \
;;                  o   o           o   o
;;
;; Trying to imagine what Emacs' position would do as you move about such a tree
;; will likely frazzle your brain circuits! But in `jump-tree-mode', you're
;; just moving around this position history tree. Most of the time, you'll
;; probably only need to stay on the most recent branch, in which case it
;; behaves like standard jump-prev/jump-next, and is just as simple to understand. But
;; if you ever need to recover a buffer state on a different branch, the
;; possibility of switching between branches and accessing the full position
;; history is still there.
;;
;;
;;
;; The Jump-Tree Visualizer
;; ========================
;;
;; Actually, it gets better. You don't have to imagine all these tree
;; diagrams, because `jump-tree-mode' includes an jump-tree visualizer which
;; draws them for you! In fact, it draws even better diagrams: it highlights
;; the node representing the current buffer state, it highlights the current
;; branch, and you can toggle the display of time-stamps (by hitting "t") and
;; a diff of the position changes (by hitting "d"). (There's one other tiny
;; difference: the visualizer puts the most recent branch on the left rather
;; than the right.)
;;
;; Bring up the position tree visualizer whenever you want by hitting "C-x u".
;;
;; In the visualizer, the usual keys for moving up and down a buffer instead
;; move up and down the position history tree (e.g. the up and down arrow keys, or
;; "C-n" and "C-p"). The state of the "parent" buffer (the buffer whose position
;; history you are visualizing) is updated as you move around the position tree in
;; the visualizer. If you reach a branch point in the visualizer, the usual
;; keys for moving forward and backward in a buffer instead switch branch
;; (e.g. the left and right arrow keys, or "C-f" and "C-b").
;;
;; Clicking with the mouse on any node in the visualizer will take you
;; directly to that node, resetting the state of the parent buffer to the
;; state represented by that node.
;;
;; You can also select nodes directly using the keyboard, by hitting "s" to
;; toggle selection mode. The usual motion keys now allow you to move around
;; the tree without changing the parent buffer. Hitting <enter> will reset the
;; state of the parent buffer to the state represented by the currently
;; selected node.
;;
;; It can be useful to see how long ago the parent buffer was in the state
;; represented by a particular node in the visualizer. Hitting "t" in the
;; visualizer toggles the display of time-stamps for all the nodes. (Note
;; that, because of the way `jump-tree-mode' works, these time-stamps may be
;; somewhat later than the true times, especially if it's been a long time
;; since you last undid any changes.)

;;; Code:

(eval-when-compile (require 'cl))


;;; =====================================================================
;;;              Compatibility hacks for older Emacsen

;; `characterp' isn't defined in Emacs versions < 23
(unless (fboundp 'characterp)
  (defalias 'characterp 'char-valid-p))

;; `registerv' defstruct isn't defined in Emacs versions < 24
(unless (fboundp 'registerv-make)
  (defmacro registerv-make (data &rest _dummy) data))

(unless (fboundp 'registerv-data)
  (defmacro registerv-data (data) data))

;; `user-error' isn't defined in Emacs < 24.3
(unless (fboundp 'user-error)
  (defalias 'user-error 'error)
  ;; prevent debugger being called on user errors
  (add-to-list 'debug-ignored-errors "^No further position information")
  (add-to-list 'debug-ignored-errors "^No further jump-next information")
  (add-to-list 'debug-ignored-errors "^No further jump-next information for region"))


;;; =====================================================================
;;;              Global variables and customization options

(defgroup jump-tree nil
  "Tree jump-prev/jump-next."
  :group 'jump-tree)

(defvar jump-tree-pos-tree nil
  "Tree of position entries globally.")

(defvar jump-tree-pos-list '()
  "Jump history list, contain POSITION entries '(file-name . pointer).")

(defvar jump-tree-in-progress nil
  "Jump-Tree-Pos-List state.")

(defcustom jump-tree-pos-list-limit 40
  "Max length of jump-tree-pos-list."
  :type 'integer
  :group 'jump-tree)

(defcustom jump-tree-pos-tree-limit 100
  "Max count of jump-tree-tree-list."
  :type 'integer
  :group 'jump-tree)

(defcustom jump-tree-pos-list-hook-commands '(end-of-buffer beginning-of-buffer)
  "Commands to hook."
  :type 'list
  :group 'jump-tree)

(defcustom jump-tree-mode-lighter " Jump-Tree"
  "Lighter displayed in mode line
when `jump-tree-mode' is enabled."
  :group 'jump-tree
  :type 'string)

(defcustom jump-tree-visualizer-relative-timestamps t
  "When non-nil, display times relative to current time
when displaying time stamps in visualizer.

Otherwise, display absolute times."
  :group 'jump-tree
  :type 'boolean)

(defcustom jump-tree-visualizer-timestamps nil
  "When non-nil, display time-stamps by default
in jump-tree visualizer.

\\<jump-tree-visualizer-mode-map>You can always toggle time-stamps on and off \
using \\[jump-tree-visualizer-toggle-timestamps], regardless of the
setting of this variable."
  :group 'jump-tree
  :type 'boolean)

(defcustom jump-tree-visualizer-lazy-drawing 100
  "When non-nil, use lazy jump-tree drawing in visualizer.

Setting this to a number causes the visualizer to switch to lazy
drawing when the number of nodes in the tree is larger than this
value.

Lazy drawing means that only the visible portion of the tree will
be drawn initially, and the tree will be extended later as
needed. For the most part, the only visible effect of this is to
significantly speed up displaying the visualizer for very large
trees.

There is one potential negative effect of lazy drawing. Other
branches of the tree will only be drawn once the node from which
they branch off becomes visible. So it can happen that certain
portions of the tree that would be shown with lazy drawing
disabled, will not be drawn immediately when it is
enabled. However, this effect is quite rare in practice."
  :group 'jump-tree
  :type '(choice (const :tag "never" nil)
                 (const :tag "always" t)
                 (integer :tag "> count")))

(defface jump-tree-visualizer-default-face
  '((((class color)) :foreground "gray"))
  "Face used to draw jump-tree in visualizer."
  :group 'jump-tree)

(defface jump-tree-visualizer-current-face
  '((((class color)) :foreground "red"))
  "Face used to highlight current jump-tree node in visualizer."
  :group 'jump-tree)

(defface jump-tree-visualizer-active-branch-face
  '((((class color) (background dark))
     (:foreground "white" :weight bold))
    (((class color) (background light))
     (:foreground "black" :weight bold)))
  "Face used to highlight active jump-tree branch in visualizer."
  :group 'jump-tree)

(defface jump-tree-visualizer-register-face
  '((((class color)) :foreground "yellow"))
  "Face used to highlight jump-tree nodes saved to a register
in visualizer."
  :group 'jump-tree)

(defvar jump-tree-visualizer-parent-buffer nil
  "Parent buffer in visualizer.")
(put 'jump-tree-visualizer-parent-buffer 'permanent-local t)
(make-variable-buffer-local 'jump-tree-visualizer-parent-buffer)

;; stores modification time of parent buffer's file, if any
(defvar jump-tree-visualizer-parent-mtime nil)
(put 'jump-tree-visualizer-parent-mtime 'permanent-local t)
(make-variable-buffer-local 'jump-tree-visualizer-parent-mtime)

;; stores current horizontal spacing needed for drawing jump-tree
(defvar jump-tree-visualizer-spacing nil)
(put 'jump-tree-visualizer-spacing 'permanent-local t)
(make-variable-buffer-local 'jump-tree-visualizer-spacing)

;; calculate horizontal spacing required for drawing tree with current
;; settings
(defsubst jump-tree-visualizer-calculate-spacing ()
  (if jump-tree-visualizer-timestamps
      (if jump-tree-visualizer-relative-timestamps 9 13)
    3))

;; holds node that was current when visualizer was invoked
(defvar jump-tree-visualizer-initial-node nil)
(put 'jump-tree-visualizer-initial-node 'permanent-local t)
(make-variable-buffer-local 'jump-tree-visualizer-initial-node)

;; holds currently selected node in visualizer selection mode
(defvar jump-tree-visualizer-selected-node nil)
(put 'jump-tree-visualizer-selected-node 'permanent-local t)
(make-variable-buffer-local 'jump-tree-visualizer-selected)

;; used to store nodes at edge of currently drawn portion of tree
(defvar jump-tree-visualizer-needs-extending-down nil)
(put 'jump-tree-visualizer-needs-extending-down 'permanent-local t)
(make-variable-buffer-local 'jump-tree-visualizer-needs-extending-down)
(defvar jump-tree-visualizer-needs-extending-up nil)
(put 'jump-tree-visualizer-needs-extending-up 'permanent-local t)
(make-variable-buffer-local 'jump-tree-visualizer-needs-extending-up)

;; dynamically bound to t when jump-preving from visualizer, to inhibit
;; `jump-tree-kill-visualizer' hook function in parent buffer
(defvar jump-tree-inhibit-kill-visualizer nil)

;; can be let-bound to a face name, used in drawing functions
(defvar jump-tree-insert-face nil)

;; visualizer buffer names
(defconst jump-tree-visualizer-buffer-name " *jump-tree*")


;;; =================================================================
;;;                          Default keymaps

(defvar jump-tree-map nil
  "Keymap used in jump-tree-mode.")

(unless jump-tree-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-,") 'jump-tree-jump-prev)
    (define-key map (kbd "M-.") 'jump-tree-jump-next)
    ;; we use "C-x j" for the jump-tree visualizer
    (define-key map (kbd "\C-x j") 'jump-tree-visualize)
    ;; set keymap
    (setq jump-tree-map map)))

(defvar jump-tree-visualizer-mode-map nil
  "Keymap used in jump-tree visualizer.")

(unless jump-tree-visualizer-mode-map
  (let ((map (make-sparse-keymap)))
    ;; vertical motion keys jump-prev/jump-next
    (define-key map [remap previous-line] 'jump-tree-visualize-jump-prev)
    (define-key map [remap next-line] 'jump-tree-visualize-jump-next)
    (define-key map [up] 'jump-tree-visualize-jump-prev)
    (define-key map "p" 'jump-tree-visualize-jump-prev)
    (define-key map "\C-p" 'jump-tree-visualize-jump-prev)
    (define-key map [down] 'jump-tree-visualize-jump-next)
    (define-key map "n" 'jump-tree-visualize-jump-next)
    (define-key map "\C-n" 'jump-tree-visualize-jump-next)
    ;; horizontal motion keys switch branch
    (define-key map [remap forward-char]
      'jump-tree-visualize-switch-branch-right)
    (define-key map [remap backward-char]
      'jump-tree-visualize-switch-branch-left)
    (define-key map [right] 'jump-tree-visualize-switch-branch-right)
    (define-key map "f" 'jump-tree-visualize-switch-branch-right)
    (define-key map "\C-f" 'jump-tree-visualize-switch-branch-right)
    (define-key map [left] 'jump-tree-visualize-switch-branch-left)
    (define-key map "b" 'jump-tree-visualize-switch-branch-left)
    (define-key map "\C-b" 'jump-tree-visualize-switch-branch-left)
    ;; paragraph motion keys jump-prev/jump-next to significant points in tree
    (define-key map [remap backward-paragraph] 'jump-tree-visualize-jump-prev-to-x)
    (define-key map "\M-{" 'jump-tree-visualize-jump-prev-to-x)
    (define-key map [C-up] 'jump-tree-visualize-jump-prev-to-x)
    ;; mouse sets buffer state to node at click
    (define-key map [mouse-1] 'jump-tree-visualizer-mouse-set)
    ;; toggle selection mode
    (define-key map "s" 'jump-tree-visualizer-selection-mode)
    ;; horizontal scrolling may be needed if the tree is very wide
    (define-key map "," 'jump-tree-visualizer-scroll-left)
    (define-key map "." 'jump-tree-visualizer-scroll-right)
    (define-key map "<" 'jump-tree-visualizer-scroll-left)
    (define-key map ">" 'jump-tree-visualizer-scroll-right)
    ;; vertical scrolling may be needed if the tree is very tall
    (define-key map [next] 'jump-tree-visualizer-scroll-up)
    (define-key map [prior] 'jump-tree-visualizer-scroll-down)
    ;; toggle timestamp
    (define-key map "t" 'jump-tree-visualizer-toggle-timestamps)
    ;; quit/abort visualizer
    (define-key map "q" 'jump-tree-visualizer-quit)
    (define-key map "\C-q" 'jump-tree-visualizer-abort)
    ;; set keymap
    (setq jump-tree-visualizer-mode-map map)))

(defvar jump-tree-visualizer-selection-mode-map nil
  "Keymap used in jump-tree visualizer selection mode.")

(unless jump-tree-visualizer-selection-mode-map
  (let ((map (make-sparse-keymap)))
    ;; vertical motion keys move up and down tree
    (define-key map [remap previous-line]
      'jump-tree-visualizer-select-previous)
    (define-key map [remap next-line]
      'jump-tree-visualizer-select-next)
    (define-key map [up] 'jump-tree-visualizer-select-previous)
    (define-key map "p" 'jump-tree-visualizer-select-previous)
    (define-key map "\C-p" 'jump-tree-visualizer-select-previous)
    (define-key map [down] 'jump-tree-visualizer-select-next)
    (define-key map "n" 'jump-tree-visualizer-select-next)
    (define-key map "\C-n" 'jump-tree-visualizer-select-next)
    ;; vertical scroll keys move up and down quickly
    (define-key map [next]
      (lambda () (interactive) (jump-tree-visualizer-select-next 10)))
    (define-key map [prior]
      (lambda () (interactive) (jump-tree-visualizer-select-previous 10)))
    ;; horizontal motion keys move to left and right siblings
    (define-key map [remap forward-char] 'jump-tree-visualizer-select-right)
    (define-key map [remap backward-char] 'jump-tree-visualizer-select-left)
    (define-key map [right] 'jump-tree-visualizer-select-right)
    (define-key map "f" 'jump-tree-visualizer-select-right)
    (define-key map "\C-f" 'jump-tree-visualizer-select-right)
    (define-key map [left] 'jump-tree-visualizer-select-left)
    (define-key map "b" 'jump-tree-visualizer-select-left)
    (define-key map "\C-b" 'jump-tree-visualizer-select-left)
    ;; horizontal scroll keys move left or right quickly
    (define-key map ","
      (lambda () (interactive) (jump-tree-visualizer-select-left 10)))
    (define-key map "."
      (lambda () (interactive) (jump-tree-visualizer-select-right 10)))
    (define-key map "<"
      (lambda () (interactive) (jump-tree-visualizer-select-left 10)))
    (define-key map ">"
      (lambda () (interactive) (jump-tree-visualizer-select-right 10)))
    ;; <enter> sets buffer state to node at point
    (define-key map "\r" 'jump-tree-visualizer-set)
    ;; mouse selects node at click
    (define-key map [mouse-1] 'jump-tree-visualizer-mouse-select)
    ;; toggle timestamp
    (define-key map "t" 'jump-tree-visualizer-toggle-timestamps)
    ;; set keymap
    (setq jump-tree-visualizer-selection-mode-map map)))


;;; =====================================================================
;;;                     jump-tree data structure

(defstruct
    (jump-tree
     :named
     (:constructor nil)
     (:constructor make-jump-tree
                   (&aux
                    (root (jump-tree-make-node nil nil))
                    (current root)
                    (count 0)))
     ;;(:copier nil)
     )
  root current count)

(defstruct
    (jump-tree-node
     (:type vector)   ; create unnamed struct
     (:constructor nil)
     (:constructor jump-tree-make-node
                   (previous position
                             &aux
                             (timestamp (current-time))
                             (branch 0)))
     (:copier nil))
  previous next position timestamp branch meta-data)

(defmacro jump-tree-node-p (n)
  (let ((len (length (jump-tree-make-node nil nil))))
    `(and (vectorp ,n) (= (length ,n) ,len))))

(defstruct
    (jump-tree-position-data
     (:type vector)   ; create unnamed struct
     (:constructor nil)
     (:constructor jump-tree-make-position-data
                   (&optional file-path pointer))
     (:copier nil))
  file-path pointer)

(defmacro jump-tree-position-data-p (pos)
  (let ((len (length (jump-tree-make-position-data))))
    `(and (vectorp ,pos) (= (length ,pos) ,len))))

(defmacro jump-tree-node-file-path (node)
  `(let ((pos (plist-get (jump-tree-node-meta-data ,node) :position)))
     (when (jump-tree-position-data-p pos)
       (jump-tree-position-data-file-path pos))))

(defmacro jump-tree-node-pointer (node)
  `(let ((pos (plist-get (jump-tree-node-meta-data ,node) :position)))
     (when (jump-tree-position-data-p pos)
       (jump-tree-position-data-pointer pos))))

(defsetf jump-tree-node-file-path (node) (val)
  `(let ((pos (plist-get (jump-tree-node-meta-data ,node) :position)))
     (unless (jump-tree-position-data-p pos)
       (setf (jump-tree-node-meta-data ,node)
             (plist-put (jump-tree-node-meta-data ,node) :position
                        (setq pos (jump-tree-make-position-data)))))
     (setf (jump-tree-position-data-file-path pos) ,val)))

(defsetf jump-tree-node-pointer (node) (val)
  `(let ((pos (plist-get (jump-tree-node-meta-data ,node) :position)))
     (unless (jump-tree-position-data-p pos)
       (setf (jump-tree-node-meta-data ,node)
             (plist-put (jump-tree-node-meta-data ,node) :position
                        (setq pos (jump-tree-make-position-data)))))
     (setf (jump-tree-position-data-pointer pos) ,val)))

(defstruct
    (jump-tree-visualizer-data
     (:type vector)   ; create unnamed struct
     (:constructor nil)
     (:constructor jump-tree-make-visualizer-data
                   (&optional lwidth cwidth rwidth marker))
     (:copier nil))
  lwidth cwidth rwidth marker)

(defmacro jump-tree-visualizer-data-p (v)
  (let ((len (length (jump-tree-make-visualizer-data))))
    `(and (vectorp ,v) (= (length ,v) ,len))))

(defun jump-tree-node-clear-visualizer-data (node)
  (let ((plist (jump-tree-node-meta-data node)))
    (if (eq (car plist) :visualizer)
        (setf (jump-tree-node-meta-data node) (nthcdr 2 plist))
      (while (and plist (not (eq (cadr plist) :visualizer)))
        (setq plist (cdr plist)))
      (if plist (setcdr plist (nthcdr 3 plist))))))

(defmacro jump-tree-node-lwidth (node)
  `(let ((v (plist-get (jump-tree-node-meta-data ,node) :visualizer)))
     (when (jump-tree-visualizer-data-p v)
       (jump-tree-visualizer-data-lwidth v))))

(defmacro jump-tree-node-cwidth (node)
  `(let ((v (plist-get (jump-tree-node-meta-data ,node) :visualizer)))
     (when (jump-tree-visualizer-data-p v)
       (jump-tree-visualizer-data-cwidth v))))

(defmacro jump-tree-node-rwidth (node)
  `(let ((v (plist-get (jump-tree-node-meta-data ,node) :visualizer)))
     (when (jump-tree-visualizer-data-p v)
       (jump-tree-visualizer-data-rwidth v))))

(defmacro jump-tree-node-marker (node)
  `(let ((v (plist-get (jump-tree-node-meta-data ,node) :visualizer)))
     (when (jump-tree-visualizer-data-p v)
       (jump-tree-visualizer-data-marker v))))

(defsetf jump-tree-node-lwidth (node) (val)
  `(let ((v (plist-get (jump-tree-node-meta-data ,node) :visualizer)))
     (unless (jump-tree-visualizer-data-p v)
       (setf (jump-tree-node-meta-data ,node)
             (plist-put (jump-tree-node-meta-data ,node) :visualizer
                        (setq v (jump-tree-make-visualizer-data)))))
     (setf (jump-tree-visualizer-data-lwidth v) ,val)))

(defsetf jump-tree-node-cwidth (node) (val)
  `(let ((v (plist-get (jump-tree-node-meta-data ,node) :visualizer)))
     (unless (jump-tree-visualizer-data-p v)
       (setf (jump-tree-node-meta-data ,node)
             (plist-put (jump-tree-node-meta-data ,node) :visualizer
                        (setq v (jump-tree-make-visualizer-data)))))
     (setf (jump-tree-visualizer-data-cwidth v) ,val)))

(defsetf jump-tree-node-rwidth (node) (val)
  `(let ((v (plist-get (jump-tree-node-meta-data ,node) :visualizer)))
     (unless (jump-tree-visualizer-data-p v)
       (setf (jump-tree-node-meta-data ,node)
             (plist-put (jump-tree-node-meta-data ,node) :visualizer
                        (setq v (jump-tree-make-visualizer-data)))))
     (setf (jump-tree-visualizer-data-rwidth v) ,val)))

(defsetf jump-tree-node-marker (node) (val)
  `(let ((v (plist-get (jump-tree-node-meta-data ,node) :visualizer)))
     (unless (jump-tree-visualizer-data-p v)
       (setf (jump-tree-node-meta-data ,node)
             (plist-put (jump-tree-node-meta-data ,node) :visualizer
                        (setq v (jump-tree-make-visualizer-data)))))
     (setf (jump-tree-visualizer-data-marker v) ,val)))

(defstruct
    (jump-tree-register-data
     (:type vector)
     (:constructor nil)
     (:constructor jump-tree-make-register-data (buffer node)))
  buffer node)

(defun jump-tree-register-data-p (data)
  (and (vectorp data)
       (= (length data) 2)
       (jump-tree-node-p (jump-tree-register-data-node data))))

(defun jump-tree-register-data-print-func (data)
  (princ (format "an jump-tree state for buffer %s"
                 (jump-tree-register-data-buffer data))))

(defmacro jump-tree-node-register (node)
  `(plist-get (jump-tree-node-meta-data ,node) :register))

(defsetf jump-tree-node-register (node) (val)
  `(setf (jump-tree-node-meta-data ,node)
         (plist-put (jump-tree-node-meta-data ,node) :register ,val)))


;;; =====================================================================
;;;              Basic jump-tree data structure functions

(defun jump-tree-grow-backwards (node position)
  "Add new node *above* jump-tree NODE, and return new node.
Note that this will overwrite NODE's \"previous\" link, so should
only be used on a detached NODE, never on nodes that are already
part of `jump-tree-pos-tree'."
  (let ((new (jump-tree-make-node nil position)))
    (setf (jump-tree-node-next new) (list node))
    (setf (jump-tree-node-previous node) new)
    new))

(defun jump-tree-splice-node (node splice)
  "Splice NODE into position tree, below node SPLICE.
Note that this will overwrite NODE's \"next\" and \"previous\"
links, so should only be used on a detached NODE, never on nodes
that are already part of `jump-tree-pos-tree'."
  (setf (jump-tree-node-next node) (jump-tree-node-next splice)
        (jump-tree-node-branch node) (jump-tree-node-branch splice)
        (jump-tree-node-previous node) splice
        (jump-tree-node-next splice) (list node)
        (jump-tree-node-branch splice) 0)
  (dolist (n (jump-tree-node-next node))
    (setf (jump-tree-node-previous n) node)))

(defun jump-tree-snip-node (node)
  "Snip NODE out of position tree."
  (let* ((parent (jump-tree-node-previous node))
         index i)
    ;; if NODE is only child, replace parent's next links with NODE's
    (if (= (length (jump-tree-node-next parent)) 0)
        (setf (jump-tree-node-next parent) (jump-tree-node-next node)
              (jump-tree-node-branch parent) (jump-tree-node-branch node))
      ;; otherwise...
      (setq index (jump-tree-index node (jump-tree-node-next parent)))
      (cond
       ;; if active branch used do go via NODE, set parent's branch to active
       ;; branch of NODE
       ((= (jump-tree-node-branch parent) index)
        (setf (jump-tree-node-branch parent)
              (+ index (jump-tree-node-branch node))))
       ;; if active branch didn't go via NODE, update parent's branch to point
       ;; to same node as before
       ((> (jump-tree-node-branch parent) index)
        (incf (jump-tree-node-branch parent)
              (1- (length (jump-tree-node-next node))))))
      ;; replace NODE in parent's next list with NODE's entire next list
      (if (= index 0)
          (setf (jump-tree-node-next parent)
                (nconc (jump-tree-node-next node)
                       (cdr (jump-tree-node-next parent))))
        (setq i (nthcdr (1- index) (jump-tree-node-next parent)))
        (setcdr i (nconc (jump-tree-node-next node) (cddr i)))))
    ;; update previous links of NODE's children
    (dolist (n (jump-tree-node-next node))
      (setf (jump-tree-node-previous n) parent))))

(defun jump-tree-mapc (func node)
  ;; Apply FUNC to NODE and to each node below it.
  (let ((stack (list node))
        n)
    (while stack
      (setq n (pop stack))
      (funcall func n)
      (setq stack (append (jump-tree-node-next n) stack)))))

(defmacro jump-tree-num-branches ()
  "Return number of branches at current position tree node."
  '(length (jump-tree-node-next (jump-tree-current jump-tree-pos-tree))))

(defun jump-tree-index (node list)
  "Find the first occurrence of NODE in LIST.
Return the index of the matching item, or nil of not found.
Comparison is done with `eq'."
  (let ((i 0))
    (catch 'found
      (while (progn
               (when (eq node (car list)) (throw 'found i))
               (incf i)
               (setq list (cdr list))))
      nil)))


;;; =====================================================================
;;;             position list utility functions
(defun jump-tree-pos-list-discard-invalid ()
  "If the file or buffer is closed, then the marker is invalid. This function will
remove these invalid entries."
  (let ((empty-marker (make-marker)))
    (setq jump-tree-pos-list
          (remove-if (lambda (position) (equal empty-marker (cdr position)))
                     jump-tree-pos-list))))

(defun jump-tree-pos-tree-reset-when-empty-node (node)
  "If the file or buffer is closed, then the marker is invalid. This function will
remove these invalid entries in tree."
  (let ((empty-marker (make-marker))
        (current node)
        oldcurrent next)
    (when (equal empty-marker (cdr (jump-tree-node-position current)))
      (setq jump-tree-pos-tree nil))))

(defun jump-tree-pos-tree-remove-prev-empty (node)
  "If the file or buffer is closed, then the marker is invalid. This function will
remove these invalid entries in tree."
  (let ((empty-marker (make-marker))
        (current node)
        oldcurrent next)
    (while (and (jump-tree-node-previous current)
                (equal empty-marker (cdr (jump-tree-node-position current))))
      (setq oldcurrent current)
      (setq next (nth (jump-tree-node-branch current)
                      (jump-tree-node-next current)))
      (setf (jump-tree-current jump-tree-pos-tree) (jump-tree-node-previous current))
      (setq current (jump-tree-current jump-tree-pos-tree))
      (setf (jump-tree-node-next current) (list next))
      (setf (jump-tree-node-previous oldcurrent) nil)
      (setf (jump-tree-node-next oldcurrent) nil))
    current))

(defun jump-tree-pos-tree-remove-next-empty (node)
  "If the file or buffer is closed, then the marker is invalid. This function will
remove these invalid entries in tree."
  (let ((empty-marker (make-marker))
        (current node)
        oldcurrent prev)
    (while (and (null (jump-tree-node-next current))
                (equal empty-marker (cdr (jump-tree-node-position current))))
      (setq oldcurrent current)
      (setq prev (jump-tree-node-previous current))
      (setq current (nth (jump-tree-node-branch current)
                         (jump-tree-node-next current)))
      (setf (jump-tree-current jump-tree-pos-tree) current)
      (setf (jump-tree-node-next prev) current)
      (setf (jump-tree-node-previous oldcurrent) nil)
      (setf (jump-tree-node-next oldcurrent) nil))
    current))

(defun jump-tree-pos-list-transfer-to-tree ()
  ;; Transfer entries accumulated in `jump-tree-pos-list' to `jump-tree-pos-tree'.

  ;; `jump-tree-pos-list-transfer-to-tree' should never be called when jump is disabled
  ;; (i.e. `jump-tree-pos-tree' is t)
  (assert (not (eq jump-tree-pos-tree t)))

  ;; if `jump-tree-pos-tree' is empty, create initial jump-tree
  (when (null jump-tree-pos-tree) (setq jump-tree-pos-tree (make-jump-tree)))

  (jump-tree-pos-list-discard-invalid)

  (when jump-tree-pos-list
    ;; create new node from first changeset in `jump-tree-pos-list', save old
    ;; `jump-tree-pos-tree' current node, and make new node the current node
    (let* ((node (jump-tree-make-node nil (pop jump-tree-pos-list)))
           (splice (jump-tree-current jump-tree-pos-tree))
           (count 1))
      (setf (jump-tree-current jump-tree-pos-tree) node)
      ;; grow tree fragment backwards
      (while jump-tree-pos-list
        (setq node
              (jump-tree-grow-backwards node (pop jump-tree-pos-list)))
        (incf count))
      ;; build a new branch, number 0.
      (setf (jump-tree-node-previous node) splice)
      (push node (jump-tree-node-next splice))
      (setf (jump-tree-node-branch splice) 0)
      (incf (jump-tree-count jump-tree-pos-tree) count))
    ;; discard position history if necessary
    (jump-tree-discard-history)))

(defun jump-tree-pos-list-rebuild-from-tree ()
  "Rebuild `jump-tree-pos-list' from information in `jump-tree-pos-tree', when some
buffers are closed, and the markers become invalid."
  (unless (eq jump-tree-pos-list t)
    (jump-tree-pos-list-transfer-to-tree)
    (setq jump-tree-pos-list nil)
    (when jump-tree-pos-tree
      (let ((stack (list (list (jump-tree-root jump-tree-pos-tree)))))
        (push (sort (mapcar 'identity (jump-tree-node-next (caar stack)))
                    (lambda (a b)
                      (time-less-p (jump-tree-node-timestamp a)
                                   (jump-tree-node-timestamp b))))
              stack)
        ;; Traverse tree in depth-and-oldest-first order, but add position records
        ;; on the way down, and jump-next records on the way up.
        (while (or (car stack)
                   (not (eq (car (nth 1 stack))
                            (jump-tree-current jump-tree-pos-tree))))
          (if (car stack)
              (progn
                (setq jump-tree-pos-list
                      (append (jump-tree-node-position (caar stack))
                              jump-tree-pos-list))
                (push (sort (mapcar 'identity
                                    (jump-tree-node-next (caar stack)))
                            (lambda (a b)
                              (time-less-p (jump-tree-node-timestamp a)
                                           (jump-tree-node-timestamp b))))
                      stack))
            (pop stack)
            (pop (car stack))))))))


;;; =====================================================================
;;;                History discarding utility functions

(defun jump-tree-oldest-leaf (node)
  ;; Return oldest leaf node below NODE.
  (while (jump-tree-node-next node)
    (setq node
          (car (sort (mapcar 'identity (jump-tree-node-next node))
                     (lambda (a b)
                       (time-less-p (jump-tree-node-timestamp a)
                                    (jump-tree-node-timestamp b)))))))
  node)

(defun jump-tree-discard-node (node)
  ;; Discard NODE from `jump-tree-pos-tree', and return next in line for
  ;; discarding.

  ;; don't discard current node
  (unless (eq node (jump-tree-current jump-tree-pos-tree))

    ;; discarding root node...
    (if (eq node (jump-tree-root jump-tree-pos-tree))
        (cond
         ;; should always discard branches before root
         ((> (length (jump-tree-node-next node)) 1)
          (error "Trying to discard jump-tree root which still\
 has multiple branches"))
         ;; don't discard root if current node is only child
         ((eq (car (jump-tree-node-next node))
              (jump-tree-current jump-tree-pos-tree))
          nil)
         ;; discard root
         (t
          ;; clear any register referring to root
          (let ((pos (jump-tree-node-register node)))
            (when (and pos (eq (get-register pos) node))
              (set-register pos nil)))
          ;; make child of root into new root
          (setq node (setf (jump-tree-root jump-tree-pos-tree)
                           (car (jump-tree-node-next node))))
          (decf (jump-tree-count jump-tree-pos-tree))
          ;; discard new root's position data and PREVIOUS link
          (setf (jump-tree-node-position node) nil
                (jump-tree-node-previous node) nil)
          ;; if new root has branches, or new root is current node, next node
          ;; to discard is oldest leaf, otherwise it's new root
          (if (or (> (length (jump-tree-node-next node)) 1)
                  (eq (car (jump-tree-node-next node))
                      (jump-tree-current jump-tree-pos-tree)))
              (jump-tree-oldest-leaf node)
            node)))

      ;; discarding leaf node...
      (let* ((parent (jump-tree-node-previous node))
             (current (nth (jump-tree-node-branch parent)
                           (jump-tree-node-next parent))))
        ;; clear any register referring to the discarded node
        (let ((pos (jump-tree-node-register node)))
          (when (and pos (eq (get-register pos) node))
            (set-register pos nil)))
        (decf (jump-tree-count jump-tree-pos-tree))
        ;; discard leaf
        (setf (jump-tree-node-next parent)
              (delq node (jump-tree-node-next parent))
              (jump-tree-node-branch parent)
              (jump-tree-index current (jump-tree-node-next parent)))
        ;; if parent has branches, or parent is current node, next node to
        ;; discard is oldest leaf, otherwise it's the parent itself
        (if (or (eq parent (jump-tree-current jump-tree-pos-tree))
                (and (jump-tree-node-next parent)
                     (or (not (eq parent (jump-tree-root jump-tree-pos-tree)))
                         (> (length (jump-tree-node-next parent)) 1))))
            (jump-tree-oldest-leaf parent)
          parent)))))

(defun jump-tree-discard-history ()
  "Discard position history until we're within memory usage limits
set by `jump-tree-pos-tree-limit'."

  (when (> (jump-tree-count jump-tree-pos-tree) jump-tree-pos-tree-limit)
    ;; if there are no branches off root, first node to discard is root;
    ;; otherwise it's leaf node at botom of oldest branch
    (let ((node (if (> (length (jump-tree-node-next
                                (jump-tree-root jump-tree-pos-tree))) 1)
                    (jump-tree-oldest-leaf (jump-tree-root jump-tree-pos-tree))
                  (jump-tree-root jump-tree-pos-tree))))

      ;; discard nodes until next node to discard would bring memory use
      ;; within `jump-tree-pos-tree-limit'
      (while (and node
                  (> (jump-tree-count jump-tree-pos-tree) jump-tree-pos-tree-limit))
        (setq node (jump-tree-discard-node node))))))


;;; =====================================================================
;;;                   Visualizer utility functions

(defun jump-tree-compute-widths (node)
  "Recursively compute widths for nodes below NODE."
  (let ((stack (list node))
        res)
    (while stack
      ;; try to compute widths for node at top of stack
      (if (jump-tree-node-p
           (setq res (jump-tree-node-compute-widths (car stack))))
          ;; if computation fails, it returns a node whose widths still need
          ;; computing, which we push onto the stack
          (push res stack)
        ;; otherwise, store widths and remove it from stack
        (setf (jump-tree-node-lwidth (car stack)) (aref res 0)
              (jump-tree-node-cwidth (car stack)) (aref res 1)
              (jump-tree-node-rwidth (car stack)) (aref res 2))
        (pop stack)))))

(defun jump-tree-node-compute-widths (node)
  ;; Compute NODE's left-, centre-, and right-subtree widths. Returns widths
  ;; (in a vector) if successful. Otherwise, returns a node whose widths need
  ;; calculating before NODE's can be calculated.
  (let ((num-children (length (jump-tree-node-next node)))
        (lwidth 0) (cwidth 0) (rwidth 0) p)
    (catch 'need-widths
      (cond
       ;; leaf nodes have 0 width
       ((= 0 num-children)
        (setf cwidth 1
              (jump-tree-node-lwidth node) 0
              (jump-tree-node-cwidth node) 1
              (jump-tree-node-rwidth node) 0))

       ;; odd number of children
       ((= (mod num-children 2) 1)
        (setq p (jump-tree-node-next node))
        ;; compute left-width
        (dotimes (i (/ num-children 2))
          (if (jump-tree-node-lwidth (car p))
              (incf lwidth (+ (jump-tree-node-lwidth (car p))
                              (jump-tree-node-cwidth (car p))
                              (jump-tree-node-rwidth (car p))))
            ;; if child's widths haven't been computed, return that child
            (throw 'need-widths (car p)))
          (setq p (cdr p)))
        (if (jump-tree-node-lwidth (car p))
            (incf lwidth (jump-tree-node-lwidth (car p)))
          (throw 'need-widths (car p)))
        ;; centre-width is inherited from middle child
        (setf cwidth (jump-tree-node-cwidth (car p)))
        ;; compute right-width
        (incf rwidth (jump-tree-node-rwidth (car p)))
        (setq p (cdr p))
        (dotimes (i (/ num-children 2))
          (if (jump-tree-node-lwidth (car p))
              (incf rwidth (+ (jump-tree-node-lwidth (car p))
                              (jump-tree-node-cwidth (car p))
                              (jump-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p))))

       ;; even number of children
       (t
        (setq p (jump-tree-node-next node))
        ;; compute left-width
        (dotimes (i (/ num-children 2))
          (if (jump-tree-node-lwidth (car p))
              (incf lwidth (+ (jump-tree-node-lwidth (car p))
                              (jump-tree-node-cwidth (car p))
                              (jump-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p)))
        ;; centre-width is 0 when number of children is even
        (setq cwidth 0)
        ;; compute right-width
        (dotimes (i (/ num-children 2))
          (if (jump-tree-node-lwidth (car p))
              (incf rwidth (+ (jump-tree-node-lwidth (car p))
                              (jump-tree-node-cwidth (car p))
                              (jump-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p)))))

      ;; return left-, centre- and right-widths
      (vector lwidth cwidth rwidth))))

(defun jump-tree-clear-visualizer-data (tree)
  ;; Clear visualizer data below NODE.
  (jump-tree-mapc
   (lambda (n) (jump-tree-node-clear-visualizer-data n))
   (jump-tree-root tree)))


;;; =====================================================================
;;;                        jump-tree commands

;;;###autoload
(define-minor-mode jump-tree-mode
  "Toggle jump-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.
jump-tree-mode replaces Emacs' standard position feature with a more
powerful yet easier to use version, that treats the position history
as what it is: a tree.
The following keys are available in `jump-tree-mode':
  \\{jump-tree-map}
Within the jump-tree visualizer, the following keys are available:
  \\{jump-tree-visualizer-mode-map}"
  nil                       ; init value
  jump-tree-mode-lighter    ; lighter
  jump-tree-map             ; keymap
  (when (not jump-tree-mode)
    (setq jump-tree-pos-list nil)
    (setq jump-tree-pos-tree nil)))

(defun turn-on-jump-tree-mode (&optional print-message)
  "Enable `jump-tree-mode' in the current buffer, set the keybindings in  global map."
  (interactive "p")
  (jump-tree-mode 1)
  (jump-tree-overridden-jump-bindings-p))

(defun jump-tree-overridden-jump-bindings-p ()
  (global-set-key (kbd "C-x j") 'jump-tree-visualize)
  (global-set-key [?\M-,] 'jump-tree-jump-prev)
  (global-set-key [?\M-.] 'jump-tree-jump-next))

;;;###autoload
(define-globalized-minor-mode global-jump-tree-mode
  jump-tree-mode turn-on-jump-tree-mode)

(defun jump-tree-jump-prev (&optional arg)
  "Jump-Prev changes.
Repeat this command to position more changes.
A numeric ARG serves as a repeat count.
In Transient Mark mode when the mark is active, only position changes
within the current region. Similarly, when not in Transient Mark
mode, just \\[universal-argument] as an argument limits position to
changes within the current region."
  (interactive "*P")
  (unless jump-tree-mode
    (user-error "jump-tree mode not enabled in buffer"))
  ;; throw error if position is disabled in buffer
  (when (eq jump-tree-pos-list t)
    (user-error "No position information in this buffer"))
  (jump-tree-jump-prev-1 arg)
  ;; inform user if at branch point
  (when (> (jump-tree-num-branches) 1) (message "Jump-Prev branch point!")))

(defun jump-tree-jump-prev-1 (&optional arg)
  ;; Internal position function.
  (setq deactivate-mark t)
  (let ((jump-tree-in-progress t)
        pos current)
    ;; transfer entries accumulated in `jump-tree-pos-list' to
    ;; `jump-tree-pos-tree'
    (jump-tree-pos-list-transfer-to-tree)
    (dotimes (i (or (and (numberp arg) (prefix-numeric-value arg)) 1))
      ;; check if at top of position tree
      (setq current (jump-tree-current jump-tree-pos-tree))
      (unless (jump-tree-node-previous current)
        (user-error "No further position information"))
      (setq current (jump-tree-node-previous current))
      (setf (jump-tree-current jump-tree-pos-tree) current)

      (print (cdr (jump-tree-node-position current)))

      (jump-tree-pos-list--do-jump (jump-tree-node-position current)))))

(defun jump-tree-jump-next (&optional arg)
  "Jump-Next changes. A numeric ARG serves as a repeat count.
In Transient Mark mode when the mark is active, only jump-next changes
within the current region. Similarly, when not in Transient Mark
mode, just \\[universal-argument] as an argument limits jump-next to
changes within the current region."
  (interactive "*P")
  (unless jump-tree-mode
    (user-error "jump-tree mode not enabled in buffer"))
  ;; throw error if position is disabled in buffer
  (when (eq jump-tree-pos-list t)
    (user-error "No position information in this buffer"))
  (jump-tree-jump-next-1 arg)
  ;; inform user if at branch point
  (when (> (jump-tree-num-branches) 1) (message "Jump-Prev branch point!")))

(defun jump-tree-jump-next-1 (&optional arg)
  ;; Internal jump-next function.
  (setq deactivate-mark t)
  (let ((jump-tree-in-progress t)
        pos current)
    ;; transfer entries accumulated in `jump-tree-pos-list' to
    ;; `jump-tree-pos-tree'
    (jump-tree-pos-list-transfer-to-tree)
    (dotimes (i (or (and (numberp arg) (prefix-numeric-value arg)) 1))
      ;; check if at bottom of position tree
      (setq current (jump-tree-current jump-tree-pos-tree))
      (when (null (jump-tree-node-next current))
        (user-error "No further jump-next information"))
      (setq current (nth (jump-tree-node-branch current)
                         (jump-tree-node-next current)))
      (setf (jump-tree-current jump-tree-pos-tree) current)

      (print (cdr (jump-tree-node-position current)))

      (jump-tree-pos-list--do-jump (jump-tree-node-position current)))))

(defun jump-tree-switch-branch (branch)
  "Switch to a different BRANCH of the position tree.
This will affect which branch to descend when *jump-nexting* changes
using `jump-tree-jump-next'."
  (interactive (list (or (and prefix-arg (prefix-numeric-value prefix-arg))
                         (and (not (eq jump-tree-pos-list t))
                              (or (jump-tree-pos-list-transfer-to-tree) t)
                              (let ((b (jump-tree-node-branch
                                        (jump-tree-current
                                         jump-tree-pos-tree))))
                                (cond
                                 ;; switch to other branch if only 2
                                 ((= (jump-tree-num-branches) 2) (- 1 b))
                                 ;; prompt if more than 2
                                 ((> (jump-tree-num-branches) 2)
                                  (read-number
                                   (format "Branch (0-%d, on %d): "
                                           (1- (jump-tree-num-branches)) b)))
                                 ))))))
  (unless jump-tree-mode
    (user-error "jump-tree mode not enabled in buffer"))
  ;; throw error if position is disabled in buffer
  (when (eq jump-tree-pos-list t)
    (user-error "No position information in this buffer"))
  ;; sanity check branch number
  (when (<= (jump-tree-num-branches) 1)
    (user-error "Not at position branch point"))
  (when (or (< branch 0) (> branch (1- (jump-tree-num-branches))))
    (user-error "Invalid branch number"))
  ;; transfer entries accumulated in `jump-tree-pos-list' to `jump-tree-pos-tree'
  (jump-tree-pos-list-transfer-to-tree)
  ;; switch branch
  (setf (jump-tree-node-branch (jump-tree-current jump-tree-pos-tree))
        branch)
  (message "Switched to branch %d" branch))

(defun jump-tree-set (node)
  ;; Set buffer to state corresponding to NODE. Returns intersection point
  ;; between path back from current node and path back from selected NODE.
  (let ((path (make-hash-table :test 'eq))
        (n node))
    (puthash (jump-tree-root jump-tree-pos-tree) t path)
    ;; build list of nodes leading back from selected node to root, updating
    ;; branches as we go to point down to selected node
    (while (progn
             (puthash n t path)
             (when (jump-tree-node-previous n)
               (setf (jump-tree-node-branch (jump-tree-node-previous n))
                     (jump-tree-index
                      n (jump-tree-node-next (jump-tree-node-previous n))))
               (setq n (jump-tree-node-previous n)))))
    ;; work backwards from current node until we intersect path back from
    ;; selected node
    (setq n (jump-tree-current jump-tree-pos-tree))
    (while (not (gethash n path))
      (setq n (jump-tree-node-previous n)))
    ;; ascend tree until intersection node
    (while (not (eq (jump-tree-current jump-tree-pos-tree) n))
      (jump-tree-jump-prev-1 nil nil))
    ;; descend tree until selected node
    (while (not (eq (jump-tree-current jump-tree-pos-tree) node))
      (jump-tree-jump-next-1 nil nil))
    n))  ; return intersection node

(defun jump-tree-save-state-to-register (register)
  "Store current jump-tree state to REGISTER.
The saved state can be restored using
`jump-tree-restore-state-from-register'.
Argument is a character, naming the register."
  (interactive "cjump-tree state to register: ")
  (unless jump-tree-mode
    (user-error "jump-tree mode not enabled in buffer"))
  ;; throw error if position is disabled in buffer
  (when (eq jump-tree-pos-list t)
    (user-error "No position information in this buffer"))
  ;; transfer entries accumulated in `jump-tree-pos-list' to `jump-tree-pos-tree'
  (jump-tree-pos-list-transfer-to-tree)
  ;; save current node to REGISTER
  (set-register
   register (registerv-make
             (jump-tree-make-register-data
              (current-buffer) (jump-tree-current jump-tree-pos-tree))
             :print-func 'jump-tree-register-data-print-func))
  ;; record REGISTER in current node, for visualizer
  (setf (jump-tree-node-register (jump-tree-current jump-tree-pos-tree))
        register))

(defun jump-tree-restore-state-from-register (register)
  "Restore jump-tree state from REGISTER.
The state must be saved using `jump-tree-save-state-to-register'.
Argument is a character, naming the register."
  (interactive "*cRestore jump-tree state from register: ")
  (unless jump-tree-mode
    (user-error "jump-tree mode not enabled in buffer"))
  ;; throw error if position is disabled in buffer, or if register doesn't contain
  ;; an jump-tree node
  (let ((data (registerv-data (get-register register))))
    (cond
     ((eq jump-tree-pos-list t)
      (user-error "No position information in this buffer"))
     ((not (jump-tree-register-data-p data))
      (user-error "Register doesn't contain jump-tree state"))
     ((not (eq (current-buffer) (jump-tree-register-data-buffer data)))
      (user-error "Register contains jump-tree state for a different buffer")))
    ;; transfer entries accumulated in `jump-tree-pos-list' to `jump-tree-pos-tree'
    (jump-tree-pos-list-transfer-to-tree)
    ;; restore buffer state corresponding to saved node
    (jump-tree-set (jump-tree-register-data-node data))))


;;; =====================================================================
;;;                    Visualizer drawing functions
(defun jump-tree-visualize ()
  "Visualize the current buffer's position tree."
  (interactive "*")
  (unless jump-tree-mode
    (user-error "jump-tree mode not enabled in buffer"))
  (deactivate-mark)
  ;; throw error if position is disabled in buffer
  (when (eq jump-tree-pos-list t)
    (user-error "No position information in this buffer"))
  ;; transfer entries accumulated in `jump-tree-pos-list' to `jump-tree-pos-tree'
  (jump-tree-pos-list-transfer-to-tree)
  ;; add hook to kill visualizer buffer if original buffer is changed
  (add-hook 'before-change-functions 'jump-tree-kill-visualizer nil t)
  ;; prepare *jump-tree* buffer, then draw tree in it
  (let ((jump-tree jump-tree-pos-tree)
        (buff (current-buffer))
        (display-buffer-mark-dedicated 'soft))
    (switch-to-buffer-other-window
     (get-buffer-create jump-tree-visualizer-buffer-name))
    (setq jump-tree-visualizer-parent-buffer buff)
    (setq jump-tree-visualizer-parent-mtime
          (and (buffer-file-name buff)
               (nth 5 (file-attributes (buffer-file-name buff)))))
    (setq jump-tree-visualizer-initial-node (jump-tree-current jump-tree))
    (setq jump-tree-visualizer-spacing
          (jump-tree-visualizer-calculate-spacing))
    (make-local-variable 'jump-tree-visualizer-timestamps)
    (setq jump-tree-pos-tree jump-tree)
    (jump-tree-visualizer-mode)
    ;; FIXME; don't know why `jump-tree-visualizer-mode' clears this
    (setq jump-tree-pos-tree jump-tree)
    (set (make-local-variable 'jump-tree-visualizer-lazy-drawing)
         (or (eq jump-tree-visualizer-lazy-drawing t)
             (and (numberp jump-tree-visualizer-lazy-drawing)
                  (>= (jump-tree-count jump-tree)
                      jump-tree-visualizer-lazy-drawing))))
    (let ((inhibit-read-only t)) (jump-tree-draw-tree jump-tree))))

(defun jump-tree-kill-visualizer (&rest _dummy)
  ;; Kill visualizer. Added to `before-change-functions' hook of original
  ;; buffer when visualizer is invoked.
  (unless (or jump-tree-inhibit-kill-visualizer
              (null (get-buffer jump-tree-visualizer-buffer-name)))
    (with-current-buffer jump-tree-visualizer-buffer-name
      (jump-tree-visualizer-quit))))

(defun jump-tree-draw-tree (jump-tree)
  ;; Draw jump-tree in current buffer starting from NODE (or root if nil).
  (let ((node (if jump-tree-visualizer-lazy-drawing
                  (jump-tree-current jump-tree)
                (jump-tree-root jump-tree))))
    (erase-buffer)
    (setq jump-tree-visualizer-needs-extending-down nil
          jump-tree-visualizer-needs-extending-up nil)
    (jump-tree-clear-visualizer-data jump-tree)
    (jump-tree-compute-widths node)
    ;; lazy drawing starts vertically centred and displaced horizontally to
    ;; the left (window-width/4), since trees will typically grow right
    (if jump-tree-visualizer-lazy-drawing
        (progn
          (jump-tree-move-down (/ (window-height) 2))
          (jump-tree-move-forward (max 2 (/ (window-width) 4)))) ; left margin
      ;; non-lazy drawing starts in centre at top of buffer
      (jump-tree-move-down 1)  ; top margin
      (jump-tree-move-forward
       (max (/ (window-width) 2)
            (+ (jump-tree-node-char-lwidth node)
               ;; add space for left part of left-most time-stamp
               (if jump-tree-visualizer-timestamps
                   (/ (- jump-tree-visualizer-spacing 4) 2)
                 0)
               2))))  ; left margin
    ;; link starting node to its representation in visualizer
    (setf (jump-tree-node-marker node) (make-marker))
    (set-marker-insertion-type (jump-tree-node-marker node) nil)
    (move-marker (jump-tree-node-marker node) (point))
    ;; draw jump-tree
    (let ((jump-tree-insert-face 'jump-tree-visualizer-default-face)
          node-list)
      (if (not jump-tree-visualizer-lazy-drawing)
          (jump-tree-extend-down node t)
        (jump-tree-extend-down node)
        (jump-tree-extend-up node)
        (setq node-list jump-tree-visualizer-needs-extending-down
              jump-tree-visualizer-needs-extending-down nil)
        (while node-list (jump-tree-extend-down (pop node-list)))))
    ;; highlight active branch
    (let ((jump-tree-insert-face 'jump-tree-visualizer-active-branch-face))
      (jump-tree-highlight-active-branch
       (or jump-tree-visualizer-needs-extending-up
           (jump-tree-root jump-tree))))
    ;; highlight current node
    (jump-tree-draw-node (jump-tree-current jump-tree) 'current)))

(defun jump-tree-extend-down (node &optional bottom)
  ;; Extend tree downwards starting from NODE and point. If BOTTOM is t,
  ;; extend all the way down to the leaves. If BOTTOM is a node, extend down
  ;; as far as that node. If BOTTOM is an integer, extend down as far as that
  ;; line. Otherwise, only extend visible portion of tree. NODE is assumed to
  ;; already have a node marker. Returns non-nil if anything was actually
  ;; extended.
  (let ((extended nil)
        (cur-stack (list node))
        next-stack)
    ;; don't bother extending if BOTTOM specifies an already-drawn node
    (unless (and (jump-tree-node-p bottom) (jump-tree-node-marker bottom))
      ;; draw nodes layer by layer
      (while (or cur-stack
                 (prog1 (setq cur-stack next-stack)
                   (setq next-stack nil)))
        (setq node (pop cur-stack))
        ;; if node is within range being drawn...
        (if (or (eq bottom t)
                (and (jump-tree-node-p bottom)
                     (not (eq (jump-tree-node-previous node) bottom)))
                (and (integerp bottom)
                     (>= bottom (line-number-at-pos
                                 (jump-tree-node-marker node))))
                (and (null bottom)
                     (pos-visible-in-window-p (jump-tree-node-marker node)
                                              nil t)))
            ;; ...draw one layer of node's subtree (if not already drawn)
            (progn
              (unless (and (jump-tree-node-next node)
                           (jump-tree-node-marker
                            (nth (jump-tree-node-branch node)
                                 (jump-tree-node-next node))))
                (goto-char (jump-tree-node-marker node))
                (jump-tree-draw-subtree node)
                (setq extended t))
              (setq next-stack
                    (append (jump-tree-node-next node) next-stack)))
          ;; ...otherwise, postpone drawing until later
          (push node jump-tree-visualizer-needs-extending-down))))
    extended))

(defun jump-tree-extend-up (node &optional top)
  ;; Extend tree upwards starting from NODE. If TOP is t, extend all the way
  ;; to root. If TOP is a node, extend up as far as that node. If TOP is an
  ;; integer, extend up as far as that line. Otherwise, only extend visible
  ;; portion of tree. NODE is assumed to already have a node marker. Returns
  ;; non-nil if anything was actually extended.
  (let ((extended nil) parent)
    ;; don't bother extending if TOP specifies an already-drawn node
    (unless (and (jump-tree-node-p top) (jump-tree-node-marker top))
      (while node
        (setq parent (jump-tree-node-previous node))
        ;; if we haven't reached root...
        (if parent
            ;; ...and node is within range being drawn...
            (if (or (eq top t)
                    (and (jump-tree-node-p top) (not (eq node top)))
                    (and (integerp top)
                         (< top (line-number-at-pos
                                 (jump-tree-node-marker node))))
                    (and (null top)
                         ;; NOTE: we check point in case window-start is outdated
                         (< (min (line-number-at-pos (point))
                                 (line-number-at-pos (window-start)))
                            (line-number-at-pos
                             (jump-tree-node-marker node)))))
                ;; ...and it hasn't already been drawn
                (when (not (jump-tree-node-marker parent))
                  ;; link parent node to its representation in visualizer
                  (jump-tree-compute-widths parent)
                  (jump-tree-move-to-parent node)
                  (setf (jump-tree-node-marker parent) (make-marker))
                  (set-marker-insertion-type
                   (jump-tree-node-marker parent) nil)
                  (move-marker (jump-tree-node-marker parent) (point))
                  ;; draw subtree beneath parent
                  (setq jump-tree-visualizer-needs-extending-down
                        (nconc (delq node (jump-tree-draw-subtree parent))
                               jump-tree-visualizer-needs-extending-down))
                  (setq extended t))
              ;; ...otherwise, postpone drawing for later and exit
              (setq jump-tree-visualizer-needs-extending-up (when parent node)
                    parent nil))
          ;; if we've reached root, stop extending and add top margin
          (setq jump-tree-visualizer-needs-extending-up nil)
          (goto-char (jump-tree-node-marker node))
          (jump-tree-move-up 1)  ; top margin
          (delete-region (point-min) (line-beginning-position)))
        ;; next iteration
        (setq node parent)))
    extended))

(defun jump-tree-expand-down (from &optional to)
  ;; Expand tree downwards. FROM is the node to start expanding from. Stop
  ;; expanding at TO if specified. Otherwise, just expand visible portion of
  ;; tree and highlight active branch from FROM.
  (when jump-tree-visualizer-needs-extending-down
    (let ((inhibit-read-only t)
          node-list extended)
      ;; extend down as far as TO node
      (when to
        (setq extended (jump-tree-extend-down from to))
        (goto-char (jump-tree-node-marker to))
        (redisplay t))  ; force redisplay to scroll buffer if necessary
      ;; extend visible portion of tree downwards
      (setq node-list jump-tree-visualizer-needs-extending-down
            jump-tree-visualizer-needs-extending-down nil)
      (when node-list
        (dolist (n node-list)
          (when (jump-tree-extend-down n) (setq extended t)))
        ;; highlight active branch in newly-extended-down portion, if any
        (when extended
          (let ((jump-tree-insert-face
                 'jump-tree-visualizer-active-branch-face))
            (jump-tree-highlight-active-branch from)))))))

(defun jump-tree-expand-up (from &optional to)
  ;; Expand tree upwards. FROM is the node to start expanding from, TO is the
  ;; node to stop expanding at. If TO node isn't specified, just expand visible
  ;; portion of tree and highlight active branch down to FROM.
  (when jump-tree-visualizer-needs-extending-up
    (let ((inhibit-read-only t)
          extended node-list)
      ;; extend up as far as TO node
      (when to
        (setq extended (jump-tree-extend-up from to))
        (goto-char (jump-tree-node-marker to))
        ;; simulate auto-scrolling if close to top of buffer
        (when (<= (line-number-at-pos (point)) scroll-margin)
          (jump-tree-move-up (if (= scroll-conservatively 0)
                                 (/ (window-height) 2) 3))
          (when (jump-tree-extend-up to) (setq extended t))
          (goto-char (jump-tree-node-marker to))
          (unless (= scroll-conservatively 0) (recenter scroll-margin))))
      ;; extend visible portion of tree upwards
      (and jump-tree-visualizer-needs-extending-up
           (jump-tree-extend-up jump-tree-visualizer-needs-extending-up)
           (setq extended t))
      ;; extend visible portion of tree downwards
      (setq node-list jump-tree-visualizer-needs-extending-down
            jump-tree-visualizer-needs-extending-down nil)
      (dolist (n node-list) (jump-tree-extend-down n))
      ;; highlight active branch in newly-extended-up portion, if any
      (when extended
        (let ((jump-tree-insert-face
               'jump-tree-visualizer-active-branch-face))
          (jump-tree-highlight-active-branch
           (or jump-tree-visualizer-needs-extending-up
               (jump-tree-root jump-tree-pos-tree))
           from))))))

(defun jump-tree-highlight-active-branch (node &optional end)
  ;; Draw highlighted active branch below NODE in current buffer. Stop
  ;; highlighting at END node if specified.
  (let ((stack (list node)))
    ;; draw active branch
    (while stack
      (setq node (pop stack))
      (unless (or (eq node end)
                  (memq node jump-tree-visualizer-needs-extending-down))
        (goto-char (jump-tree-node-marker node))
        (setq node (jump-tree-draw-subtree node 'active)
              stack (nconc stack node))))))

(defun jump-tree-draw-node (node &optional current)
  ;; Draw symbol representing NODE in visualizer. If CURRENT is non-nil, node
  ;; is current node.
  (goto-char (jump-tree-node-marker node))
  (when jump-tree-visualizer-timestamps
    (jump-tree-move-backward (/ jump-tree-visualizer-spacing 2)))
  (let* ((jump-tree-insert-face (and jump-tree-insert-face
                                     (or (and (consp jump-tree-insert-face)
                                              jump-tree-insert-face)
                                         (list jump-tree-insert-face))))
         (register (jump-tree-node-register node))
         node-string)
    ;; check node's register (if any) still stores appropriate jump-tree state
    (unless (and register
                 (jump-tree-register-data-p
                  (registerv-data (get-register register)))
                 (eq node (jump-tree-register-data-node
                           (registerv-data (get-register register)))))
      (setq register nil))
    ;; represent node by different symbols, depending on whether it's the
    ;; current node, is saved in a register
    ;; buffer
    (setq node-string
          (cond
           (jump-tree-visualizer-timestamps
            (jump-tree-timestamp-to-string
             (jump-tree-node-timestamp node)
             jump-tree-visualizer-relative-timestamps
             current register))
           (register (char-to-string register))
           (current "x")
           (t "o"))
          jump-tree-insert-face
          (nconc
           (cond
            (current    '(jump-tree-visualizer-current-face))
            (register   '(jump-tree-visualizer-register-face)))
           jump-tree-insert-face))
    ;; draw node and link it to its representation in visualizer
    (jump-tree-insert node-string)
    (jump-tree-move-backward (if jump-tree-visualizer-timestamps
                                 (1+ (/ jump-tree-visualizer-spacing 2))
                               1))
    (move-marker (jump-tree-node-marker node) (point))
    (put-text-property (point) (1+ (point)) 'jump-tree-node node)))

(defun jump-tree-draw-subtree (node &optional active-branch)
  ;; Draw subtree rooted at NODE. The subtree will start from point.
  ;; If ACTIVE-BRANCH is non-nil, just draw active branch below NODE. Returns
  ;; list of nodes below NODE.
  (let ((num-children (length (jump-tree-node-next node)))
        node-list pos trunk-pos n)
    ;; draw node itself
    (jump-tree-draw-node node)
    (cond
     ;; if we're at a leaf node, we're done
     ((= num-children 0))
     ;; if node has only one child, draw it (not strictly necessary to deal
     ;; with this case separately, but as it's by far the most common case
     ;; this makes the code clearer and more efficient)
     ((= num-children 1)
      (jump-tree-move-down 1)
      (jump-tree-insert ?|)
      (jump-tree-move-backward 1)
      (jump-tree-move-down 1)
      (jump-tree-insert ?|)
      (jump-tree-move-backward 1)
      (jump-tree-move-down 1)
      (setq n (car (jump-tree-node-next node)))
      ;; link next node to its representation in visualizer
      (unless (markerp (jump-tree-node-marker n))
        (setf (jump-tree-node-marker n) (make-marker))
        (set-marker-insertion-type (jump-tree-node-marker n) nil))
      (move-marker (jump-tree-node-marker n) (point))
      ;; add next node to list of nodes to draw next
      (push n node-list))
     ;; if node has multiple children, draw branches
     (t
      (jump-tree-move-down 1)
      (jump-tree-insert ?|)
      (jump-tree-move-backward 1)
      (move-marker (setq trunk-pos (make-marker)) (point))
      ;; left subtrees
      (jump-tree-move-backward
       (- (jump-tree-node-char-lwidth node)
          (jump-tree-node-char-lwidth
           (car (jump-tree-node-next node)))))
      (move-marker (setq pos (make-marker)) (point))
      (setq n (cons nil (jump-tree-node-next node)))
      (dotimes (i (/ num-children 2))
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (jump-tree-node-branch node)
                           (jump-tree-node-next node))))
          (jump-tree-move-forward 2)
          (jump-tree-insert ?_ (- trunk-pos pos 2))
          (goto-char pos)
          (jump-tree-move-forward 1)
          (jump-tree-move-down 1)
          (jump-tree-insert ?/)
          (jump-tree-move-backward 2)
          (jump-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (jump-tree-node-marker (car n)))
            (setf (jump-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (jump-tree-node-marker (car n)) nil))
          (move-marker (jump-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (goto-char pos)
        (jump-tree-move-forward
         (+ (jump-tree-node-char-rwidth (car n))
            (jump-tree-node-char-lwidth (cadr n))
            jump-tree-visualizer-spacing 1))
        (move-marker pos (point)))
      ;; middle subtree (only when number of children is odd)
      (when (= (mod num-children 2) 1)
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (jump-tree-node-branch node)
                           (jump-tree-node-next node))))
          (jump-tree-move-down 1)
          (jump-tree-insert ?|)
          (jump-tree-move-backward 1)
          (jump-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (jump-tree-node-marker (car n)))
            (setf (jump-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (jump-tree-node-marker (car n)) nil))
          (move-marker (jump-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (goto-char pos)
        (jump-tree-move-forward
         (+ (jump-tree-node-char-rwidth (car n))
            (if (cadr n) (jump-tree-node-char-lwidth (cadr n)) 0)
            jump-tree-visualizer-spacing 1))
        (move-marker pos (point)))
      ;; right subtrees
      (move-marker trunk-pos (1+ trunk-pos))
      (dotimes (i (/ num-children 2))
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (jump-tree-node-branch node)
                           (jump-tree-node-next node))))
          (goto-char trunk-pos)
          (jump-tree-insert ?_ (- pos trunk-pos 1))
          (goto-char pos)
          (jump-tree-move-backward 1)
          (jump-tree-move-down 1)
          (jump-tree-insert ?\\)
          (jump-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (jump-tree-node-marker (car n)))
            (setf (jump-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (jump-tree-node-marker (car n)) nil))
          (move-marker (jump-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (when (cdr n)
          (goto-char pos)
          (jump-tree-move-forward
           (+ (jump-tree-node-char-rwidth (car n))
              (if (cadr n) (jump-tree-node-char-lwidth (cadr n)) 0)
              jump-tree-visualizer-spacing 1))
          (move-marker pos (point))))
      ))
    ;; return list of nodes to draw next
    (nreverse node-list)))

(defun jump-tree-node-char-lwidth (node)
  ;; Return left-width of NODE measured in characters.
  (if (= (length (jump-tree-node-next node)) 0) 0
    (- (* (+ jump-tree-visualizer-spacing 1) (jump-tree-node-lwidth node))
       (if (= (jump-tree-node-cwidth node) 0)
           (1+ (/ jump-tree-visualizer-spacing 2)) 0))))

(defun jump-tree-node-char-rwidth (node)
  ;; Return right-width of NODE measured in characters.
  (if (= (length (jump-tree-node-next node)) 0) 0
    (- (* (+ jump-tree-visualizer-spacing 1) (jump-tree-node-rwidth node))
       (if (= (jump-tree-node-cwidth node) 0)
           (1+ (/ jump-tree-visualizer-spacing 2)) 0))))

(defun jump-tree-insert (str &optional arg)
  ;; Insert character or string STR ARG times, overwriting, and using
  ;; `jump-tree-insert-face'.
  (unless arg (setq arg 1))
  (when (characterp str)
    (setq str (make-string arg str))
    (setq arg 1))
  (dotimes (i arg) (insert str))
  (setq arg (* arg (length str)))
  (jump-tree-move-forward arg)
  ;; make sure mark isn't active, otherwise `backward-delete-char' might
  ;; delete region instead of single char if transient-mark-mode is enabled
  (setq mark-active nil)
  (backward-delete-char arg)
  (when jump-tree-insert-face
    (put-text-property (- (point) arg) (point) 'face jump-tree-insert-face)))

(defun jump-tree-move-down (&optional arg)
  ;; Move down, extending buffer if necessary.
  (let ((row (line-number-at-pos))
        (col (current-column))
        line)
    (unless arg (setq arg 1))
    (forward-line arg)
    (setq line (line-number-at-pos))
    ;; if buffer doesn't have enough lines, add some
    (when (/= line (+ row arg))
      (cond
       ((< arg 0)
        (insert (make-string (- line row arg) ?\n))
        (forward-line (+ arg (- row line))))
       (t (insert (make-string (- arg (- line row)) ?\n)))))
    (jump-tree-move-forward col)))

(defun jump-tree-move-up (&optional arg)
  ;; Move up, extending buffer if necessary.
  (unless arg (setq arg 1))
  (jump-tree-move-down (- arg)))

(defun jump-tree-move-forward (&optional arg)
  ;; Move forward, extending buffer if necessary.
  (unless arg (setq arg 1))
  (let (n)
    (cond
     ((>= arg 0)
      (setq n (- (line-end-position) (point)))
      (if (> n arg)
          (forward-char arg)
        (end-of-line)
        (insert (make-string (- arg n) ? ))))
     ((< arg 0)
      (setq arg (- arg))
      (setq n (- (point) (line-beginning-position)))
      (when (< (- n 2) arg)  ; -2 to create left-margin
        ;; no space left - shift entire buffer contents right!
        (let ((pos (move-marker (make-marker) (point))))
          (set-marker-insertion-type pos t)
          (goto-char (point-min))
          (while (not (eobp))
            (insert-before-markers (make-string (- arg -2 n) ? ))
            (forward-line 1))
          (goto-char pos)))
      (backward-char arg)))))

(defun jump-tree-move-backward (&optional arg)
  ;; Move backward, extending buffer if necessary.
  (unless arg (setq arg 1))
  (jump-tree-move-forward (- arg)))

(defun jump-tree-move-to-parent (node)
  ;; Move to position of parent of NODE, extending buffer if necessary.
  (let* ((parent (jump-tree-node-previous node))
         (n (jump-tree-node-next parent))
         (l (length n)) p)
    (goto-char (jump-tree-node-marker node))
    (unless (= l 1)
      ;; move horizontally
      (setq p (jump-tree-index node n))
      (cond
       ;; node in centre subtree: no horizontal movement
       ((and (= (mod l 2) 1) (= p (/ l 2))))
       ;; node in left subtree: move right
       ((< p (/ l 2))
        (setq n (nthcdr p n))
        (jump-tree-move-forward
         (+ (jump-tree-node-char-rwidth (car n))
            (/ jump-tree-visualizer-spacing 2) 1))
        (dotimes (i (- (/ l 2) p 1))
          (setq n (cdr n))
          (jump-tree-move-forward
           (+ (jump-tree-node-char-lwidth (car n))
              (jump-tree-node-char-rwidth (car n))
              jump-tree-visualizer-spacing 1)))
        (when (= (mod l 2) 1)
          (setq n (cdr n))
          (jump-tree-move-forward
           (+ (jump-tree-node-char-lwidth (car n))
              (/ jump-tree-visualizer-spacing 2) 1))))
       (t ;; node in right subtree: move left
        (setq n (nthcdr (/ l 2) n))
        (when (= (mod l 2) 1)
          (jump-tree-move-backward
           (+ (jump-tree-node-char-rwidth (car n))
              (/ jump-tree-visualizer-spacing 2) 1))
          (setq n (cdr n)))
        (dotimes (i (- p (/ l 2) (mod l 2)))
          (jump-tree-move-backward
           (+ (jump-tree-node-char-lwidth (car n))
              (jump-tree-node-char-rwidth (car n))
              jump-tree-visualizer-spacing 1))
          (setq n (cdr n)))
        (jump-tree-move-backward
         (+ (jump-tree-node-char-lwidth (car n))
            (/ jump-tree-visualizer-spacing 2) 1)))))
    ;; move vertically
    (jump-tree-move-up 3)))

(defun jump-tree-timestamp-to-string
    (timestamp &optional relative current register)
  ;; Convert TIMESTAMP to string (either absolute or RELATVE time), indicating
  ;; if it's the CURRENT node and/or has an associated REGISTER.
  (if relative
      ;; relative time
      (let ((time (floor (float-time
                          (subtract-time (current-time) timestamp))))
            n)
        (setq time
              ;; years
              (if (> (setq n (/ time 315360000)) 0)
                  (if (> n 999) "-ages" (format "-%dy" n))
                (setq time (% time 315360000))
                ;; days
                (if (> (setq n (/ time 86400)) 0)
                    (format "-%dd" n)
                  (setq time (% time 86400))
                  ;; hours
                  (if (> (setq n (/ time 3600)) 0)
                      (format "-%dh" n)
                    (setq time (% time 3600))
                    ;; mins
                    (if (> (setq n (/ time 60)) 0)
                        (format "-%dm" n)
                      ;; secs
                      (format "-%ds" (% time 60)))))))
        (setq time (concat
                    (if current "*" " ")
                    time
                    (if register (concat "[" (char-to-string register) "]")
                      "   ")))
        (setq n (length time))
        (if (< n 9)
            (concat (make-string (- 9 n) ? ) time)
          time))
    ;; absolute time
    (concat (if current " *" "  ")
            (format-time-string "%H:%M:%S" timestamp)
            (if register
                (concat "[" (char-to-string register) "]")
              "   "))))


;;; =====================================================================
;;;                        Visualizer commands
(define-derived-mode
  jump-tree-visualizer-mode special-mode "jump-tree-visualizer"
  "Major mode used in jump-tree visualizer.
The jump-tree visualizer can only be invoked from a buffer in
which `jump-tree-mode' is enabled. The visualizer displays the
position history tree graphically, and allows you to browse around
the position history, jump-preving or jump-nexting the corresponding changes in
the parent buffer.
Within the jump-tree visualizer, the following keys are available:
  \\{jump-tree-visualizer-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t)
  (setq cursor-type nil)
  (setq jump-tree-visualizer-selected-node nil))

(defun jump-tree-visualize-jump-prev (&optional arg)
  "Jump-Prev changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (let ((old (jump-tree-current jump-tree-pos-tree))
        current)
    ;; unhighlight old current node
    (let ((jump-tree-insert-face 'jump-tree-visualizer-active-branch-face)
          (inhibit-read-only t))
      (jump-tree-draw-node old))
    ;; position in parent buffer
    (switch-to-buffer-other-window jump-tree-visualizer-parent-buffer)
    (deactivate-mark)
    (unwind-protect
        (let ((jump-tree-inhibit-kill-visualizer t)) (jump-tree-jump-prev-1 arg))
      (setq current (jump-tree-current jump-tree-pos-tree))
      (switch-to-buffer-other-window jump-tree-visualizer-buffer-name)
      ;; when using lazy drawing, extend tree upwards as required
      (when jump-tree-visualizer-lazy-drawing
        (jump-tree-expand-up old current))
      ;; highlight new current node
      (let ((inhibit-read-only t)) (jump-tree-draw-node current 'current)))))

(defun jump-tree-visualize-jump-next (&optional arg)
  "Jump-Next changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (let ((old (jump-tree-current jump-tree-pos-tree))
        current)
    ;; unhighlight old current node
    (let ((jump-tree-insert-face 'jump-tree-visualizer-active-branch-face)
          (inhibit-read-only t))
      (jump-tree-draw-node (jump-tree-current jump-tree-pos-tree)))
    ;; jump-next in parent buffer
    (switch-to-buffer-other-window jump-tree-visualizer-parent-buffer)
    (deactivate-mark)
    (unwind-protect
        (let ((jump-tree-inhibit-kill-visualizer t)) (jump-tree-jump-next-1 arg))
      (setq current (jump-tree-current jump-tree-pos-tree))
      (switch-to-buffer-other-window jump-tree-visualizer-buffer-name)
      ;; when using lazy drawing, extend tree downwards as required
      (when jump-tree-visualizer-lazy-drawing
        (jump-tree-expand-down old current))
      ;; highlight new current node
      (let ((inhibit-read-only t)) (jump-tree-draw-node current 'current)))))

(defun jump-tree-visualize-switch-branch-right (arg)
  "Switch to next branch of the position tree.
This will affect which branch to descend when *jump-nexting* changes
using `jump-tree-jump-next' or `jump-tree-visualizer-jump-next'."
  (interactive "p")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  ;; un-highlight old active branch below current node
  (goto-char (jump-tree-node-marker (jump-tree-current jump-tree-pos-tree)))
  (let ((jump-tree-insert-face 'jump-tree-visualizer-default-face)
        (inhibit-read-only t))
    (jump-tree-highlight-active-branch (jump-tree-current jump-tree-pos-tree)))
  ;; increment branch
  (let ((branch (jump-tree-node-branch (jump-tree-current jump-tree-pos-tree))))
    (setf (jump-tree-node-branch (jump-tree-current jump-tree-pos-tree))
          (cond
           ((>= (+ branch arg) (jump-tree-num-branches))
            (1- (jump-tree-num-branches)))
           ((<= (+ branch arg) 0) 0)
           (t (+ branch arg))))
    (let ((inhibit-read-only t))
      ;; highlight new active branch below current node
      (goto-char (jump-tree-node-marker (jump-tree-current jump-tree-pos-tree)))
      (let ((jump-tree-insert-face 'jump-tree-visualizer-active-branch-face))
        (jump-tree-highlight-active-branch (jump-tree-current jump-tree-pos-tree)))
      ;; re-highlight current node
      (jump-tree-draw-node (jump-tree-current jump-tree-pos-tree) 'current))))

(defun jump-tree-visualize-switch-branch-left (arg)
  "Switch to previous branch of the position tree.
This will affect which branch to descend when *jump-nexting* changes
using `jump-tree-jump-next' or `jump-tree-visualizer-jump-next'."
  (interactive "p")
  (jump-tree-visualize-switch-branch-right (- arg)))

(defun jump-tree-visualizer-quit ()
  "Quit the jump-tree visualizer."
  (interactive)
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (jump-tree-clear-visualizer-data jump-tree-pos-tree)
  ;; remove kill visualizer hook from parent buffer
  (unwind-protect
      (with-current-buffer jump-tree-visualizer-parent-buffer
        (remove-hook 'before-change-functions 'jump-tree-kill-visualizer t))
    (let ((parent jump-tree-visualizer-parent-buffer)
          window)
      ;; kill visualizer buffer
      (kill-buffer nil)
      ;; switch back to parent buffer
      (unwind-protect
          (if (setq window (get-buffer-window parent))
              (select-window window)
            (switch-to-buffer parent))))))

(defun jump-tree-visualizer-abort ()
  "Quit the jump-tree visualizer and return buffer to original state."
  (interactive)
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (let ((node jump-tree-visualizer-initial-node))
    (jump-tree-visualizer-quit)
    (jump-tree-set node)))

(defun jump-tree-visualizer-set (&optional pos)
  "Set buffer to state corresponding to position tree node
at POS, or point if POS is nil."
  (interactive)
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (unless pos (setq pos (point)))
  (let ((node (get-text-property pos 'jump-tree-node)))
    (when node
      ;; set parent buffer to state corresponding to node at POS
      (switch-to-buffer-other-window jump-tree-visualizer-parent-buffer)
      (let ((jump-tree-inhibit-kill-visualizer t)) (jump-tree-set node))
      (switch-to-buffer-other-window jump-tree-visualizer-buffer-name)
      ;; re-draw position tree
      (let ((inhibit-read-only t)) (jump-tree-draw-tree jump-tree-pos-tree)))))

(defun jump-tree-visualizer-mouse-set (pos)
  "Set buffer to state corresponding to position tree node
at mouse event POS."
  (interactive "@e")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (jump-tree-visualizer-set (event-start (nth 1 pos))))

(defun jump-tree-visualize-jump-prev-to-x (&optional x)
  "Jump-Prev to last branch point, register, or saved state.
If X is the symbol `branch', position to last branch point. If X is
the symbol `register', position to last register. If X is the sumbol
`saved', position to last saved state. If X is null, position to first of
these that's encountered.
Interactively, a single \\[universal-argument] specifies
`branch', a double \\[universal-argument] \\[universal-argument]
specifies `saved', and a negative prefix argument specifies
`register'."
  (interactive "P")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (when (and (called-interactively-p 'any) x)
    (setq x (prefix-numeric-value x)
          x (cond
             ((< x 0)  'register)
             ((<= x 4) 'branch)
             (t        'saved))))
  (let ((current (if jump-tree-visualizer-selection-mode
                     jump-tree-visualizer-selected-node
                   (jump-tree-current jump-tree-pos-tree))))
    (unwind-protect
        (jump-tree-expand-up
         (jump-tree-node-previous current)))
    ))

(defun jump-tree-visualizer-toggle-timestamps ()
  "Toggle display of time-stamps."
  (interactive)
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (setq jump-tree-visualizer-timestamps (not jump-tree-visualizer-timestamps))
  (setq jump-tree-visualizer-spacing (jump-tree-visualizer-calculate-spacing))
  ;; redraw tree
  (let ((inhibit-read-only t)) (jump-tree-draw-tree jump-tree-pos-tree)))

(defun jump-tree-visualizer-scroll-left (&optional arg)
  (interactive "p")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (scroll-left (or arg 1) t))

(defun jump-tree-visualizer-scroll-right (&optional arg)
  (interactive "p")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (scroll-right (or arg 1) t))

(defun jump-tree-visualizer-scroll-up (&optional arg)
  (interactive "P")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (if (or (and (numberp arg) (< arg 0)) (eq arg '-))
      (jump-tree-visualizer-scroll-down arg)
    ;; scroll up and expand newly-visible portion of tree
    (unwind-protect
        (scroll-up-command arg)
      (jump-tree-expand-down
       (nth (jump-tree-node-branch (jump-tree-current jump-tree-pos-tree))
            (jump-tree-node-next (jump-tree-current jump-tree-pos-tree)))))
    ;; signal error if at eob
    (when (and (not jump-tree-visualizer-needs-extending-down) (eobp))
      (scroll-up))))

(defun jump-tree-visualizer-scroll-down (&optional arg)
  (interactive "P")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (if (or (and (numberp arg) (< arg 0)) (eq arg '-))
      (jump-tree-visualizer-scroll-up arg)
    ;; ensure there's enough room at top of buffer to scroll
    (let ((scroll-lines
           (or arg (- (window-height) next-screen-context-lines)))
          (window-line (1- (line-number-at-pos (window-start)))))
      (when (and jump-tree-visualizer-needs-extending-up
                 (< window-line scroll-lines))
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (jump-tree-move-up (- scroll-lines window-line)))))
    ;; scroll down and expand newly-visible portion of tree
    (unwind-protect
        (scroll-down-command arg)
      (jump-tree-expand-up
       (jump-tree-node-previous (jump-tree-current jump-tree-pos-tree))))
    ;; signal error if at bob
    (when (and (not jump-tree-visualizer-needs-extending-down) (bobp))
      (scroll-down))))


;;; =====================================================================
;;;                    Visualizer selection mode
(define-minor-mode jump-tree-visualizer-selection-mode
  "Toggle mode to select nodes in jump-tree visualizer."
  :lighter "Select"
  :keymap jump-tree-visualizer-selection-mode-map
  :group jump-tree
  (cond
   ;; enable selection mode
   (jump-tree-visualizer-selection-mode
    (setq cursor-type 'box)
    (setq jump-tree-visualizer-selected-node
          (jump-tree-current jump-tree-pos-tree)))
   (t ;; disable selection mode
    (setq cursor-type nil)
    (setq jump-tree-visualizer-selected-node nil)
    (goto-char (jump-tree-node-marker (jump-tree-current jump-tree-pos-tree))))))

(defun jump-tree-visualizer-select-previous (&optional arg)
  "Move to previous node."
  (interactive "p")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (let ((node jump-tree-visualizer-selected-node))
    (catch 'top
      (dotimes (i (or arg 1))
        (unless (jump-tree-node-previous node) (throw 'top t))
        (setq node (jump-tree-node-previous node))))
    ;; when using lazy drawing, extend tree upwards as required
    (when jump-tree-visualizer-lazy-drawing
      (jump-tree-expand-up jump-tree-visualizer-selected-node node))
    ;; move to selected node
    (goto-char (jump-tree-node-marker node))
    (setq jump-tree-visualizer-selected-node node)))

(defun jump-tree-visualizer-select-next (&optional arg)
  "Move to next node."
  (interactive "p")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (let ((node jump-tree-visualizer-selected-node))
    (catch 'bottom
      (dotimes (i (or arg 1))
        (unless (nth (jump-tree-node-branch node) (jump-tree-node-next node))
          (throw 'bottom t))
        (setq node
              (nth (jump-tree-node-branch node) (jump-tree-node-next node)))))
    ;; when using lazy drawing, extend tree downwards as required
    (when jump-tree-visualizer-lazy-drawing
      (jump-tree-expand-down jump-tree-visualizer-selected-node node))
    ;; move to selected node
    (goto-char (jump-tree-node-marker node))
    (setq jump-tree-visualizer-selected-node node)))

(defun jump-tree-visualizer-select-right (&optional arg)
  "Move right to a sibling node."
  (interactive "p")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (let ((node jump-tree-visualizer-selected-node)
        end)
    (goto-char (jump-tree-node-marker jump-tree-visualizer-selected-node))
    (setq end (line-end-position))
    (catch 'end
      (dotimes (i arg)
        (while (or (null node) (eq node jump-tree-visualizer-selected-node))
          (forward-char)
          (setq node (get-text-property (point) 'jump-tree-node))
          (when (= (point) end) (throw 'end t)))))
    (goto-char (jump-tree-node-marker
                (or node jump-tree-visualizer-selected-node)))
    (when node (setq jump-tree-visualizer-selected-node node))))

(defun jump-tree-visualizer-select-left (&optional arg)
  "Move left to a sibling node."
  (interactive "p")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (let ((node (get-text-property (point) 'jump-tree-node))
        beg)
    (goto-char (jump-tree-node-marker jump-tree-visualizer-selected-node))
    (setq beg (line-beginning-position))
    (catch 'beg
      (dotimes (i arg)
        (while (or (null node) (eq node jump-tree-visualizer-selected-node))
          (backward-char)
          (setq node (get-text-property (point) 'jump-tree-node))
          (when (= (point) beg) (throw 'beg t)))))
    (goto-char (jump-tree-node-marker
                (or node jump-tree-visualizer-selected-node)))
    (when node (setq jump-tree-visualizer-selected-node node))))

(defun jump-tree-visualizer-select (pos)
  (let ((node (get-text-property pos 'jump-tree-node)))
    (when node
      ;; select node at POS
      (goto-char (jump-tree-node-marker node))
      ;; when using lazy drawing, extend tree up and down as required
      (when jump-tree-visualizer-lazy-drawing
        (jump-tree-expand-up jump-tree-visualizer-selected-node node)
        (jump-tree-expand-down jump-tree-visualizer-selected-node node))
      ;; update selected node
      (setq jump-tree-visualizer-selected-node node)
      )))

(defun jump-tree-visualizer-mouse-select (pos)
  "Select position tree node at mouse event POS."
  (interactive "@e")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (jump-tree-visualizer-select (event-start (nth 1 pos))))



(defun jump-tree-pos-list--do-jump (position)
  "Do jump to target file and point from BUFF."
  (let ((file-path (car position))
        (marker (cdr position)))
    (when (and (markerp marker) (marker-buffer marker))
      (find-file file-path)
      (goto-char marker))))

(defun jump-tree-pos-list--push (position)
  "Push POSITION to `jump-tree-pos-list'."
  (while (> (length jump-tree-pos-list) jump-tree-pos-list-limit)
    (setq jump-tree-pos-list (cdr jump-tree-pos-list)))
  (push position jump-tree-pos-list))

(defun jump-tree-pos-list--same-position? (position)
  (let ((new-point (cdr position))
        (top-point (cdar jump-tree-pos-list)))
    (cond ((not new-point) nil)
          ((not top-point) nil)
          ((eq (marker-position new-point) (marker-position top-point)) 't))))

(defun jump-tree-pos-list--set ()
  "The record data structure is (file-name . position)."
  (interactive)
  (if (buffer-file-name)
      (let ((position (cons (buffer-file-name) (point-marker))))
        (unless (jump-tree-pos-list--same-position? position)
          (jump-tree-pos-list--push position)))))

(defun jump-tree-pos-list--do-command? (command do-hook-command-list)
  (if do-hook-command-list
      (or
       (eq command (car do-hook-command-list))
       (jump-tree-pos-list--do-command? command (cdr do-hook-command-list)))))

(defun jump-tree-pos-list--command-hook ()
  "Pre command hook that call `jump-tree-pos-list--set' when registerd command hook called."
  (when (and (not jump-tree-in-progress)
             (jump-tree-pos-list--do-command? this-command jump-tree-pos-list-hook-commands))
    (jump-tree-pos-list--set)))
(add-hook 'pre-command-hook 'jump-tree-pos-list--command-hook)

(provide 'jump-tree)
;;; jump-tree.el ends here
