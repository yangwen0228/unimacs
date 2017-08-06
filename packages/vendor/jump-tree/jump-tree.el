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

;; `registerv' defstruct isn't defined in Emacs versions < 24
(unless (fboundp 'registerv-make)
  (defmacro registerv-make (data &rest _dummy) data))

(unless (fboundp 'registerv-data)
  (defmacro registerv-data (data) data))

;; `user-error' isn't defined in Emacs < 24.3
(unless (fboundp 'user-error)
  (defalias 'user-error 'error)
  ;; prevent debugger being called on user errors
  (add-to-list 'debug-ignored-errors "^No further jump-prev information")
  (add-to-list 'debug-ignored-errors "^No further jump-next information"))


;;; =====================================================================
;;;              Global variables and customization options

(defgroup jump-tree nil
  "Tree jump-prev/jump-next."
  :group 'jump-tree)

(defvar jump-tree-pos-list '()
  "Jump history list, contain POSITION entries '(file-name . marker).")

(defvar jump-tree-pos-tree nil
  "Tree of position entries globally.")

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

(defcustom jump-tree-pos-list-hook-commands
  '(beginning-of-buffer
    end-of-buffer backward-up-list
    beginning-of-defun end-of-defun
    unimacs-move-beginning-of-line unimacs-move-end-of-line
    unimacs-move-beginning-of-window unimacs-move-end-of-window
    helm-swoop helm-imenu helm-find-files helm-multi-files
    helm-projectile-switch-project helm-projectile-find-file
    helm-gtags-find-pattern helm-gtags-find-tag-adapter
    helm-gtags-find-rtag-adapter helm-ag-select-directory
    find-function find-variable
    mark-defun mark-whole-buffer
    avy-goto-char avy-goto-char-2
    ensime-edit-definition
    ensime-edit-definition-with-fallback
    isearch-forward)
  "Commands to hook."
  :type 'list
  :group 'jump-tree)

(defcustom jump-tree-mode-lighter " Jump-Tree"
  "Lighter displayed in mode line
when `jump-tree-mode' is enabled."
  :group 'jump-tree
  :type 'string)


;;; =====================================================================
;;;              jump-tree record position list

(defun jump-tree-pos-list-jump (position)
  "Do jump to target file and point from BUFF."
  (let ((file-path (car position))
        (marker (cdr position)))
    (when (and (markerp marker) (marker-buffer marker))
      (find-file file-path)
      (goto-char marker))))

(defun jump-tree-pos-list-push (position)
  "Push POSITION to `jump-tree-pos-list'."
  (while (> (length jump-tree-pos-list) jump-tree-pos-list-limit)
    (setq jump-tree-pos-list (cdr jump-tree-pos-list)))
  (push position jump-tree-pos-list))

(defun jump-tree-pos-list-same-position? (position)
  (let ((new-point (cdr position))
        (top-point (cdar jump-tree-pos-list)))
    (cond ((not new-point) nil)
          ((not top-point) nil)
          ((eq (marker-position new-point) (marker-position top-point)) 't))))

(defun jump-tree-pos-list-set ()
  "The record data structure is (file-name . position)."
  (interactive)
  (if (buffer-file-name)
      (let ((position (cons (buffer-file-name) (point-marker))))
        (unless (jump-tree-pos-list-same-position? position)
          (jump-tree-pos-list-push position)))))

(defun jump-tree-pos-list-command-hook ()
  "Pre command hook that call `jump-tree-pos-list-set' when registerd command hook called."
  (when (and (not jump-tree-in-progress)
             (memq this-command jump-tree-pos-list-hook-commands))
    (jump-tree-pos-list-set)))

(add-hook 'pre-command-hook 'jump-tree-pos-list-command-hook)


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
                   (&optional file-path marker))
     (:copier nil))
  file-path marker)

(defmacro jump-tree-position-data-p (pos)
  (let ((len (length (jump-tree-make-position-data))))
    `(and (vectorp ,pos) (= (length ,pos) ,len))))

(defmacro jump-tree-node-file-path (node)
  `(let ((pos (plist-get (jump-tree-node-meta-data ,node) :position)))
     (when (jump-tree-position-data-p pos)
       (jump-tree-position-data-file-path pos))))

(defmacro jump-tree-node-marker (node)
  `(let ((pos (plist-get (jump-tree-node-meta-data ,node) :position)))
     (when (jump-tree-position-data-p pos)
       (jump-tree-position-data-marker pos))))

(defsetf jump-tree-node-file-path (node) (val)
  `(let ((pos (plist-get (jump-tree-node-meta-data ,node) :position)))
     (unless (jump-tree-position-data-p pos)
       (setf (jump-tree-node-meta-data ,node)
             (plist-put (jump-tree-node-meta-data ,node) :position
                        (setq pos (jump-tree-make-position-data)))))
     (setf (jump-tree-position-data-file-path pos) ,val)))

(defsetf jump-tree-node-marker (node) (val)
  `(let ((pos (plist-get (jump-tree-node-meta-data ,node) :position)))
     (unless (jump-tree-position-data-p pos)
       (setf (jump-tree-node-meta-data ,node)
             (plist-put (jump-tree-node-meta-data ,node) :position
                        (setq pos (jump-tree-make-position-data)))))
     (setf (jump-tree-position-data-marker pos) ,val)))

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
;;;         Basic common jump-tree data structure functions
(defmacro jump-tree-num-branches ()
  "Return number of branches at current position tree node."
  '(length (jump-tree-node-next (jump-tree-current jump-tree-pos-tree))))

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
  (setq jump-tree-pos-list
        (remove-if (lambda (position)
                     (or (not (markerp (cdr position)))
                         (not (marker-buffer (cdr position)))))
                   jump-tree-pos-list)))

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
        (user-error "No further jump-prev information"))
      (setq current (jump-tree-node-previous current))
      (setf (jump-tree-current jump-tree-pos-tree) current)

      (print (cdr (jump-tree-node-position current)))

      (jump-tree-pos-list-jump (jump-tree-node-position current)))))

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

      (jump-tree-pos-list-jump (jump-tree-node-position current)))))

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

(provide 'jump-tree)
;;; jump-tree.el ends here
