;;; jump-tree.el --- Treat jump-prev history as a tree  -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2014  Free Software Foundation, Inc

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
;; Emacs has a powerful jump-prev system. Unlike the standard jump-prev/jump-next system in
;; most software, it allows you to recover *any* past state of a buffer
;; (whereas the standard jump-prev/jump-next system can lose past states as soon as you
;; jump-next). However, this power comes at a price: many people find Emacs' jump-prev
;; system confusing and difficult to use, spawning a number of packages that
;; replace it with the less powerful but more intuitive jump-prev/jump-next system.
;;
;; Both the loss of data with standard jump-prev/jump-next, and the confusion of Emacs'
;; jump-prev, stem from trying to treat jump-prev history as a linear sequence of
;; changes. It's not. The `jump-tree-mode' provided by this package replaces
;; Emacs' jump-prev system with a system that treats jump-prev history as what it is: a
;; branching tree of changes. This simple idea allows the more intuitive
;; behaviour of the standard jump-prev/jump-next system to be combined with the power of
;; never losing any history. An added side bonus is that jump-prev history can in
;; some cases be stored more efficiently, allowing more changes to accumulate
;; before Emacs starts discarding history.
;;
;; The only downside to this more advanced yet simpler jump-prev system is that it
;; was inspired by Vim. But, after all, most successful religions steal the
;; best ideas from their competitors!
;;
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
;; If you want to replace the standard Emacs' jump-prev system with the
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
;; C-_  C-/  (`jump-tree-jump-prev')
;;   Jump-Prev changes.
;;
;; M-_  C-?  (`jump-tree-jump-next')
;;   Jump-Next changes.
;;
;; `jump-tree-switch-branch'
;;   Switch jump-tree branch.
;;   (What does this mean? Better press the button and see!)
;;
;; C-x u  (`jump-tree-visualize')
;;   Visualize the jump-prev tree.
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
;; C-<down>  M-}  (`jump-tree-visualize-jump-next-to-x')
;;   Jump-Next changes down to next branch point.
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
;; d  (`jump-tree-visualizer-toggle-diff')
;;   Toggle diff display.
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
;; Persistent jump-prev history:
;;
;; Note: Requires Emacs version 24.3 or higher.
;;
;; `jump-tree-auto-save-history' (variable)
;;    automatically save and restore jump-tree history along with buffer
;;    (disabled by default)
;;
;; `jump-tree-save-history' (command)
;;    manually save jump-prev history to file
;;
;; `jump-tree-load-history' (command)
;;    manually load jump-prev history from file
;;
;;
;;
;; Compressing jump-prev history:
;;
;;   Jump-Prev history files cannot grow beyond the maximum jump-prev tree size, which
;;   is limited by `jump-prev-limit', `jump-prev-strong-limit' and
;;   `jump-prev-outer-limit'. Nevertheless, jump-prev history files can grow quite
;;   large. If you want to automatically compress jump-prev history, add the
;;   following advice to your .emacs file (replacing ".gz" with the filename
;;   extension of your favourite compression algorithm):
;;
;;   (defadvice jump-tree-make-history-save-file-name
;;     (after jump-tree activate)
;;     (setq ad-return-value (concat ad-return-value ".gz")))
;;
;;
;;
;;
;; Jump-Prev Systems
;; ============
;;
;; To understand the different jump-prev systems, it's easiest to consider an
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
;; Now imagine that you jump-prev the last two changes. We can visualize this as
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
;; However, this isn't a good representation of what Emacs' jump-prev system
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
;;                                x  (buffer state before jump-prev)
;;                                |
;;                                |
;;                                o  (first jump-prev)
;;                                |
;;                                |
;;                                x  (second jump-prev)
;;
;;
;; Actually, since the buffer returns to a previous state after an jump-prev,
;; perhaps a better way to visualize it is to imagine the string of changes
;; turning back on itself:
;;
;;        (initial buffer state)  o
;;                                |
;;                                |
;;                  (first edit)  o  x  (second jump-prev)
;;                                |  |
;;                                |  |
;;                 (second edit)  o  o  (first jump-prev)
;;                                | /
;;                                |/
;;                                o  (buffer state before jump-prev)
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
;; branch. Emacs' jump-prev just keeps adding changes to the end of the string. So
;; the jump-prev history in the two systems now looks like this:
;;
;;            Jump-Prev/Jump-Next:                      Emacs' jump-prev
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
;; However, in Emacs' jump-prev system, those old buffer states are still there in
;; the jump-prev history. You just have to rewind back through the new edit, and
;; back through the changes made by the jump-prevs, until you reach them. Of
;; course, since Emacs treats jump-prevs (even jump-prevs of jump-prevs!) as new changes,
;; you're really weaving backwards and forwards through the history, all the
;; time adding new changes to the end of the string as you go:
;;
;;                       o
;;                       |
;;                       |
;;                       o  o     o  (jump-prev new edit)
;;                       |  |\    |\
;;                       |  | \   | \
;;                       o  o  |  |  o  (jump-prev the jump-prev)
;;                       | /   |  |  |
;;                       |/    |  |  |
;;      (trying to get   o     |  |  x  (jump-prev the jump-prev)
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
;; and haven't invoked any command since the last jump-prev, you can just keep on
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
;; getting to the state labelled "got this far", then you've "broken the jump-prev
;; chain". Hold on to something solid, because things are about to get
;; hairy. If you try to jump-prev now, Emacs thinks you're trying to jump-prev the
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
;;                              broke the jump-prev chain)
;;
;; Confused?
;;
;; In practice you can just hold down the jump-prev key until you reach the buffer
;; state that you want. But whatever you do, don't move around in the buffer
;; to *check* that you've got back to where you want! Because you'll break the
;; jump-prev chain, and then you'll have to traverse the entire string of jump-prevs
;; again, just to get back to the point at which you broke the
;; chain. Jump-Prev-in-region and commands such as `jump-prev-only' help to make using
;; Emacs' jump-prev a little easier, but nonetheless it remains confusing for many
;; people.
;;
;;
;; So what does `jump-tree-mode' do? Remember the diagram we drew to represent
;; the history we've been discussing (make a few edits, jump-prev a couple of them,
;; and edit again)? The diagram that conceptually represented our jump-prev
;; history, before we started discussing specific jump-prev systems? It looked like
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
;; Well, that's *exactly* what the jump-prev history looks like to
;; `jump-tree-mode'.  It doesn't discard the old branch (as standard jump-prev/jump-next
;; does), nor does it treat jump-prevs as new changes to be added to the end of a
;; linear string of buffer states (as Emacs' jump-prev does). It just keeps track
;; of the tree of branching changes that make up the entire jump-prev history.
;;
;; If you jump-prev from this point, you'll rewind back up the tree to the previous
;; state:
;;
;;                                o
;;                                |
;;                                |
;;                                x  (jump-prev)
;;                                |\
;;                                | \
;;                                o  o
;;                                |
;;                                |
;;                                o
;;
;; If you were to jump-prev again, you'd rewind back to the initial state. If on
;; the other hand you jump-next the change, you'll end up back at the bottom of the
;; most recent branch:
;;
;;                                o  (jump-prev takes you here)
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
;; Now you're on the other branch, if you jump-prev and jump-next changes you'll stay on
;; that branch, moving up and down through the buffer states located on that
;; branch. Until you decide to switch branches again, of course.
;;
;; Real jump-prev trees might have multiple branches and sub-branches:
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
;; Trying to imagine what Emacs' jump-prev would do as you move about such a tree
;; will likely frazzle your brain circuits! But in `jump-tree-mode', you're
;; just moving around this jump-prev history tree. Most of the time, you'll
;; probably only need to stay on the most recent branch, in which case it
;; behaves like standard jump-prev/jump-next, and is just as simple to understand. But
;; if you ever need to recover a buffer state on a different branch, the
;; possibility of switching between branches and accessing the full jump-prev
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
;; a diff of the jump-prev changes (by hitting "d"). (There's one other tiny
;; difference: the visualizer puts the most recent branch on the left rather
;; than the right.)
;;
;; Bring up the jump-prev tree visualizer whenever you want by hitting "C-x u".
;;
;; In the visualizer, the usual keys for moving up and down a buffer instead
;; move up and down the jump-prev history tree (e.g. the up and down arrow keys, or
;; "C-n" and "C-p"). The state of the "parent" buffer (the buffer whose jump-prev
;; history you are visualizing) is updated as you move around the jump-prev tree in
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
(require 'diff)


;;; =====================================================================
;;;              Compatibility hacks for older Emacsen

;; `characterp' isn't defined in Emacs versions < 23
(unless (fboundp 'characterp)
  (defalias 'characterp 'char-valid-p))

;; `region-active-p' isn't defined in Emacs versions < 23
(unless (fboundp 'region-active-p)
  (defun region-active-p () (and transient-mark-mode mark-active)))

;; `registerv' defstruct isn't defined in Emacs versions < 24
(unless (fboundp 'registerv-make)
  (defmacro registerv-make (data &rest _dummy) data))

(unless (fboundp 'registerv-data)
  (defmacro registerv-data (data) data))

;; `diff-no-select' and `diff-file-local-copy' aren't defined in Emacs
;; versions < 24 (copied and adapted from Emacs 24)
(unless (fboundp 'diff-no-select)
  (defun diff-no-select (old new &optional switches no-async buf)
    ;; Noninteractive helper for creating and reverting diff buffers
    (unless (bufferp new) (setq new (expand-file-name new)))
    (unless (bufferp old) (setq old (expand-file-name old)))
    (or switches (setq switches diff-switches)) ; If not specified, use default.
    (unless (listp switches) (setq switches (list switches)))
    (or buf (setq buf (get-buffer-create "*Diff*")))
    (let* ((old-alt (diff-file-local-copy old))
           (new-alt (diff-file-local-copy new))
           (command
            (mapconcat 'identity
                       `(,diff-command
                         ;; Use explicitly specified switches
                         ,@switches
                         ,@(mapcar #'shell-quote-argument
                                   (nconc
                                    (when (or old-alt new-alt)
                                      (list "-L" (if (stringp old)
                                                     old (prin1-to-string old))
                                            "-L" (if (stringp new)
                                                     new (prin1-to-string new))))
                                    (list (or old-alt old)
                                          (or new-alt new)))))
                       " "))
           (thisdir default-directory))
      (with-current-buffer buf
        (setq buffer-read-only t)
        (buffer-disable-jump-prev (current-buffer))
        (let ((inhibit-read-only t))
          (erase-buffer))
        (buffer-enable-jump-prev (current-buffer))
        (diff-mode)
        (set (make-local-variable 'revert-buffer-function)
             (lambda (_ignore-auto _noconfirm)
               (diff-no-select old new switches no-async (current-buffer))))
        (setq default-directory thisdir)
        (let ((inhibit-read-only t))
          (insert command "\n"))
        (if (and (not no-async) (fboundp 'start-process))
            (let ((proc (start-process "Diff" buf shell-file-name
                                       shell-command-switch command)))
              (set-process-filter proc 'diff-process-filter)
              (set-process-sentinel
               proc (lambda (proc _msg)
                      (with-current-buffer (process-buffer proc)
                        (diff-sentinel (process-exit-status proc))
                        (if old-alt (delete-file old-alt))
                        (if new-alt (delete-file new-alt))))))
          ;; Async processes aren't available.
          (let ((inhibit-read-only t))
            (diff-sentinel
             (call-process shell-file-name nil buf nil
                           shell-command-switch command))
            (if old-alt (delete-file old-alt))
            (if new-alt (delete-file new-alt)))))
      buf)))

(unless (fboundp 'diff-file-local-copy)
  (defun diff-file-local-copy (file-or-buf)
    (if (bufferp file-or-buf)
        (with-current-buffer file-or-buf
          (let ((tempfile (make-temp-file "buffer-content-")))
            (write-region nil nil tempfile nil 'nomessage)
            tempfile))
      (file-local-copy file-or-buf))))

;; `user-error' isn't defined in Emacs < 24.3
(unless (fboundp 'user-error)
  (defalias 'user-error 'error)
  ;; prevent debugger being called on user errors
  (add-to-list 'debug-ignored-errors "^No further jump-prev information")
  (add-to-list 'debug-ignored-errors "^No further jump-next information")
  (add-to-list 'debug-ignored-errors "^No further jump-next information for region"))


;;; =====================================================================
;;;              Global variables and customization options

(defgroup jump-tree nil
  "Tree jump-prev/jump-next."
  :group 'jump-prev)

(defvar jump-tree-global-tree nil
  "Tree of jump-prev entries globally.")

(defvar jump-tree-global-list '()
  "Jump history list, contain entries '(file-name . pointer).")

(defcustom jump-tree-mode-lighter " Jump-Tree"
  "Lighter displayed in mode line
when `jump-tree-mode' is enabled."
  :group 'jump-tree
  :type 'string)

(defcustom jump-tree-incompatible-major-modes '(term-mode)
  "List of major-modes in which `jump-tree-mode' should not be enabled.
\(See `turn-on-jump-tree-mode'.\)"
  :group 'jump-tree
  :type '(repeat symbol))

(defcustom jump-tree-enable-jump-prev-in-region t
  "When non-nil, enable jump-prev-in-region.

When jump-prev-in-region is enabled, jump-preving or jump-nexting when the
region is active (in `transient-mark-mode') or with a prefix
argument (not in `transient-mark-mode') only jump-preves changes
within the current region."
  :group 'jump-tree
  :type 'boolean)

(defcustom jump-tree-auto-save-history nil
  "When non-nil, `jump-tree-mode' will save jump-prev history to file
when a buffer is saved to file.

It will automatically load jump-prev history when a buffer is loaded
from file, if an jump-prev save file exists.

By default, jump-tree history is saved to a file called
\".<buffer-file-name>.~jump-tree~\" in the same directory as the
file itself. To save under a different directory, customize
`jump-tree-history-directory-alist' (see the documentation for
that variable for details).

WARNING! `jump-tree-auto-save-history' will not work properly in
Emacs versions prior to 24.3, so it cannot be enabled via
the customization interface in versions earlier than that one. To
ignore this warning and enable it regardless, set
`jump-tree-auto-save-history' to a non-nil value outside of
customize."
  :group 'jump-tree
  :type (if (version-list-< (version-to-list emacs-version) '(24 3))
            '(choice (const :tag "<disabled>" nil))
          'boolean))

(defcustom jump-tree-history-directory-alist nil
  "Alist of filename patterns and jump-prev history directory names.
Each element looks like (REGEXP . DIRECTORY).  Jump-Prev history for
files with names matching REGEXP will be saved in DIRECTORY.
DIRECTORY may be relative or absolute.  If it is absolute, so
that all matching files are backed up into the same directory,
the file names in this directory will be the full name of the
file backed up with all directory separators changed to `!' to
prevent clashes.  This will not work correctly if your filesystem
truncates the resulting name.

For the common case of all backups going into one directory, the
alist should contain a single element pairing \".\" with the
appropriate directory name.

If this variable is nil, or it fails to match a filename, the
backup is made in the original file's directory.

On MS-DOS filesystems without long names this variable is always
ignored."
  :group 'jump-tree
  :type '(repeat (cons (regexp :tag "Regexp matching filename")
                       (directory :tag "Jump-Prev history directory name"))))

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

(defcustom jump-tree-visualizer-diff nil
  "When non-nil, display diff by default in jump-tree visualizer.

\\<jump-tree-visualizer-mode-map>You can always toggle the diff display \
using \\[jump-tree-visualizer-toggle-diff], regardless of the
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
                 (integer :tag "> size")))

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

(defface jump-tree-visualizer-unmodified-face
  '((((class color)) :foreground "cyan"))
  "Face used to highlight nodes corresponding to unmodified buffers
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
(defconst jump-tree-diff-buffer-name "*jump-tree Diff*")


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
    (define-key map [remap forward-paragraph] 'jump-tree-visualize-jump-next-to-x)
    (define-key map "\M-{" 'jump-tree-visualize-jump-prev-to-x)
    (define-key map "\M-}" 'jump-tree-visualize-jump-next-to-x)
    (define-key map [C-up] 'jump-tree-visualize-jump-prev-to-x)
    (define-key map [C-down] 'jump-tree-visualize-jump-next-to-x)
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
    ;; toggle diff
    (define-key map "d" 'jump-tree-visualizer-selection-toggle-diff)
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
                    (size 0)
                    (count 0)
                    (object-pool (make-hash-table :test 'eq :weakness 'value))))
     ;;(:copier nil)
     )
  root current size count object-pool)

(defstruct
    (jump-tree-node
     (:type vector)   ; create unnamed struct
     (:constructor nil)
     (:constructor jump-tree-make-node
                   (previous jump-prev
                             &optional jump-next
                             &aux
                             (timestamp (current-time))
                             (branch 0)))
     (:constructor jump-tree-make-node-backwards
                   (next-node jump-prev
                              &optional jump-next
                              &aux
                              (next (list next-node))
                              (timestamp (current-time))
                              (branch 0)))
     (:copier nil))
  previous next jump-prev jump-next timestamp branch meta-data)

(defmacro jump-tree-node-p (n)
  (let ((len (length (jump-tree-make-node nil nil))))
    `(and (vectorp ,n) (= (length ,n) ,len))))

(defstruct
    (jump-tree-region-data
     (:type vector)   ; create unnamed struct
     (:constructor nil)
     (:constructor jump-tree-make-region-data
                   (&optional jump-prev-beginning jump-prev-end
                              jump-next-beginning jump-next-end))
     (:constructor jump-tree-make-jump-prev-region-data
                   (jump-prev-beginning jump-prev-end))
     (:constructor jump-tree-make-jump-next-region-data
                   (jump-next-beginning jump-next-end))
     (:copier nil))
  jump-prev-beginning jump-prev-end jump-next-beginning jump-next-end)

(defmacro jump-tree-region-data-p (r)
  (let ((len (length (jump-tree-make-region-data))))
    `(and (vectorp ,r) (= (length ,r) ,len))))

(defmacro jump-tree-node-clear-region-data (node)
  `(setf (jump-tree-node-meta-data ,node)
         (delq nil
               (delq :region
                     (plist-put (jump-tree-node-meta-data ,node)
                                :region nil)))))

(defmacro jump-tree-node-jump-prev-beginning (node)
  `(let ((r (plist-get (jump-tree-node-meta-data ,node) :region)))
     (when (jump-tree-region-data-p r)
       (jump-tree-region-data-jump-prev-beginning r))))

(defmacro jump-tree-node-jump-prev-end (node)
  `(let ((r (plist-get (jump-tree-node-meta-data ,node) :region)))
     (when (jump-tree-region-data-p r)
       (jump-tree-region-data-jump-prev-end r))))

(defmacro jump-tree-node-jump-next-beginning (node)
  `(let ((r (plist-get (jump-tree-node-meta-data ,node) :region)))
     (when (jump-tree-region-data-p r)
       (jump-tree-region-data-jump-next-beginning r))))

(defmacro jump-tree-node-jump-next-end (node)
  `(let ((r (plist-get (jump-tree-node-meta-data ,node) :region)))
     (when (jump-tree-region-data-p r)
       (jump-tree-region-data-jump-next-end r))))

(defsetf jump-tree-node-jump-prev-beginning (node) (val)
  `(let ((r (plist-get (jump-tree-node-meta-data ,node) :region)))
     (unless (jump-tree-region-data-p r)
       (setf (jump-tree-node-meta-data ,node)
             (plist-put (jump-tree-node-meta-data ,node) :region
                        (setq r (jump-tree-make-region-data)))))
     (setf (jump-tree-region-data-jump-prev-beginning r) ,val)))

(defsetf jump-tree-node-jump-prev-end (node) (val)
  `(let ((r (plist-get (jump-tree-node-meta-data ,node) :region)))
     (unless (jump-tree-region-data-p r)
       (setf (jump-tree-node-meta-data ,node)
             (plist-put (jump-tree-node-meta-data ,node) :region
                        (setq r (jump-tree-make-region-data)))))
     (setf (jump-tree-region-data-jump-prev-end r) ,val)))

(defsetf jump-tree-node-jump-next-beginning (node) (val)
  `(let ((r (plist-get (jump-tree-node-meta-data ,node) :region)))
     (unless (jump-tree-region-data-p r)
       (setf (jump-tree-node-meta-data ,node)
             (plist-put (jump-tree-node-meta-data ,node) :region
                        (setq r (jump-tree-make-region-data)))))
     (setf (jump-tree-region-data-jump-next-beginning r) ,val)))

(defsetf jump-tree-node-jump-next-end (node) (val)
  `(let ((r (plist-get (jump-tree-node-meta-data ,node) :region)))
     (unless (jump-tree-region-data-p r)
       (setf (jump-tree-node-meta-data ,node)
             (plist-put (jump-tree-node-meta-data ,node) :region
                        (setq r (jump-tree-make-region-data)))))
     (setf (jump-tree-region-data-jump-next-end r) ,val)))

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

(defun jump-tree-grow (jump-prev)
  "Add an JUMP-PREV node to current branch of `jump-tree-global-tree'."
  (let* ((current (jump-tree-current jump-tree-global-tree))
         (new (jump-tree-make-node current jump-prev)))
    (push new (jump-tree-node-next current))
    (setf (jump-tree-current jump-tree-global-tree) new)))

(defun jump-tree-grow-backwards (node jump-prev &optional jump-next)
  "Add new node *above* jump-tree NODE, and return new node.
Note that this will overwrite NODE's \"previous\" link, so should
only be used on a detached NODE, never on nodes that are already
part of `jump-tree-global-tree'."
  (let ((new (jump-tree-make-node-backwards node jump-prev jump-next)))
    (setf (jump-tree-node-previous node) new)
    new))

(defun jump-tree-splice-node (node splice)
  "Splice NODE into jump-prev tree, below node SPLICE.
Note that this will overwrite NODE's \"next\" and \"previous\"
links, so should only be used on a detached NODE, never on nodes
that are already part of `jump-tree-global-tree'."
  (setf (jump-tree-node-next node) (jump-tree-node-next splice)
        (jump-tree-node-branch node) (jump-tree-node-branch splice)
        (jump-tree-node-previous node) splice
        (jump-tree-node-next splice) (list node)
        (jump-tree-node-branch splice) 0)
  (dolist (n (jump-tree-node-next node))
    (setf (jump-tree-node-previous n) node)))

(defun jump-tree-snip-node (node)
  "Snip NODE out of jump-prev tree."
  (let* ((parent (jump-tree-node-previous node))
         position p)
    ;; if NODE is only child, replace parent's next links with NODE's
    (if (= (length (jump-tree-node-next parent)) 0)
        (setf (jump-tree-node-next parent) (jump-tree-node-next node)
              (jump-tree-node-branch parent) (jump-tree-node-branch node))
      ;; otherwise...
      (setq position (jump-tree-position node (jump-tree-node-next parent)))
      (cond
       ;; if active branch used do go via NODE, set parent's branch to active
       ;; branch of NODE
       ((= (jump-tree-node-branch parent) position)
        (setf (jump-tree-node-branch parent)
              (+ position (jump-tree-node-branch node))))
       ;; if active branch didn't go via NODE, update parent's branch to point
       ;; to same node as before
       ((> (jump-tree-node-branch parent) position)
        (incf (jump-tree-node-branch parent)
              (1- (length (jump-tree-node-next node))))))
      ;; replace NODE in parent's next list with NODE's entire next list
      (if (= position 0)
          (setf (jump-tree-node-next parent)
                (nconc (jump-tree-node-next node)
                       (cdr (jump-tree-node-next parent))))
        (setq p (nthcdr (1- position) (jump-tree-node-next parent)))
        (setcdr p (nconc (jump-tree-node-next node) (cddr p)))))
    ;; update previous links of NODE's children
    (dolist (n (jump-tree-node-next node))
      (setf (jump-tree-node-previous n) parent))))

(defun jump-tree-mapc (--jump-tree-mapc-function-- node)
  ;; Apply FUNCTION to NODE and to each node below it.
  (let ((stack (list node))
        n)
    (while stack
      (setq n (pop stack))
      (funcall --jump-tree-mapc-function-- n)
      (setq stack (append (jump-tree-node-next n) stack)))))

(defmacro jump-tree-num-branches ()
  "Return number of branches at current jump-prev tree node."
  '(length (jump-tree-node-next (jump-tree-current jump-tree-global-tree))))

(defun jump-tree-position (node list)
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

(defvar *jump-tree-id-counter* 0)

(defmacro jump-tree-generate-id ()
  ;; Generate a new, unique id (uninterned symbol).
  ;; The name is made by appending a number to "jump-tree-id".
  ;; (Copied from CL package `gensym'.)
  `(let ((num (prog1 *jump-tree-id-counter* (incf *jump-tree-id-counter*))))
     (make-symbol (format "jump-tree-id%d" num))))

(defun jump-tree-decircle (jump-tree)
  ;; Nullify PREVIOUS links of JUMP-TREE nodes, to make JUMP-TREE data
  ;; structure non-circular.
  (jump-tree-mapc
   (lambda (node)
     (dolist (n (jump-tree-node-next node))
       (setf (jump-tree-node-previous n) nil)))
   (jump-tree-root jump-tree)))

(defun jump-tree-recircle (jump-tree)
  ;; Recreate PREVIOUS links of JUMP-TREE nodes, to restore circular JUMP-TREE
  ;; data structure.
  (jump-tree-mapc
   (lambda (node)
     (dolist (n (jump-tree-node-next node))
       (setf (jump-tree-node-previous n) node)))
   (jump-tree-root jump-tree)))


;;; =====================================================================
;;;             Jump-Prev list and jump-prev changeset utility functions

(defmacro jump-prev-list-marker-elt-p (elt)
  `(markerp (car-safe ,elt)))

(defmacro jump-prev-list-GCd-marker-elt-p (elt)
  ;; Return t if ELT is a marker element whose marker has been moved to the
  ;; object-pool, so may potentially have been garbage-collected.
  ;; Note: Valid marker jump-prev elements should be uniquely identified as cons
  ;; cells with a symbol in the car (replacing the marker), and a number in
  ;; the cdr. However, to guard against future changes to jump-prev element
  ;; formats, we perform an additional redundant check on the symbol name.
  `(and (car-safe ,elt)
        (symbolp (car ,elt))
        (let ((str (symbol-name (car ,elt))))
          (and (> (length str) 12)
               (string= (substring str 0 12) "jump-tree-id")))
        (numberp (cdr-safe ,elt))))

(defun jump-tree-move-GC-elts-to-pool (elt)
  ;; Move elements that can be garbage-collected into `jump-tree-global-tree'
  ;; object pool, substituting a unique id that can be used to retrieve them
  ;; later. (Only markers require this treatment currently.)
  (when (jump-prev-list-marker-elt-p elt)
    (let ((id (jump-tree-generate-id)))
      (puthash id (car elt) (jump-tree-object-pool jump-tree-global-tree))
      (setcar elt id))))

(defun jump-tree-restore-GC-elts-from-pool (elt)
  ;; Replace object id's in ELT with corresponding objects from
  ;; `jump-tree-global-tree' object pool and return modified ELT, or return nil if
  ;; any object in ELT has been garbage-collected.
  (if (jump-prev-list-GCd-marker-elt-p elt)
      (when (setcar elt (gethash (car elt)
                                 (jump-tree-object-pool jump-tree-global-tree)))
        elt)
    elt))

(defun jump-prev-list-clean-GCd-elts (jump-prev-list)
  ;; Remove object id's from JUMP-PREV-LIST that refer to elements that have been
  ;; garbage-collected. JUMP-PREV-LIST is modified by side-effect.
  (while (jump-prev-list-GCd-marker-elt-p (car jump-prev-list))
    (unless (gethash (caar jump-prev-list)
                     (jump-tree-object-pool jump-tree-global-tree))
      (setq jump-prev-list (cdr jump-prev-list))))
  (let ((p jump-prev-list))
    (while (cdr p)
      (when (and (jump-prev-list-GCd-marker-elt-p (cadr p))
                 (null (gethash (car (cadr p))
                                (jump-tree-object-pool jump-tree-global-tree))))
        (setcdr p (cddr p)))
      (setq p (cdr p))))
  jump-prev-list)

(defun jump-prev-list-pop-changeset (&optional discard-pos)
  ;; Pop changeset from `jump-tree-global-list'. If DISCARD-POS is non-nil, discard
  ;; any position entries from changeset.

  ;; discard jump-prev boundaries and (if DISCARD-POS is non-nil) position entries
  ;; at head of jump-prev list
  (while (or (null (car jump-tree-global-list))
             (and discard-pos (integerp (car jump-tree-global-list))))
    (setq jump-tree-global-list (cdr jump-tree-global-list)))
  ;; pop elements up to next jump-prev boundary, discarding position entries if
  ;; DISCARD-POS is non-nil
  (if (eq (car jump-tree-global-list) 'jump-tree-canary)
      (push nil jump-tree-global-list)
    (let* ((changeset (list (pop jump-tree-global-list)))
           (p changeset))
      (while (progn
               (jump-tree-move-GC-elts-to-pool (car p))
               (while (and discard-pos (integerp (car jump-tree-global-list)))
                 (setq jump-tree-global-list (cdr jump-tree-global-list)))
               (and (car jump-tree-global-list)
                    (not (eq (car jump-tree-global-list) 'jump-tree-canary))))
        (setcdr p (list (pop jump-tree-global-list)))
        (setq p (cdr p)))
      changeset)))

(defun jump-tree-copy-list (jump-prev-list)
  ;; Return a deep copy of first changeset in `jump-prev-list'. Object id's are
  ;; replaced by corresponding objects from `jump-tree-global-tree' object-pool.
  (let (copy p)
    ;; if first element contains an object id, replace it with object from
    ;; pool, discarding element entirely if it's been GC'd
    (while (and jump-prev-list (null copy))
      (setq copy
            (jump-tree-restore-GC-elts-from-pool (pop jump-prev-list))))
    (when copy
      (setq copy (list copy)
            p copy)
      ;; copy remaining elements, replacing object id's with objects from
      ;; pool, or discarding them entirely if they've been GC'd
      (while jump-prev-list
        (when (setcdr p (jump-tree-restore-GC-elts-from-pool
                         (jump-prev-copy-list-1 (pop jump-prev-list))))
          (setcdr p (list (cdr p)))
          (setq p (cdr p))))
      copy)))

(defun jump-prev-list-transfer-to-tree ()
  ;; Transfer entries accumulated in `jump-tree-global-list' to `jump-tree-global-tree'.

  ;; `jump-prev-list-transfer-to-tree' should never be called when jump-prev is disabled
  ;; (i.e. `jump-tree-global-tree' is t)
  (assert (not (eq jump-tree-global-tree t)))

  ;; if `jump-tree-global-tree' is empty, create initial jump-tree
  (when (null jump-tree-global-tree) (setq jump-tree-global-tree (make-jump-tree)))
  ;; make sure there's a canary at end of `jump-tree-global-list'
  (when (null jump-tree-global-list)
    (setq jump-tree-global-list '(nil jump-tree-canary)))

  (unless (or (eq (cadr jump-tree-global-list) 'jump-tree-canary)
              (eq (car jump-tree-global-list) 'jump-tree-canary))
    ;; create new node from first changeset in `jump-tree-global-list', save old
    ;; `jump-tree-global-tree' current node, and make new node the current node
    (let* ((node (jump-tree-make-node nil (jump-prev-list-pop-changeset)))
           (splice (jump-tree-current jump-tree-global-tree))
           (size (jump-prev-list-byte-size (jump-tree-node-jump-prev node)))
           (count 1))
      (setf (jump-tree-current jump-tree-global-tree) node)
      ;; grow tree fragment backwards using `jump-tree-global-list' changesets
      (while (and jump-tree-global-list
                  (not (eq (cadr jump-tree-global-list) 'jump-tree-canary)))
        (setq node
              (jump-tree-grow-backwards node (jump-prev-list-pop-changeset)))
        (incf size (jump-prev-list-byte-size (jump-tree-node-jump-prev node)))
        (incf count))
      ;; if no jump-prev history has been discarded from `jump-tree-global-list' since
      ;; last transfer, splice new tree fragment onto end of old
      ;; `jump-tree-global-tree' current node
      (if (or (eq (cadr jump-tree-global-list) 'jump-tree-canary)
              (eq (car jump-tree-global-list) 'jump-tree-canary))
          (progn
            (setf (jump-tree-node-previous node) splice)
            (push node (jump-tree-node-next splice))
            (setf (jump-tree-node-branch splice) 0)
            (incf (jump-tree-size jump-tree-global-tree) size)
            (incf (jump-tree-count jump-tree-global-tree) count))
        ;; if jump-prev history has been discarded, replace entire
        ;; `jump-tree-global-tree' with new tree fragment
        (setq node (jump-tree-grow-backwards node nil))
        (setf (jump-tree-root jump-tree-global-tree) node)
        (setq jump-tree-global-list '(nil jump-tree-canary))
        (setf (jump-tree-size jump-tree-global-tree) size)
        (setf (jump-tree-count jump-tree-global-tree) count)
        (setq jump-tree-global-list '(nil jump-tree-canary))))
    ;; discard jump-prev history if necessary
    (jump-tree-discard-history)))

(defun jump-prev-list-byte-size (jump-prev-list)
  ;; Return size (in bytes) of JUMP-PREV-LIST
  (let ((size 0) (p jump-prev-list))
    (while p
      (incf size 8)  ; cons cells use up 8 bytes
      (when (and (consp (car p)) (stringp (caar p)))
        (incf size (string-bytes (caar p))))
      (setq p (cdr p)))
    size))

(defun jump-prev-list-rebuild-from-tree ()
  "Rebuild `jump-tree-global-list' from information in `jump-tree-global-tree'."
  (unless (eq jump-tree-global-list t)
    (jump-prev-list-transfer-to-tree)
    (setq jump-tree-global-list nil)
    (when jump-tree-global-tree
      (let ((stack (list (list (jump-tree-root jump-tree-global-tree)))))
        (push (sort (mapcar 'identity (jump-tree-node-next (caar stack)))
                    (lambda (a b)
                      (time-less-p (jump-tree-node-timestamp a)
                                   (jump-tree-node-timestamp b))))
              stack)
        ;; Traverse tree in depth-and-oldest-first order, but add jump-prev records
        ;; on the way down, and jump-next records on the way up.
        (while (or (car stack)
                   (not (eq (car (nth 1 stack))
                            (jump-tree-current jump-tree-global-tree))))
          (if (car stack)
              (progn
                (setq jump-tree-global-list
                      (append (jump-tree-node-jump-prev (caar stack))
                              jump-tree-global-list))
                (jump-prev-boundary)
                (push (sort (mapcar 'identity
                                    (jump-tree-node-next (caar stack)))
                            (lambda (a b)
                              (time-less-p (jump-tree-node-timestamp a)
                                           (jump-tree-node-timestamp b))))
                      stack))
            (pop stack)
            (setq jump-tree-global-list
                  (append (jump-tree-node-jump-next (caar stack))
                          jump-tree-global-list))
            (jump-prev-boundary)
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
  ;; Discard NODE from `jump-tree-global-tree', and return next in line for
  ;; discarding.

  ;; don't discard current node
  (unless (eq node (jump-tree-current jump-tree-global-tree))

    ;; discarding root node...
    (if (eq node (jump-tree-root jump-tree-global-tree))
        (cond
         ;; should always discard branches before root
         ((> (length (jump-tree-node-next node)) 1)
          (error "Trying to discard jump-tree root which still\
 has multiple branches"))
         ;; don't discard root if current node is only child
         ((eq (car (jump-tree-node-next node))
              (jump-tree-current jump-tree-global-tree))
          nil)
         ;; discard root
         (t
          ;; clear any register referring to root
          (let ((r (jump-tree-node-register node)))
            (when (and r (eq (get-register r) node))
              (set-register r nil)))
          ;; make child of root into new root
          (setq node (setf (jump-tree-root jump-tree-global-tree)
                           (car (jump-tree-node-next node))))
          ;; update jump-tree size
          (decf (jump-tree-size jump-tree-global-tree)
                (+ (jump-prev-list-byte-size (jump-tree-node-jump-prev node))
                   (jump-prev-list-byte-size (jump-tree-node-jump-next node))))
          (decf (jump-tree-count jump-tree-global-tree))
          ;; discard new root's jump-prev data and PREVIOUS link
          (setf (jump-tree-node-jump-prev node) nil
                (jump-tree-node-jump-next node) nil
                (jump-tree-node-previous node) nil)
          ;; if new root has branches, or new root is current node, next node
          ;; to discard is oldest leaf, otherwise it's new root
          (if (or (> (length (jump-tree-node-next node)) 1)
                  (eq (car (jump-tree-node-next node))
                      (jump-tree-current jump-tree-global-tree)))
              (jump-tree-oldest-leaf node)
            node)))

      ;; discarding leaf node...
      (let* ((parent (jump-tree-node-previous node))
             (current (nth (jump-tree-node-branch parent)
                           (jump-tree-node-next parent))))
        ;; clear any register referring to the discarded node
        (let ((r (jump-tree-node-register node)))
          (when (and r (eq (get-register r) node))
            (set-register r nil)))
        ;; update jump-tree size
        (decf (jump-tree-size jump-tree-global-tree)
              (+ (jump-prev-list-byte-size (jump-tree-node-jump-prev node))
                 (jump-prev-list-byte-size (jump-tree-node-jump-next node))))
        (decf (jump-tree-count jump-tree-global-tree))
        ;; discard leaf
        (setf (jump-tree-node-next parent)
              (delq node (jump-tree-node-next parent))
              (jump-tree-node-branch parent)
              (jump-tree-position current (jump-tree-node-next parent)))
        ;; if parent has branches, or parent is current node, next node to
        ;; discard is oldest leaf, otherwise it's the parent itself
        (if (or (eq parent (jump-tree-current jump-tree-global-tree))
                (and (jump-tree-node-next parent)
                     (or (not (eq parent (jump-tree-root jump-tree-global-tree)))
                         (> (length (jump-tree-node-next parent)) 1))))
            (jump-tree-oldest-leaf parent)
          parent)))))

(defun jump-tree-discard-history ()
  "Discard jump-prev history until we're within memory usage limits
set by `jump-prev-limit', `jump-prev-strong-limit' and `jump-prev-outer-limit'."

  (when (> (jump-tree-size jump-tree-global-tree) jump-prev-limit)
    ;; if there are no branches off root, first node to discard is root;
    ;; otherwise it's leaf node at botom of oldest branch
    (let ((node (if (> (length (jump-tree-node-next
                                (jump-tree-root jump-tree-global-tree))) 1)
                    (jump-tree-oldest-leaf (jump-tree-root jump-tree-global-tree))
                  (jump-tree-root jump-tree-global-tree))))

      ;; discard nodes until memory use is within `jump-prev-strong-limit'
      (while (and node
                  (> (jump-tree-size jump-tree-global-tree) jump-prev-strong-limit))
        (setq node (jump-tree-discard-node node)))

      ;; discard nodes until next node to discard would bring memory use
      ;; within `jump-prev-limit'
      (while (and node
                  ;; check first if last discard has brought us within
                  ;; `jump-prev-limit', in case we can avoid more expensive
                  ;; `jump-prev-strong-limit' calculation
                  ;; Note: this assumes jump-prev-strong-limit > jump-prev-limit;
                  ;;       if not, effectively jump-prev-strong-limit = jump-prev-limit
                  (> (jump-tree-size jump-tree-global-tree) jump-prev-limit)
                  (> (- (jump-tree-size jump-tree-global-tree)
                        ;; if next node to discard is root, the memory we
                        ;; free-up comes from discarding changesets from its
                        ;; only child...
                        (if (eq node (jump-tree-root jump-tree-global-tree))
                            (+ (jump-prev-list-byte-size
                                (jump-tree-node-jump-prev
                                 (car (jump-tree-node-next node))))
                               (jump-prev-list-byte-size
                                (jump-tree-node-jump-next
                                 (car (jump-tree-node-next node)))))
                          ;; ...otherwise, it comes from discarding changesets
                          ;; from along with the node itself
                          (+ (jump-prev-list-byte-size (jump-tree-node-jump-prev node))
                             (jump-prev-list-byte-size (jump-tree-node-jump-next node)))
                          ))
                     jump-prev-limit))
        (setq node (jump-tree-discard-node node)))

      ;; if we're still over the `jump-prev-outer-limit', discard entire history
      (when (> (jump-tree-size jump-tree-global-tree) jump-prev-outer-limit)
        ;; query first if `jump-prev-ask-before-discard' is set
        (if jump-prev-ask-before-discard
            (when (yes-or-no-p
                   (format
                    "Buffer `%s' jump-prev info is %d bytes long;  discard it? "
                    (buffer-name) (jump-tree-size jump-tree-global-tree)))
              (setq jump-tree-global-tree nil))
          ;; otherwise, discard and display warning
          (display-warning
           '(jump-prev discard-info)
           (concat
            (format "Buffer `%s' jump-prev info was %d bytes long.\n"
                    (buffer-name) (jump-tree-size jump-tree-global-tree))
            "The jump-prev info was discarded because it exceeded\
 `jump-prev-outer-limit'.

This is normal if you executed a command that made a huge change
to the buffer. In that case, to prevent similar problems in the
future, set `jump-prev-outer-limit' to a value that is large enough to
cover the maximum size of normal changes you expect a single
command to make, but not so large that it might exceed the
maximum memory allotted to Emacs.

If you did not execute any such command, the situation is
probably due to a bug and you should report it.

You can disable the popping up of this buffer by adding the entry
\(jump-prev discard-info) to the user option `warning-suppress-types',
which is defined in the `warnings' library.\n")
           :warning)
          (setq jump-tree-global-tree nil)))
      )))


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

(defun jump-tree-node-unmodified-p (node &optional mtime)
  ;; Return non-nil if NODE corresponds to a buffer state that once upon a
  ;; time was unmodified. If a file modification time MTIME is specified,
  ;; return non-nil if the corresponding buffer state really is unmodified.
  (let (changeset ntime)
    (setq changeset
          (or (jump-tree-node-jump-next node)
              (and (setq changeset (car (jump-tree-node-next node)))
                   (jump-tree-node-jump-prev changeset)))
          ntime
          (catch 'found
            (dolist (elt changeset)
              (when (and (consp elt) (eq (car elt) t) (consp (cdr elt))
                         (throw 'found (cdr elt)))))))
    (and ntime
         (or (null mtime)
             ;; high-precision timestamps
             (if (listp (cdr ntime))
                 (equal ntime mtime)
               ;; old-style timestamps
               (and (= (car ntime) (car mtime))
                    (= (cdr ntime) (cadr mtime))))))))


;;; =====================================================================
;;;                  Jump-Prev-in-region utility functions

;; `jump-prev-elt-in-region' uses this as a dynamically-scoped variable
(defvar jump-prev-adjusted-markers nil)

(defun jump-tree-pull-jump-prev-in-region-branch (start end)
  ;; Pull out entries from jump-prev changesets to create a new jump-prev-in-region
  ;; branch, which jump-preves changeset entries lying between START and END first,
  ;; followed by remaining entries from the changesets, before rejoining the
  ;; existing jump-prev tree history. Repeated calls will, if appropriate, extend
  ;; the current jump-prev-in-region branch rather than creating a new one.
  ;; if we're just reverting the last jump-next-in-region, we don't need to
  ;; manipulate the jump-prev tree at all
  (if (jump-tree-reverting-jump-next-in-region-p start end)
      t  ; return t to indicate success
    ;; We build the `region-changeset' and `delta-list' lists forwards, using
    ;; pointers `r' and `d' to the penultimate element of the list. So that we
    ;; don't have to treat the first element differently, we prepend a dummy
    ;; leading nil to the lists, and have the pointers point to that
    ;; initially.
    ;; Note: using '(nil) instead of (list nil) in the `let*' results in
    ;;       bizarre errors when the code is byte-compiled, where parts of the
    ;;       lists appear to survive across different calls to this function.
    ;;       An obscure byte-compiler bug, perhaps?
    (let* ((region-changeset (list nil))
           (r region-changeset)
           (delta-list (list nil))
           (d delta-list)
           (node (jump-tree-current jump-tree-global-tree))
           (repeated-jump-prev-in-region
            (jump-tree-repeated-jump-prev-in-region-p start end))
           jump-prev-adjusted-markers  ; `jump-prev-elt-in-region' expects this
           fragment splice original-fragment original-splice original-current
           got-visible-elt jump-prev-list elt)
      ;; --- initialisation ---
      (cond
       ;; if this is a repeated jump-prev in the same region, start pulling changes
       ;; from NODE at which jump-prev-in-region branch iss attached, and detatch
       ;; the branch, using it as initial FRAGMENT of branch being constructed
       (repeated-jump-prev-in-region
        (setq original-current node
              fragment (car (jump-tree-node-next node))
              splice node)
        ;; jump-prev up to node at which jump-prev-in-region branch is attached
        ;; (recognizable as first node with more than one branch)
        (let ((mark-active nil))
          (while (= (length (jump-tree-node-next node)) 1)
            (jump-tree-jump-prev-1)
            (setq fragment node
                  node (jump-tree-current jump-tree-global-tree))))
        (when (eq splice node) (setq splice nil))
        ;; detatch jump-prev-in-region branch
        (setf (jump-tree-node-next node)
              (delq fragment (jump-tree-node-next node))
              (jump-tree-node-previous fragment) nil
              original-fragment fragment
              original-splice node))
       ;; if this is a new jump-prev-in-region, initial FRAGMENT is a copy of all
       ;; nodes below the current one in the active branch
       ((jump-tree-node-next node)
        (setq fragment (jump-tree-make-node nil nil)
              splice fragment)
        (while (setq node (nth (jump-tree-node-branch node)
                               (jump-tree-node-next node)))
          (push (jump-tree-make-node
                 splice
                 (jump-prev-copy-list (jump-tree-node-jump-prev node))
                 (jump-prev-copy-list (jump-tree-node-jump-next node)))
                (jump-tree-node-next splice))
          (setq splice (car (jump-tree-node-next splice))))
        (setq fragment (car (jump-tree-node-next fragment))
              splice nil
              node (jump-tree-current jump-tree-global-tree))))

      ;; --- pull jump-prev-in-region elements into branch ---
      ;; work backwards up tree, pulling out jump-prev elements within region until
      ;; we've got one that jump-preves a visible change (insertion or deletion)
      (catch 'abort
        (while (and (not got-visible-elt) node (jump-tree-node-jump-prev node))
          ;; we cons a dummy nil element on the front of the changeset so that
          ;; we can conveniently remove the first (real) element from the
          ;; changeset if we need to; the leading nil is removed once we're
          ;; done with this changeset
          (setq jump-prev-list (cons nil (jump-prev-copy-list (jump-tree-node-jump-prev node)))
                elt (cadr jump-prev-list))
          (if fragment
              (progn
                (setq fragment (jump-tree-grow-backwards fragment jump-prev-list))
                (unless splice (setq splice fragment)))
            (setq fragment (jump-tree-make-node nil jump-prev-list))
            (setq splice fragment))
          (while elt
            (cond
             ;; keep elements within region
             ((jump-prev-elt-in-region elt start end)
              ;; set flag if kept element is visible (insertion or deletion)
              (when (and (consp elt)
                         (or (stringp (car elt)) (integerp (car elt))))
                (setq got-visible-elt t))
              ;; adjust buffer positions in elements previously jump-prevne before
              ;; kept element, as kept element will now be jump-prevne first
              (jump-tree-adjust-elements-to-elt splice elt)
              ;; move kept element to jump-prev-in-region changeset, adjusting its
              ;; buffer position as it will now be jump-prevne first
              (setcdr r (list (jump-tree-apply-deltas elt (cdr delta-list))))
              (setq r (cdr r))
              (setcdr jump-prev-list (cddr jump-prev-list)))
             ;; discard "was unmodified" elements
             ;; FIXME: deal properly with these
             ((and (consp elt) (eq (car elt) t))
              (setcdr jump-prev-list (cddr jump-prev-list)))
             ;; if element crosses region, we can't pull any more elements
             ((jump-prev-elt-crosses-region elt start end)
              ;; if we've found a visible element, it must be earlier in
              ;; current node's changeset; stop pulling elements (null
              ;; `jump-prev-list' and non-nil `got-visible-elt' cause loop to exit)
              (if got-visible-elt
                  (setq jump-prev-list nil)
                ;; if we haven't found a visible element yet, pulling
                ;; jump-prev-in-region branch has failed
                (setq region-changeset nil)
                (throw 'abort t)))
             ;; if rejecting element, add its delta (if any) to the list
             (t
              (let ((delta (jump-prev-delta elt)))
                (when (/= 0 (cdr delta))
                  (setcdr d (list delta))
                  (setq d (cdr d))))
              (setq jump-prev-list (cdr jump-prev-list))))
            ;; process next element of current changeset
            (setq elt (cadr jump-prev-list)))
          ;; if there are remaining elements in changeset, remove dummy nil
          ;; from front
          (if (cadr (jump-tree-node-jump-prev fragment))
              (pop (jump-tree-node-jump-prev fragment))
            ;; otherwise, if we've kept all elements in changeset, discard
            ;; empty changeset
            (when (eq splice fragment) (setq splice nil))
            (setq fragment (car (jump-tree-node-next fragment))))
          ;; process changeset from next node up the tree
          (setq node (jump-tree-node-previous node))))
      ;; pop dummy nil from front of `region-changeset'
      (setq region-changeset (cdr region-changeset))

      ;; --- integrate branch into tree ---
      ;; if no jump-prev-in-region elements were found, restore jump-prev tree
      (if (null region-changeset)
          (when original-current
            (push original-fragment (jump-tree-node-next original-splice))
            (setf (jump-tree-node-branch original-splice) 0
                  (jump-tree-node-previous original-fragment) original-splice)
            (let ((mark-active nil))
              (while (not (eq (jump-tree-current jump-tree-global-tree)
                              original-current))
                (jump-tree-jump-next-1)))
            nil)  ; return nil to indicate failure
        ;; otherwise...
        ;; need to jump-prev up to node where new branch will be attached, to
        ;; ensure jump-next entries are populated, and then jump-next back to where we
        ;; started
        (let ((mark-active nil)
              (current (jump-tree-current jump-tree-global-tree)))
          (while (not (eq (jump-tree-current jump-tree-global-tree) node))
            (jump-tree-jump-prev-1))
          (while (not (eq (jump-tree-current jump-tree-global-tree) current))
            (jump-tree-jump-next-1)))
        (cond
         ;; if there's no remaining fragment, just create jump-prev-in-region node
         ;; and attach it to parent of last node from which elements were
         ;; pulled
         ((null fragment)
          (setq fragment (jump-tree-make-node node region-changeset))
          (push fragment (jump-tree-node-next node))
          (setf (jump-tree-node-branch node) 0)
          ;; set current node to jump-prev-in-region node
          (setf (jump-tree-current jump-tree-global-tree) fragment))
         ;; if no splice point has been set, add jump-prev-in-region node to top of
         ;; fragment and attach it to parent of last node from which elements
         ;; were pulled
         ((null splice)
          (setq fragment (jump-tree-grow-backwards fragment region-changeset))
          (push fragment (jump-tree-node-next node))
          (setf (jump-tree-node-branch node) 0
                (jump-tree-node-previous fragment) node)
          ;; set current node to jump-prev-in-region node
          (setf (jump-tree-current jump-tree-global-tree) fragment))
         ;; if fragment contains nodes, attach fragment to parent of last node
         ;; from which elements were pulled, and splice in jump-prev-in-region node
         (t
          (setf (jump-tree-node-previous fragment) node)
          (push fragment (jump-tree-node-next node))
          (setf (jump-tree-node-branch node) 0)
          ;; if this is a repeated jump-prev-in-region, then we've left the current
          ;; node at the original splice-point; we need to set the current
          ;; node to the equivalent node on the jump-prev-in-region branch and jump-next
          ;; back to where we started
          (when repeated-jump-prev-in-region
            (setf (jump-tree-current jump-tree-global-tree)
                  (jump-tree-node-previous original-fragment))
            (let ((mark-active nil))
              (while (not (eq (jump-tree-current jump-tree-global-tree) splice))
                (jump-tree-jump-next-1 nil 'preserve-jump-prev))))
          ;; splice new jump-prev-in-region node into fragment
          (setq node (jump-tree-make-node nil region-changeset))
          (jump-tree-splice-node node splice)
          ;; set current node to jump-prev-in-region node
          (setf (jump-tree-current jump-tree-global-tree) node)))
        ;; update jump-tree size
        (setq node (jump-tree-node-previous fragment))
        (while (progn
                 (and (setq node (car (jump-tree-node-next node)))
                      (not (eq node original-fragment))
                      (incf (jump-tree-count jump-tree-global-tree))
                      (incf (jump-tree-size jump-tree-global-tree)
                            (+ (jump-prev-list-byte-size (jump-tree-node-jump-prev node))
                               (jump-prev-list-byte-size (jump-tree-node-jump-next node)))))))
        t)  ; indicate jump-prev-in-region branch was successfully pulled
      )))

(defun jump-tree-pull-jump-next-in-region-branch (start end)
  ;; Pull out entries from jump-next changesets to create a new jump-next-in-region
  ;; branch, which jump-nextes changeset entries lying between START and END first,
  ;; followed by remaining entries from the changesets. Repeated calls will,
  ;; if appropriate, extend the current jump-next-in-region branch rather than
  ;; creating a new one.
  ;; if we're just reverting the last jump-prev-in-region, we don't need to
  ;; manipulate the jump-prev tree at all
  (if (jump-tree-reverting-jump-prev-in-region-p start end)
      t  ; return t to indicate success
    ;; We build the `region-changeset' and `delta-list' lists forwards, using
    ;; pointers `r' and `d' to the penultimate element of the list. So that we
    ;; don't have to treat the first element differently, we prepend a dummy
    ;; leading nil to the lists, and have the pointers point to that
    ;; initially.
    ;; Note: using '(nil) instead of (list nil) in the `let*' causes bizarre
    ;;       errors when the code is byte-compiled, where parts of the lists
    ;;       appear to survive across different calls to this function.  An
    ;;       obscure byte-compiler bug, perhaps?
    (let* ((region-changeset (list nil))
           (r region-changeset)
           (delta-list (list nil))
           (d delta-list)
           (node (jump-tree-current jump-tree-global-tree))
           (repeated-jump-next-in-region
            (jump-tree-repeated-jump-next-in-region-p start end))
           jump-prev-adjusted-markers  ; `jump-prev-elt-in-region' expects this
           fragment splice got-visible-elt jump-next-list elt)
      ;; --- inisitalisation ---
      (cond
       ;; if this is a repeated jump-next-in-region, detach fragment below current
       ;; node
       (repeated-jump-next-in-region
        (when (setq fragment (car (jump-tree-node-next node)))
          (setf (jump-tree-node-previous fragment) nil
                (jump-tree-node-next node)
                (delq fragment (jump-tree-node-next node)))))
       ;; if this is a new jump-next-in-region, initial fragment is a copy of all
       ;; nodes below the current one in the active branch
       ((jump-tree-node-next node)
        (setq fragment (jump-tree-make-node nil nil)
              splice fragment)
        (while (setq node (nth (jump-tree-node-branch node)
                               (jump-tree-node-next node)))
          (push (jump-tree-make-node
                 splice nil
                 (jump-prev-copy-list (jump-tree-node-jump-next node)))
                (jump-tree-node-next splice))
          (setq splice (car (jump-tree-node-next splice))))
        (setq fragment (car (jump-tree-node-next fragment)))))

      ;; --- pull jump-next-in-region elements into branch ---
      ;; work down fragment, pulling out jump-next elements within region until
      ;; we've got one that jump-nextes a visible change (insertion or deletion)
      (setq node fragment)
      (catch 'abort
        (while (and (not got-visible-elt) node (jump-tree-node-jump-next node))
          ;; we cons a dummy nil element on the front of the changeset so that
          ;; we can conveniently remove the first (real) element from the
          ;; changeset if we need to; the leading nil is removed once we're
          ;; done with this changeset
          (setq jump-next-list (push nil (jump-tree-node-jump-next node))
                elt (cadr jump-next-list))
          (while elt
            (cond
             ;; keep elements within region
             ((jump-prev-elt-in-region elt start end)
              ;; set flag if kept element is visible (insertion or deletion)
              (when (and (consp elt)
                         (or (stringp (car elt)) (integerp (car elt))))
                (setq got-visible-elt t))
              ;; adjust buffer positions in elements previously jump-nextne before
              ;; kept element, as kept element will now be jump-nextne first
              (jump-tree-adjust-elements-to-elt fragment elt t)
              ;; move kept element to jump-next-in-region changeset, adjusting its
              ;; buffer position as it will now be jump-nextne first
              (setcdr r (list (jump-tree-apply-deltas elt (cdr delta-list) -1)))
              (setq r (cdr r))
              (setcdr jump-next-list (cddr jump-next-list)))
             ;; discard "was unmodified" elements
             ;; FIXME: deal properly with these
             ((and (consp elt) (eq (car elt) t))
              (setcdr jump-next-list (cddr jump-next-list)))
             ;; if element crosses region, we can't pull any more elements
             ((jump-prev-elt-crosses-region elt start end)
              ;; if we've found a visible element, it must be earlier in
              ;; current node's changeset; stop pulling elements (null
              ;; `jump-next-list' and non-nil `got-visible-elt' cause loop to exit)
              (if got-visible-elt
                  (setq jump-next-list nil)
                ;; if we haven't found a visible element yet, pulling
                ;; jump-next-in-region branch has failed
                (setq region-changeset nil)
                (throw 'abort t)))
             ;; if rejecting element, add its delta (if any) to the list
             (t
              (let ((delta (jump-prev-delta elt)))
                (when (/= 0 (cdr delta))
                  (setcdr d (list delta))
                  (setq d (cdr d))))
              (setq jump-next-list (cdr jump-next-list))))
            ;; process next element of current changeset
            (setq elt (cadr jump-next-list)))
          ;; if there are remaining elements in changeset, remove dummy nil
          ;; from front
          (if (cadr (jump-tree-node-jump-next node))
              (pop (jump-tree-node-jump-prev node))
            ;; otherwise, if we've kept all elements in changeset, discard
            ;; empty changeset
            (if (eq fragment node)
                (setq fragment (car (jump-tree-node-next fragment)))
              (jump-tree-snip-node node)))
          ;; process changeset from next node in fragment
          (setq node (car (jump-tree-node-next node)))))
      ;; pop dummy nil from front of `region-changeset'
      (setq region-changeset (cdr region-changeset))

      ;; --- integrate branch into tree ---
      (setq node (jump-tree-current jump-tree-global-tree))
      ;; if no jump-next-in-region elements were found, restore jump-prev tree
      (if (null (car region-changeset))
          (when (and repeated-jump-next-in-region fragment)
            (push fragment (jump-tree-node-next node))
            (setf (jump-tree-node-branch node) 0
                  (jump-tree-node-previous fragment) node)
            nil)  ; return nil to indicate failure
        ;; otherwise, add jump-next-in-region node to top of fragment, and attach
        ;; it below current node
        (setq fragment
              (if fragment
                  (jump-tree-grow-backwards fragment nil region-changeset)
                (jump-tree-make-node nil nil region-changeset)))
        (push fragment (jump-tree-node-next node))
        (setf (jump-tree-node-branch node) 0
              (jump-tree-node-previous fragment) node)
        ;; update jump-tree size
        (unless repeated-jump-next-in-region
          (setq node fragment)
          (while (and (setq node (car (jump-tree-node-next node)))
                      (incf (jump-tree-count jump-tree-global-tree))
                      (incf (jump-tree-size jump-tree-global-tree)
                            (jump-prev-list-byte-size
                             (jump-tree-node-jump-next node))))))
        (incf (jump-tree-size jump-tree-global-tree)
              (jump-prev-list-byte-size (jump-tree-node-jump-next fragment)))
        t)  ; indicate jump-next-in-region branch was successfully pulled
      )))

(defun jump-tree-adjust-elements-to-elt (node jump-prev-elt &optional below)
  "Adjust buffer positions of jump-prev elements, starting at NODE's
and going up the tree (or down the active branch if BELOW is
non-nil) and through the nodes' jump-prev elements until we reach
JUMP-PREV-ELT.  JUMP-PREV-ELT must appear somewhere in the jump-prev changeset
of either NODE itself or some node above it in the tree."
  (let ((delta (list (jump-prev-delta jump-prev-elt)))
        (jump-prev-list (jump-tree-node-jump-prev node)))
    ;; adjust elements until we reach JUMP-PREV-ELT
    (while (and (car jump-prev-list)
                (not (eq (car jump-prev-list) jump-prev-elt)))
      (setcar jump-prev-list
              (jump-tree-apply-deltas (car jump-prev-list) delta -1))
      ;; move to next jump-prev element in list, or to next node if we've run out
      ;; of elements
      (unless (car (setq jump-prev-list (cdr jump-prev-list)))
        (if below
            (setq node (nth (jump-tree-node-branch node)
                            (jump-tree-node-next node)))
          (setq node (jump-tree-node-previous node)))
        (setq jump-prev-list (jump-tree-node-jump-prev node))))))

(defun jump-tree-apply-deltas (jump-prev-elt deltas &optional sgn)
  ;; Apply DELTAS in order to JUMP-PREV-ELT, multiplying deltas by SGN
  ;; (only useful value for SGN is -1).
  (let (position offset)
    (dolist (delta deltas)
      (setq position (car delta)
            offset (* (cdr delta) (or sgn 1)))
      (cond
       ;; POSITION
       ((integerp jump-prev-elt)
        (when (>= jump-prev-elt position)
          (setq jump-prev-elt (- jump-prev-elt offset))))
       ;; nil (or any other atom)
       ((atom jump-prev-elt))
       ;; (TEXT . POSITION)
       ((stringp (car jump-prev-elt))
        (let ((text-pos (abs (cdr jump-prev-elt)))
              (point-at-end (< (cdr jump-prev-elt) 0)))
          (if (>= text-pos position)
              (setcdr jump-prev-elt (* (if point-at-end -1 1)
                                  (- text-pos offset))))))
       ;; (BEGIN . END)
       ((integerp (car jump-prev-elt))
        (when (>= (car jump-prev-elt) position)
          (setcar jump-prev-elt (- (car jump-prev-elt) offset))
          (setcdr jump-prev-elt (- (cdr jump-prev-elt) offset))))
       ;; (nil PROPERTY VALUE BEG . END)
       ((null (car jump-prev-elt))
        (let ((tail (nthcdr 3 jump-prev-elt)))
          (when (>= (car tail) position)
            (setcar tail (- (car tail) offset))
            (setcdr tail (- (cdr tail) offset)))))
       ))
    jump-prev-elt))

(defun jump-tree-repeated-jump-prev-in-region-p (start end)
  ;; Return non-nil if jump-prev-in-region between START and END is a repeated
  ;; jump-prev-in-region
  (let ((node (jump-tree-current jump-tree-global-tree)))
    (and (setq node
               (nth (jump-tree-node-branch node) (jump-tree-node-next node)))
         (eq (jump-tree-node-jump-prev-beginning node) start)
         (eq (jump-tree-node-jump-prev-end node) end))))

(defun jump-tree-repeated-jump-next-in-region-p (start end)
  ;; Return non-nil if jump-prev-in-region between START and END is a repeated
  ;; jump-prev-in-region
  (let ((node (jump-tree-current jump-tree-global-tree)))
    (and (eq (jump-tree-node-jump-next-beginning node) start)
         (eq (jump-tree-node-jump-next-end node) end))))

;; Return non-nil if jump-prev-in-region between START and END is simply
;; reverting the last jump-next-in-region
(defalias 'jump-tree-reverting-jump-prev-in-region-p
  'jump-tree-repeated-jump-prev-in-region-p)

;; Return non-nil if jump-next-in-region between START and END is simply
;; reverting the last jump-prev-in-region
(defalias 'jump-tree-reverting-jump-next-in-region-p
  'jump-tree-repeated-jump-next-in-region-p)


;;; =====================================================================
;;;                        jump-tree commands

;;;###autoload
(define-minor-mode jump-tree-mode
  "Toggle jump-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.
jump-tree-mode replaces Emacs' standard jump-prev feature with a more
powerful yet easier to use version, that treats the jump-prev history
as what it is: a tree.
The following keys are available in `jump-tree-mode':
  \\{jump-tree-map}
Within the jump-tree visualizer, the following keys are available:
  \\{jump-tree-visualizer-mode-map}"
  nil                       ; init value
  jump-tree-mode-lighter    ; lighter
  jump-tree-map             ; keymap
  ;; if disabling `jump-tree-mode', rebuild `jump-tree-global-list' from tree so
  ;; Emacs jump-prev can work
  (when (not jump-tree-mode)
    (jump-prev-list-rebuild-from-tree)
    (setq jump-tree-global-tree nil)))

(defun turn-on-jump-tree-mode (&optional print-message)
  "Enable `jump-tree-mode' in the current buffer, when appropriate.
Some major modes implement their own jump-prev system, which should
not normally be overridden by `jump-tree-mode'. This command does
not enable `jump-tree-mode' in such buffers. If you want to force
`jump-tree-mode' to be enabled regardless, use (jump-tree-mode 1)
instead.
The heuristic used to detect major modes in which
`jump-tree-mode' should not be used is to check whether either
the `jump-prev' command has been remapped, or the default jump-prev
keybindings (M-, and M-.) have been overridden somewhere other
than in the global map. In addition, `jump-tree-mode' will not be
enabled if the buffer's `major-mode' appears in
`jump-tree-incompatible-major-modes'."
  (interactive "p")
  (if (or (key-binding [remap jump-prev])
          (jump-tree-overridden-jump-prev-bindings-p)
          (memq major-mode jump-tree-incompatible-major-modes))
      (when print-message
        (message "Buffer does not support jump-tree-mode;\
 jump-tree-mode NOT enabled"))
    (jump-tree-mode 1)))

(defun jump-tree-overridden-jump-prev-bindings-p ()
  "Returns t if default jump-prev bindings are overridden, nil otherwise.
Checks if either of the default jump-prev key bindings (\"C-/\" or
\"C-_\") are overridden in the current buffer by any keymap other
than the global one. (So global redefinitions of the default jump-prev
key bindings do not count.)"
  (let ((binding1 (lookup-key (current-global-map) [?\C-/]))
        (binding2 (lookup-key (current-global-map) [?\C-_])))
    (global-set-key [?\C-/] 'jump-prev)
    (global-set-key [?\C-_] 'jump-prev)
    (unwind-protect
        (or (and (key-binding [?\C-/])
                 (not (eq (key-binding [?\C-/]) 'jump-prev)))
            (and (key-binding [?\C-_])
                 (not (eq (key-binding [?\C-_]) 'jump-prev))))
      (global-set-key [?\C-/] binding1)
      (global-set-key [?\C-_] binding2))))

;;;###autoload
(define-globalized-minor-mode global-jump-tree-mode
  jump-tree-mode turn-on-jump-tree-mode)

(defun jump-tree-jump-prev (&optional arg)
  "Jump-Prev changes.
Repeat this command to jump-prev more changes.
A numeric ARG serves as a repeat count.
In Transient Mark mode when the mark is active, only jump-prev changes
within the current region. Similarly, when not in Transient Mark
mode, just \\[universal-argument] as an argument limits jump-prev to
changes within the current region."
  (interactive "*P")
  (unless jump-tree-mode
    (user-error "jump-tree mode not enabled in buffer"))
  ;; throw error if jump-prev is disabled in buffer
  (when (eq jump-tree-global-list t)
    (user-error "No jump-prev information in this buffer"))
  (jump-tree-jump-prev-1 arg)
  ;; inform user if at branch point
  (when (> (jump-tree-num-branches) 1) (message "Jump-Prev branch point!")))

(defun jump-tree-jump-prev-1 (&optional arg preserve-jump-next preserve-timestamps)
  ;; Internal jump-prev function. Non-nil PRESERVE-JUMP-NEXT
  ;; causes the existing jump-next record to be preserved, rather than replacing it
  ;; with the new one generated by jump-preving. Non-nil PRESERVE-TIMESTAMPS
  ;; disables updating of timestamps in visited jump-tree nodes. (This latter
  ;; should *only* be used when temporarily visiting another jump-prev state and
  ;; immediately returning to the original state afterwards. Otherwise, it
  ;; could cause history-discarding errors.)
  (setq deactivate-mark t)
  (let ((jump-prev-in-progress t)
        (jump-prev-in-region (and jump-tree-enable-jump-prev-in-region
                             (or (region-active-p)
                                 (and arg (not (numberp arg))))))
        pos current)
    ;; transfer entries accumulated in `jump-tree-global-list' to
    ;; `jump-tree-global-tree'
    (jump-prev-list-transfer-to-tree)
    (dotimes (i (or (and (numberp arg) (prefix-numeric-value arg)) 1))
      ;; check if at top of jump-prev tree
      (unless (jump-tree-node-previous (jump-tree-current jump-tree-global-tree))
        (user-error "No further jump-prev information"))
      ;; remove any GC'd elements from node's jump-prev list
      (setq current (jump-tree-current jump-tree-global-tree))
      (decf (jump-tree-size jump-tree-global-tree)
            (jump-prev-list-byte-size (jump-tree-node-jump-prev current)))
      (setf (jump-tree-node-jump-prev current)
            (jump-prev-list-clean-GCd-elts (jump-tree-node-jump-prev current)))
      (incf (jump-tree-size jump-tree-global-tree)
            (jump-prev-list-byte-size (jump-tree-node-jump-prev current)))
      (primitive-undo 1 (jump-tree-copy-list (jump-tree-node-jump-prev current)))
      ;; if preserving old jump-next record, discard new jump-next entries that
      ;; `primitive-jump-prev' has added to `jump-tree-global-list', and remove any GC'd
      ;; elements from node's jump-next list
      (if preserve-jump-next
          (progn
            (jump-prev-list-pop-changeset)
            (decf (jump-tree-size jump-tree-global-tree)
                  (jump-prev-list-byte-size (jump-tree-node-jump-next current)))
            (setf (jump-tree-node-jump-next current)
                  (jump-prev-list-clean-GCd-elts (jump-tree-node-jump-next current)))
            (incf (jump-tree-size jump-tree-global-tree)
                  (jump-prev-list-byte-size (jump-tree-node-jump-next current))))
        ;; otherwise, record jump-next entries that `primitive-jump-prev' has added to
        ;; `jump-tree-global-list' in current node's jump-next record, replacing
        ;; existing entry if one already exists
        (decf (jump-tree-size jump-tree-global-tree)
              (jump-prev-list-byte-size (jump-tree-node-jump-next current)))
        (setf (jump-tree-node-jump-next current)
              (jump-prev-list-pop-changeset 'discard-pos))
        (incf (jump-tree-size jump-tree-global-tree)
              (jump-prev-list-byte-size (jump-tree-node-jump-next current))))
      ;; rewind current node and update timestamp
      (setf (jump-tree-current jump-tree-global-tree)
            (jump-tree-node-previous (jump-tree-current jump-tree-global-tree)))
      (unless preserve-timestamps
        (setf (jump-tree-node-timestamp (jump-tree-current jump-tree-global-tree))
              (current-time)))
      (jump-tree-node-clear-region-data current))))

(defun jump-tree-jump-next (&optional arg)
  "Jump-Next changes. A numeric ARG serves as a repeat count.
In Transient Mark mode when the mark is active, only jump-next changes
within the current region. Similarly, when not in Transient Mark
mode, just \\[universal-argument] as an argument limits jump-next to
changes within the current region."
  (interactive "*P")
  (unless jump-tree-mode
    (user-error "jump-tree mode not enabled in buffer"))
  ;; throw error if jump-prev is disabled in buffer
  (when (eq jump-tree-global-list t)
    (user-error "No jump-prev information in this buffer"))
  (jump-tree-jump-next-1 arg)
  ;; inform user if at branch point
  (when (> (jump-tree-num-branches) 1) (message "Jump-Prev branch point!")))

(defun jump-tree-jump-next-1 (&optional arg preserve-jump-prev preserve-timestamps)
  ;; Internal jump-next function. An active mark in `transient-mark-mode', or
  ;; non-nil ARG otherwise, enables jump-prev-in-region. Non-nil PRESERVE-JUMP-PREV
  ;; causes the existing jump-next record to be preserved, rather than replacing it
  ;; with the new one generated by jump-preving. Non-nil PRESERVE-TIMESTAMPS
  ;; disables updating of timestamps in visited jump-tree nodes. (This latter
  ;; should *only* be used when temporarily visiting another jump-prev state and
  ;; immediately returning to the original state afterwards. Otherwise, it
  ;; could cause history-discarding errors.)
  (setq deactivate-mark t)
  (let ((jump-prev-in-progress t)
        (jump-next-in-region (and jump-tree-enable-jump-prev-in-region
                             (or (region-active-p)
                                 (and arg (not (numberp arg))))))
        pos current)
    ;; transfer entries accumulated in `jump-tree-global-list' to
    ;; `jump-tree-global-tree'
    (jump-prev-list-transfer-to-tree)
    (dotimes (i (or (and (numberp arg) (prefix-numeric-value arg)) 1))
      ;; check if at bottom of jump-prev tree
      (when (null (jump-tree-node-next (jump-tree-current jump-tree-global-tree)))
        (user-error "No further jump-next information"))
      ;; get next node (but DON'T advance current node in tree yet, in case
      ;; jump-nexting fails)
      (setq current (jump-tree-current jump-tree-global-tree)
            current (nth (jump-tree-node-branch current)
                         (jump-tree-node-next current)))
      ;; remove any GC'd elements from node's jump-next list
      (decf (jump-tree-size jump-tree-global-tree)
            (jump-prev-list-byte-size (jump-tree-node-jump-next current)))
      (setf (jump-tree-node-jump-next current)
            (jump-prev-list-clean-GCd-elts (jump-tree-node-jump-next current)))
      (incf (jump-tree-size jump-tree-global-tree)
            (jump-prev-list-byte-size (jump-tree-node-jump-next current)))
      (primitive-undo 1 (jump-tree-copy-list (jump-tree-node-jump-next current)))
      ;; advance current node in tree
      (setf (jump-tree-current jump-tree-global-tree) current)
      ;; if preserving old jump-prev record, discard new jump-prev entries that
      ;; `primitive-jump-prev' has added to `jump-tree-global-list', and remove any GC'd
      ;; elements from node's jump-next list
      (if preserve-jump-prev
          (progn
            (jump-prev-list-pop-changeset)
            (decf (jump-tree-size jump-tree-global-tree)
                  (jump-prev-list-byte-size (jump-tree-node-jump-prev current)))
            (setf (jump-tree-node-jump-prev current)
                  (jump-prev-list-clean-GCd-elts (jump-tree-node-jump-prev current)))
            (incf (jump-tree-size jump-tree-global-tree)
                  (jump-prev-list-byte-size (jump-tree-node-jump-prev current))))
        ;; otherwise, record jump-prev entries that `primitive-jump-prev' has added to
        ;; `jump-tree-global-list' in current node's jump-prev record, replacing
        ;; existing entry if one already exists
        (decf (jump-tree-size jump-tree-global-tree)
              (jump-prev-list-byte-size (jump-tree-node-jump-prev current)))
        (setf (jump-tree-node-jump-prev current)
              (jump-prev-list-pop-changeset 'discard-pos))
        (incf (jump-tree-size jump-tree-global-tree)
              (jump-prev-list-byte-size (jump-tree-node-jump-prev current))))
      ;; update timestamp
      (unless preserve-timestamps
        (setf (jump-tree-node-timestamp current) (current-time)))
      (jump-tree-node-clear-region-data current))))

(defun jump-tree-switch-branch (branch)
  "Switch to a different BRANCH of the jump-prev tree.
This will affect which branch to descend when *jump-nexting* changes
using `jump-tree-jump-next'."
  (interactive (list (or (and prefix-arg (prefix-numeric-value prefix-arg))
                         (and (not (eq jump-tree-global-list t))
                              (or (jump-prev-list-transfer-to-tree) t)
                              (let ((b (jump-tree-node-branch
                                        (jump-tree-current
                                         jump-tree-global-tree))))
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
  ;; throw error if jump-prev is disabled in buffer
  (when (eq jump-tree-global-list t)
    (user-error "No jump-prev information in this buffer"))
  ;; sanity check branch number
  (when (<= (jump-tree-num-branches) 1)
    (user-error "Not at jump-prev branch point"))
  (when (or (< branch 0) (> branch (1- (jump-tree-num-branches))))
    (user-error "Invalid branch number"))
  ;; transfer entries accumulated in `jump-tree-global-list' to `jump-tree-global-tree'
  (jump-prev-list-transfer-to-tree)
  ;; switch branch
  (setf (jump-tree-node-branch (jump-tree-current jump-tree-global-tree))
        branch)
  (message "Switched to branch %d" branch))

(defun jump-tree-set (node &optional preserve-timestamps)
  ;; Set buffer to state corresponding to NODE. Returns intersection point
  ;; between path back from current node and path back from selected NODE.
  ;; Non-nil PRESERVE-TIMESTAMPS disables updating of timestamps in visited
  ;; jump-tree nodes. (This should *only* be used when temporarily visiting
  ;; another jump-prev state and immediately returning to the original state
  ;; afterwards. Otherwise, it could cause history-discarding errors.)
  (let ((path (make-hash-table :test 'eq))
        (n node))
    (puthash (jump-tree-root jump-tree-global-tree) t path)
    ;; build list of nodes leading back from selected node to root, updating
    ;; branches as we go to point down to selected node
    (while (progn
             (puthash n t path)
             (when (jump-tree-node-previous n)
               (setf (jump-tree-node-branch (jump-tree-node-previous n))
                     (jump-tree-position
                      n (jump-tree-node-next (jump-tree-node-previous n))))
               (setq n (jump-tree-node-previous n)))))
    ;; work backwards from current node until we intersect path back from
    ;; selected node
    (setq n (jump-tree-current jump-tree-global-tree))
    (while (not (gethash n path))
      (setq n (jump-tree-node-previous n)))
    ;; ascend tree until intersection node
    (while (not (eq (jump-tree-current jump-tree-global-tree) n))
      (jump-tree-jump-prev-1 nil nil preserve-timestamps))
    ;; descend tree until selected node
    (while (not (eq (jump-tree-current jump-tree-global-tree) node))
      (jump-tree-jump-next-1 nil nil preserve-timestamps))
    n))  ; return intersection node

(defun jump-tree-save-state-to-register (register)
  "Store current jump-tree state to REGISTER.
The saved state can be restored using
`jump-tree-restore-state-from-register'.
Argument is a character, naming the register."
  (interactive "cjump-tree state to register: ")
  (unless jump-tree-mode
    (user-error "jump-tree mode not enabled in buffer"))
  ;; throw error if jump-prev is disabled in buffer
  (when (eq jump-tree-global-list t)
    (user-error "No jump-prev information in this buffer"))
  ;; transfer entries accumulated in `jump-tree-global-list' to `jump-tree-global-tree'
  (jump-prev-list-transfer-to-tree)
  ;; save current node to REGISTER
  (set-register
   register (registerv-make
             (jump-tree-make-register-data
              (current-buffer) (jump-tree-current jump-tree-global-tree))
             :print-func 'jump-tree-register-data-print-func))
  ;; record REGISTER in current node, for visualizer
  (setf (jump-tree-node-register (jump-tree-current jump-tree-global-tree))
        register))

(defun jump-tree-restore-state-from-register (register)
  "Restore jump-tree state from REGISTER.
The state must be saved using `jump-tree-save-state-to-register'.
Argument is a character, naming the register."
  (interactive "*cRestore jump-tree state from register: ")
  (unless jump-tree-mode
    (user-error "jump-tree mode not enabled in buffer"))
  ;; throw error if jump-prev is disabled in buffer, or if register doesn't contain
  ;; an jump-tree node
  (let ((data (registerv-data (get-register register))))
    (cond
     ((eq jump-tree-global-list t)
      (user-error "No jump-prev information in this buffer"))
     ((not (jump-tree-register-data-p data))
      (user-error "Register doesn't contain jump-tree state"))
     ((not (eq (current-buffer) (jump-tree-register-data-buffer data)))
      (user-error "Register contains jump-tree state for a different buffer")))
    ;; transfer entries accumulated in `jump-tree-global-list' to `jump-tree-global-tree'
    (jump-prev-list-transfer-to-tree)
    ;; restore buffer state corresponding to saved node
    (jump-tree-set (jump-tree-register-data-node data))))


;;; =====================================================================
;;;                    Visualizer drawing functions
(defun jump-tree-visualize ()
  "Visualize the current buffer's jump-prev tree."
  (interactive "*")
  (unless jump-tree-mode
    (user-error "jump-tree mode not enabled in buffer"))
  (deactivate-mark)
  ;; throw error if jump-prev is disabled in buffer
  (when (eq jump-tree-global-list t)
    (user-error "No jump-prev information in this buffer"))
  ;; transfer entries accumulated in `jump-tree-global-list' to `jump-tree-global-tree'
  (jump-prev-list-transfer-to-tree)
  ;; add hook to kill visualizer buffer if original buffer is changed
  (add-hook 'before-change-functions 'jump-tree-kill-visualizer nil t)
  ;; prepare *jump-tree* buffer, then draw tree in it
  (let ((jump-tree jump-tree-global-tree)
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
    (make-local-variable 'jump-tree-visualizer-diff)
    (setq jump-tree-global-tree jump-tree)
    (jump-tree-visualizer-mode)
    ;; FIXME; don't know why `jump-tree-visualizer-mode' clears this
    (setq jump-tree-global-tree jump-tree)
    (set (make-local-variable 'jump-tree-visualizer-lazy-drawing)
         (or (eq jump-tree-visualizer-lazy-drawing t)
             (and (numberp jump-tree-visualizer-lazy-drawing)
                  (>= (jump-tree-count jump-tree)
                      jump-tree-visualizer-lazy-drawing))))
    (when jump-tree-visualizer-diff (jump-tree-visualizer-show-diff))
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
               (jump-tree-root jump-tree-global-tree))
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
         (unmodified (if jump-tree-visualizer-parent-mtime
                         (jump-tree-node-unmodified-p
                          node jump-tree-visualizer-parent-mtime)
                       (jump-tree-node-unmodified-p node)))
         node-string)
    ;; check node's register (if any) still stores appropriate jump-tree state
    (unless (and register
                 (jump-tree-register-data-p
                  (registerv-data (get-register register)))
                 (eq node (jump-tree-register-data-node
                           (registerv-data (get-register register)))))
      (setq register nil))
    ;; represent node by different symbols, depending on whether it's the
    ;; current node, is saved in a register, or corresponds to an unmodified
    ;; buffer
    (setq node-string
          (cond
           (jump-tree-visualizer-timestamps
            (jump-tree-timestamp-to-string
             (jump-tree-node-timestamp node)
             jump-tree-visualizer-relative-timestamps
             current register))
           (register (char-to-string register))
           (unmodified "s")
           (current "x")
           (t "o"))
          jump-tree-insert-face
          (nconc
           (cond
            (current    '(jump-tree-visualizer-current-face))
            (unmodified '(jump-tree-visualizer-unmodified-face))
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
      (setq p (jump-tree-position node n))
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
jump-prev history tree graphically, and allows you to browse around
the jump-prev history, jump-preving or jump-nexting the corresponding changes in
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
  (let ((old (jump-tree-current jump-tree-global-tree))
        current)
    ;; unhighlight old current node
    (let ((jump-tree-insert-face 'jump-tree-visualizer-active-branch-face)
          (inhibit-read-only t))
      (jump-tree-draw-node old))
    ;; jump-prev in parent buffer
    (switch-to-buffer-other-window jump-tree-visualizer-parent-buffer)
    (deactivate-mark)
    (unwind-protect
        (let ((jump-tree-inhibit-kill-visualizer t)) (jump-tree-jump-prev-1 arg))
      (setq current (jump-tree-current jump-tree-global-tree))
      (switch-to-buffer-other-window jump-tree-visualizer-buffer-name)
      ;; when using lazy drawing, extend tree upwards as required
      (when jump-tree-visualizer-lazy-drawing
        (jump-tree-expand-up old current))
      ;; highlight new current node
      (let ((inhibit-read-only t)) (jump-tree-draw-node current 'current))
      ;; update diff display, if any
      (when jump-tree-visualizer-diff (jump-tree-visualizer-update-diff)))))

(defun jump-tree-visualize-jump-next (&optional arg)
  "Jump-Next changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (let ((old (jump-tree-current jump-tree-global-tree))
        current)
    ;; unhighlight old current node
    (let ((jump-tree-insert-face 'jump-tree-visualizer-active-branch-face)
          (inhibit-read-only t))
      (jump-tree-draw-node (jump-tree-current jump-tree-global-tree)))
    ;; jump-next in parent buffer
    (switch-to-buffer-other-window jump-tree-visualizer-parent-buffer)
    (deactivate-mark)
    (unwind-protect
        (let ((jump-tree-inhibit-kill-visualizer t)) (jump-tree-jump-next-1 arg))
      (setq current (jump-tree-current jump-tree-global-tree))
      (switch-to-buffer-other-window jump-tree-visualizer-buffer-name)
      ;; when using lazy drawing, extend tree downwards as required
      (when jump-tree-visualizer-lazy-drawing
        (jump-tree-expand-down old current))
      ;; highlight new current node
      (let ((inhibit-read-only t)) (jump-tree-draw-node current 'current))
      ;; update diff display, if any
      (when jump-tree-visualizer-diff (jump-tree-visualizer-update-diff)))))

(defun jump-tree-visualize-switch-branch-right (arg)
  "Switch to next branch of the jump-prev tree.
This will affect which branch to descend when *jump-nexting* changes
using `jump-tree-jump-next' or `jump-tree-visualizer-jump-next'."
  (interactive "p")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  ;; un-highlight old active branch below current node
  (goto-char (jump-tree-node-marker (jump-tree-current jump-tree-global-tree)))
  (let ((jump-tree-insert-face 'jump-tree-visualizer-default-face)
        (inhibit-read-only t))
    (jump-tree-highlight-active-branch (jump-tree-current jump-tree-global-tree)))
  ;; increment branch
  (let ((branch (jump-tree-node-branch (jump-tree-current jump-tree-global-tree))))
    (setf (jump-tree-node-branch (jump-tree-current jump-tree-global-tree))
          (cond
           ((>= (+ branch arg) (jump-tree-num-branches))
            (1- (jump-tree-num-branches)))
           ((<= (+ branch arg) 0) 0)
           (t (+ branch arg))))
    (let ((inhibit-read-only t))
      ;; highlight new active branch below current node
      (goto-char (jump-tree-node-marker (jump-tree-current jump-tree-global-tree)))
      (let ((jump-tree-insert-face 'jump-tree-visualizer-active-branch-face))
        (jump-tree-highlight-active-branch (jump-tree-current jump-tree-global-tree)))
      ;; re-highlight current node
      (jump-tree-draw-node (jump-tree-current jump-tree-global-tree) 'current))))

(defun jump-tree-visualize-switch-branch-left (arg)
  "Switch to previous branch of the jump-prev tree.
This will affect which branch to descend when *jump-nexting* changes
using `jump-tree-jump-next' or `jump-tree-visualizer-jump-next'."
  (interactive "p")
  (jump-tree-visualize-switch-branch-right (- arg)))

(defun jump-tree-visualizer-quit ()
  "Quit the jump-tree visualizer."
  (interactive)
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (jump-tree-clear-visualizer-data jump-tree-global-tree)
  ;; remove kill visualizer hook from parent buffer
  (unwind-protect
      (with-current-buffer jump-tree-visualizer-parent-buffer
        (remove-hook 'before-change-functions 'jump-tree-kill-visualizer t))
    ;; kill diff buffer, if any
    (when jump-tree-visualizer-diff (jump-tree-visualizer-hide-diff))
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
  "Set buffer to state corresponding to jump-prev tree node
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
      ;; re-draw jump-prev tree
      (let ((inhibit-read-only t)) (jump-tree-draw-tree jump-tree-global-tree))
      (when jump-tree-visualizer-diff (jump-tree-visualizer-update-diff)))))

(defun jump-tree-visualizer-mouse-set (pos)
  "Set buffer to state corresponding to jump-prev tree node
at mouse event POS."
  (interactive "@e")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (jump-tree-visualizer-set (event-start (nth 1 pos))))

(defun jump-tree-visualize-jump-prev-to-x (&optional x)
  "Jump-Prev to last branch point, register, or saved state.
If X is the symbol `branch', jump-prev to last branch point. If X is
the symbol `register', jump-prev to last register. If X is the sumbol
`saved', jump-prev to last saved state. If X is null, jump-prev to first of
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
                   (jump-tree-current jump-tree-global-tree)))
        (diff jump-tree-visualizer-diff)
        r)
    (jump-tree-visualizer-hide-diff)
    (while (and (jump-tree-node-previous current)
                (or (if jump-tree-visualizer-selection-mode
                        (progn
                          (jump-tree-visualizer-select-previous)
                          (setq current jump-tree-visualizer-selected-node))
                      (jump-tree-visualize-jump-prev)
                      (setq current (jump-tree-current jump-tree-global-tree)))
                    t)
                ;; branch point
                (not (or (and (or (null x) (eq x 'branch))
                              (> (jump-tree-num-branches) 1))
                         ;; register
                         (and (or (null x) (eq x 'register))
                              (setq r (jump-tree-node-register current))
                              (jump-tree-register-data-p
                               (setq r (registerv-data (get-register r))))
                              (eq current (jump-tree-register-data-node r)))
                         ;; saved state
                         (and (or (null x) (eq x 'saved))
                              (jump-tree-node-unmodified-p current))
                         ))))
    ;; update diff display, if any
    (when diff
      (jump-tree-visualizer-show-diff
       (when jump-tree-visualizer-selection-mode
         jump-tree-visualizer-selected-node)))))

(defun jump-tree-visualize-jump-next-to-x (&optional x)
  "Jump-Next to last branch point, register, or saved state.
If X is the symbol `branch', jump-next to last branch point. If X is
the symbol `register', jump-next to last register. If X is the sumbol
`saved', jump-next to last saved state. If X is null, jump-next to first of
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
                   (jump-tree-current jump-tree-global-tree)))
        (diff jump-tree-visualizer-diff)
        r)
    (jump-tree-visualizer-hide-diff)
    (while (and (jump-tree-node-next current)
                (or (if jump-tree-visualizer-selection-mode
                        (progn
                          (jump-tree-visualizer-select-next)
                          (setq current jump-tree-visualizer-selected-node))
                      (jump-tree-visualize-jump-next)
                      (setq current (jump-tree-current jump-tree-global-tree)))
                    t)
                ;; branch point
                (not (or (and (or (null x) (eq x 'branch))
                              (> (jump-tree-num-branches) 1))
                         ;; register
                         (and (or (null x) (eq x 'register))
                              (setq r (jump-tree-node-register current))
                              (jump-tree-register-data-p
                               (setq r (registerv-data (get-register r))))
                              (eq current (jump-tree-register-data-node r)))
                         ;; saved state
                         (and (or (null x) (eq x 'saved))
                              (jump-tree-node-unmodified-p current))
                         ))))
    ;; update diff display, if any
    (when diff
      (jump-tree-visualizer-show-diff
       (when jump-tree-visualizer-selection-mode
         jump-tree-visualizer-selected-node)))))

(defun jump-tree-visualizer-toggle-timestamps ()
  "Toggle display of time-stamps."
  (interactive)
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (setq jump-tree-visualizer-timestamps (not jump-tree-visualizer-timestamps))
  (setq jump-tree-visualizer-spacing (jump-tree-visualizer-calculate-spacing))
  ;; redraw tree
  (let ((inhibit-read-only t)) (jump-tree-draw-tree jump-tree-global-tree)))

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
       (nth (jump-tree-node-branch (jump-tree-current jump-tree-global-tree))
            (jump-tree-node-next (jump-tree-current jump-tree-global-tree)))))
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
       (jump-tree-node-previous (jump-tree-current jump-tree-global-tree))))
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
          (jump-tree-current jump-tree-global-tree))
    ;; erase diff (if any), as initially selected node is identical to current
    (when jump-tree-visualizer-diff
      (let ((buff (get-buffer jump-tree-diff-buffer-name))
            (inhibit-read-only t))
        (when buff (with-current-buffer buff (erase-buffer))))))
   (t ;; disable selection mode
    (setq cursor-type nil)
    (setq jump-tree-visualizer-selected-node nil)
    (goto-char (jump-tree-node-marker (jump-tree-current jump-tree-global-tree)))
    (when jump-tree-visualizer-diff (jump-tree-visualizer-update-diff)))
   ))

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
    ;; update diff display, if any
    (when (and jump-tree-visualizer-diff
               (not (eq node jump-tree-visualizer-selected-node)))
      (jump-tree-visualizer-update-diff node))
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
    ;; update diff display, if any
    (when (and jump-tree-visualizer-diff
               (not (eq node jump-tree-visualizer-selected-node)))
      (jump-tree-visualizer-update-diff node))
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
    (when (and jump-tree-visualizer-diff node
               (not (eq node jump-tree-visualizer-selected-node)))
      (jump-tree-visualizer-update-diff node))
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
    (when (and jump-tree-visualizer-diff node
               (not (eq node jump-tree-visualizer-selected-node)))
      (jump-tree-visualizer-update-diff node))
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
      ;; update diff display, if any
      (when (and jump-tree-visualizer-diff
                 (not (eq node jump-tree-visualizer-selected-node)))
        (jump-tree-visualizer-update-diff node))
      ;; update selected node
      (setq jump-tree-visualizer-selected-node node)
      )))

(defun jump-tree-visualizer-mouse-select (pos)
  "Select jump-prev tree node at mouse event POS."
  (interactive "@e")
  (unless (eq major-mode 'jump-tree-visualizer-mode)
    (user-error "jump-tree mode not enabled in buffer"))
  (jump-tree-visualizer-select (event-start (nth 1 pos))))


(defcustom jump-tree-global-list-max-length 100
  "Max length of jump-tree-global-list."
  :type 'integer
  :group 'jump-tree)

(defcustom jump-tree-global-list-ex-mode 'nil
  "Original vim like jump-tree-global-list or not."
  :type 'boolean
  :group 'jump-tree)

(defcustom jump-tree-global-list-hook-commands '(end-of-buffer beginning-of-buffer find-file)
  "Commands to hook."
  :type 'list
  :group 'jump-tree)

(defvar jump-tree-global-list--idx 0
  "Index of jump-tree-global-list.")

(defvar jump-tree-global-list--jumping nil
  "Jump-Tree-Global-List state.")

(defun jump-tree-global-list--do-jump (buff)
  "Do jump to target file and point from BUFF."
  (find-file (car buff))
  (goto-char (cdr buff)))

(defun jump-tree-global-list--reset-idx ()
  "Reset `jump-tree-global-list--idx'."
  (setq jump-tree-global-list--idx 0))

(defun jump-tree-global-list--last? ()
  "Check `jump-tree-global-list--idx' is last of list."
  (= jump-tree-global-list--idx (- (length jump-tree-global-list) 1)))

(defun jump-tree-global-list--first? ()
  "Check `jump-tree-global-list--idx' is first of list."
  (= jump-tree-global-list--idx 0))

(defun jump-tree-global-list--dec-idx ()
  "Descrement `jump-tree-global-list--idx'."
  (setq jump-tree-global-list--idx (- jump-tree-global-list--idx 1)))

(defun jump-tree-global-list--inc-idx ()
  "Increment `jump-tree-global-list--idx'."
  (setq jump-tree-global-list--idx (+ jump-tree-global-list--idx 1)))

(defun jump-tree-global-list--drop! (idx)
  "Drop item form list of IDX."
  (setq jump-tree-global-list (nthcdr jump-tree-global-list--idx jump-tree-global-list)))

(defun jump-tree-global-list--push (pointer)
  "Push POINTER to `jump-tree-global-list'."
  (while (> (length jump-tree-global-list) jump-tree-global-list-max-length)
    (nbutlast jump-tree-global-list 1))
  (push pointer jump-tree-global-list))

(defun jump-tree-global-list--same-position? (pointer)
  (let ((new-point (cdr pointer))
        (top-point (cdar jump-tree-global-list)))
    (cond ((not new-point) nil)
          ((not top-point) nil)
          ((eq (marker-position new-point) (marker-position top-point)) 't))))

(defun jump-tree-global-list--set ()
  "The record data structure is (file-name . pointer)."
  (interactive)
  (if (buffer-file-name)
      (let ((pointer (cons (buffer-file-name) (point-marker))))
        (unless (jump-tree-global-list--same-position? pointer)
          (when (and jump-tree-global-list-ex-mode jump-tree-global-list--jumping)
            (jump-tree-global-list--drop! jump-tree-global-list--idx)
            (setq jump-tree-global-list--jumping nil)
            (jump-tree-global-list--reset-idx))
          (unless (jump-tree-global-list--same-position? pointer)
            (jump-tree-global-list--push pointer))))))

(defun jump-tree-global-list--do-command? (command do-hook-command-list)
  (if do-hook-command-list
      (or
       (eq command (car do-hook-command-list))
       (jump-tree-global-list--do-command? command (cdr do-hook-command-list)))))

(defun jump-tree-global-list--command-hook ()
  "Pre command hook that call `jump-tree-global-list--set' when registerd command hook called."
  (cond
   ((jump-tree-global-list--do-command? this-command jump-tree-global-list-hook-commands) (jump-tree-global-list--set))
   ((and jump-tree-global-list--jumping               ; when jump and move
         (not (memq this-command '(jump-tree-global-list-previous jump-tree-global-list-next))))
    (jump-tree-global-list--set))))
(add-hook 'pre-command-hook 'jump-tree-global-list--command-hook)

(defun jump-tree-global-list-previous ()
  (if (or (not jump-tree-global-list)
          (and (not (jump-tree-global-list--first?))
               (jump-tree-global-list--last?)))
      (message "No further undo point.")
    (if jump-tree-global-list-ex-mode
        (unless jump-tree-global-list--jumping
          (jump-tree-global-list--set)
          (setq jump-tree-global-list--jumping 't)))
    (jump-tree-global-list--inc-idx)
    (let ((buff (nth jump-tree-global-list--idx jump-tree-global-list)))
      (jump-tree-global-list--do-jump buff))))

(defun jump-tree-global-list-next ()
  "Jump forward."
  (if (or (not jump-tree-global-list)
          (jump-tree-global-list--first?))
      (message "No further redo point.")
    (if jump-tree-global-list-ex-mode
        (unless jump-tree-global-list--jumping
          (jump-tree-global-list--set)
          (setq jump-tree-global-list--jumping 't)))
    (jump-tree-global-list--dec-idx)
    (let ((buff (nth jump-tree-global-list--idx jump-tree-global-list)))
      (jump-tree-global-list--do-jump buff))))

(provide 'jump-tree)
;;; jump-tree.el ends here
