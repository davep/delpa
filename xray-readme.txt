;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; Sometimes you need to see the internal structures to understand what is
;; going on.  This package provides a way to display internal Emacs object
;; structures in a temporary buffer.
;;
;; For good performance, be sure to byte-compile xray.el, e.g.
;;
;;    M-x byte-compile-file <give the path to xray.el when prompted>
;;
;; This will generate xray.elc, which will be loaded instead of xray.el.
;;
;; xray was tested with GNU Emacs 21.3.50.1.
;;
;; I don't know if still is compatible with XEmacs.
;;
;;
;; Usage
;; -----
;;
;; To use xray, insert in your ~/.emacs:
;;
;;        (require 'xray)
;;
;; And type, for example:
;;
;;    M-x xray-symbol RET describe-function RET
;; Or:
;;    M-: (xray-symbol 'describe-function) RET
;; Or:
;;    M-x global-set-key RET C-c x xray-symbol RET C-c x describe-function RET
;;
;; The following buffer (*Symbol X-Ray*) is shown:
;;
;; ------------------------------------------------- Begin *Symbol X-Ray*
;;
;; SYMBOL
;;
;; describe-function
;;    apropos       : *Documentation*   *Apropos*   *Info*
;;    key bindings  : C-h f    C-h d    menu-bar help-menu describe desc\
;; ribe-function
;;    file          : help
;;    function cell : *Interactive-Compiled-Lisp-Function*
;;    value cell    : void
;;    property list cell:
;;      (event-symbol-element-mask  (describe-function 0)
;;       event-symbol-elements  (describe-function)
;;       modifier-cache  ((0 . describe-function)))
;; --------------------------------------------------- End *Symbol X-Ray*
;;
;; The entries on apropos, key bindings, file and function cell are "links" to
;; other help buffers.  The key bindings (C-h f, C-h d, etc.) points to a key
;; description (if you click on C-h f, it's the same as typing C-h k C-h f),
;; the file (help) points to the position on file help.el where
;; describe-function is defined, and the function cell points to a function
;; description (if you click, it's the same as typing C-h f describe-function).
;; So, if you click on any "link", you get more related information.
;;
;; As in a help buffer, when you follow the "links", it'll appear at end of
;; buffer a `[back]' button.  You can go back by clicking with mouse-2 the
;; `[back]' button or by typing C-c C-b on xray (or help) buffer.
;;
;;
;; Objects
;; -------
;;
;; The following objects may be shown:
;;
;;    + Mouse (`xray-on-click'):
;;	 Give help on an object clicked with the mouse.
;;
;;    + Mouse on Mode Line (`xray-on-mode-line-click'):
;;	 Give help on the mode line.
;;
;;    + Click/Key (`xray-click/key'):
;;	 Give help on a key/menu sequence or object clicked with the mouse.
;;
;;	 The object can be any part of an Emacs window or a name appearing in a
;;	 buffer.  You can do any of the following:
;;
;;	    type a key sequence (e.g. `C-M-s')
;;	    choose a menu item (e.g. [menu-bar files open-file])
;;	    click on a scroll bar
;;	    click on the mode line
;;	    click in the minibuffer
;;	    click on a name in a buffer: `xray-symbol' is called
;;	    click anywhere else in a buffer: `xray-buffer' is called
;;
;;    + Symbol (`xray-symbol'):
;;	 Displays the symbol name cell, the symbol function cell, the symbol
;;	 value cell, the symbol property list cell and the key bindings
;;	 associated with symbol (if any), from which file it was loaded and
;;	 some apropos information.
;;
;;    + Position (`xray-position'):
;;	 Displays the frame, the window, the buffer, the word (if any) around
;;	 position (also some apropos information), the character width, the
;;	 character at position, the charset, the text property list, the
;;	 default text property list and the overlay list.
;;
;;    + Buffer (`xray-buffer'):
;;	 Displays the frame, the window, the base buffer (if it's an indirect
;;	 buffer), buffer name, buffer size, minimum point, point, maximum
;;	 point, the mark, the mark active flag, file name visited (if any),
;;	 file modification time, the modified flag, the read only flag,
;;	 multibyte flag, inhibit read flag, display table, active modes, window
;;	 list, buffer list, hooks related to buffers, mark ring, overlay list
;;	 and local variables.
;;
;;    + Window (`xray-window'):
;;	 Displays the associated frame, the associated buffer, the window, the
;;	 height, the width, the edges, the buffer position, the window start,
;;	 the window end, the liveness flag, the dedicated flag, the minibuffer
;;	 flag, the horizontal scrolling amount, display table, some window
;;	 related variables, the hooks, the window least recently selected, the
;;	 largest window area and the window list.
;;
;;    + Frame (`xray-frame'):
;;	 Displays the frame, frame height, frame width, pixel frame height,
;;	 pixel frame width, pixel char height, pixel char width, liveness flag,
;;	 visibility flag, the first window on frame, the selected window, the
;;	 root window, some variables related to frame, the frame parameters,
;;	 the hooks, the frame list, the visible frame list and display list.
;;
;;    + Marker (`xray-marker'):
;;	 Displays the associated buffer, the position, the insertion type, the
;;	 mark, the beginning of region, the end of region, some variable
;;	 related to marker, hooks and the mark ring.
;;
;;    + Overlay (`xray-overlay'):
;;	 Displays the associated buffer, the start position, the end position,
;;	 the overlay list and the property list.
;;
;;    + Screen (`xray-screen'):
;;	 Displays the screen capabilities, some variables and hooks related to
;;	 screen, and the display list.
;;
;;    + Faces (`xray-faces'):
;;	 Displays all defined faces.
;;
;;    + Hooks (`xray-hooks'):
;;	 Displays all standard hooks and other defined hooks.
;;
;;    + Features (`xray-features'):
;;	 Displays all features loaded.
;;
;; As a suggestion for key bindings:
;;
;; (global-set-key      [f1]                     'xray-click/key)
;; (define-key help-map [?\C-m]                  'xray-click/key) ; RET
;; (define-key help-map [down-mouse-1]           'xray-on-click)
;; (define-key help-map [mode-line down-mouse-1] 'xray-on-mode-line-click)
;;
;; Maybe the following key bindings are useful:
;;
;; (define-key help-map "o"       'edit-options) ; in `options.el'
;; (define-key help-map "u"       'manual-entry) ; in `man.el'
;; (define-key help-map "\C-l"    'locate-library)
;; (define-key help-map "\C-a"    'apropos)
;; (define-key help-map "\M-a"    'apropos-documentation)
;; (define-key help-map "\M-\C-a" 'tags-apropos)
;;
;;
;; Interfaces
;; ----------
;;
;; There are three function set for interfacing:
;;
;; KIND       MAIN INTERFACE	 HELP INTERFACE		 EHELP INTERFACE
;;	      (`xray.el')	 (`help.el')		 (`ehelp.el')
;;
;; Symbol    `xray-symbol'	`xray-help-symbol'	`xray-ehelp-symbol'
;; Position  `xray-position'	`xray-help-position'	`xray-ehelp-position'
;; Buffer    `xray-buffer'	`xray-help-buffer'	`xray-ehelp-buffer'
;; Window    `xray-window'	`xray-help-window'	`xray-ehelp-window'
;; Frame     `xray-frame'	`xray-help-frame'	`xray-ehelp-frame'
;; Marker    `xray-marker'	`xray-help-marker'	`xray-ehelp-marker'
;; Overlay   `xray-overlay'	`xray-help-overlay'	`xray-ehelp-overlay'
;; Screen    `xray-screen'	`xray-help-screen'	`xray-ehelp-screen'
;; Faces     `xray-faces'	`xray-help-faces'	`xray-ehelp-faces'
;; Hooks     `xray-hooks'	`xray-help-hooks'	`xray-ehelp-hooks'
;; Features  `xray-features'	`xray-help-features'	`xray-ehelp-features'
;;
;; The MAIN INTERFACE uses `xray-electric-p' (see Options) to decide if it
;; invokes HELP INTERFACE (when it's nil) or EHELP INTERFACE (when it's
;; non-nil).
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of xray options, please, see the
;; options declaration in the code for a long documentation.
;;
;; `xray-property-alist'		Specify association between property
;;					symbol and a display function.
;;
;; `xray-property-recursive-list'	Specify property list which can be
;;					displayed recursively.
;;
;; `xray-maximum-depth'			Specify maximum display recursive
;;					depth.
;;
;; `xray-value-threshold'		Specify maximum value data length to
;;					display.
;;
;; `xray-buffer-name'			Specify x-ray buffer name.
;;
;; `xray-apropos-do-all'		Non-nil means the apropos commands
;;					should do more.
;;
;; `xray-info-level'			Specify level of information for
;;					presentation.
;;
;; `xray-apropos-format'		Specify regexp format to be used by
;;					`apropos'.
;;
;; `xray-electric-p'			Non-nil means that ehelp interface will
;;					be used instead of help interface.
;;
;; To set the above options you may:
;;
;; a) insert code in your ~/.emacs, like:
;;
;;	 (setq xray-property-alist '((some-prop . display-some-prop)))
;;
;;    This method preserves your default settings when you enter a new Emacs
;;    session.
;;
;; b) or use `set-variable' in your Emacs session, like:
;;
;;	 M-x set-variable RET xray-property-alist RET
;;	 '((some-prop . display-some-prop)) RET
;;
;;    This method preserves your settings only during the current Emacs
;;    session.
;;
;; c) or use customization, for example:
;;	 click on menu-bar *Help* option,
;;	 then click on *Customize*,
;;	 then click on *Browse Customization Groups*,
;;	 expand *Development* group,
;;	 expand *Internal* group,
;;	 expand *Xray* group
;;	 and then customize xray options.
;;    This way, you may choose if the settings are kept or not when you leave
;;    out the current Emacs session.
;;
;; d) or see the option value:
;;
;;	 C-h v xray-property-alist RET
;;
;;    and click the *customize* hypertext button.
;;    This way, you may choose if the settings are kept or not when you leave
;;    out the current Emacs session.
;;
;; e) or invoke:
;;
;;	 M-x xray-customize RET
;;
;;    and then customize xray options.
;;    This way, you may choose if the settings are kept or not when you leave
;;    out the current Emacs session.
;;
;;
;; Acknowledgments
;; ---------------
;;
;; Thanks to Juanma Barranquero <lektu@uol.com.br> for ehelp.el suggestion and
;; for `line-number-display-limit' meaning in Emacs 21.
;;
;; Thanks to Drew Adams <drew.adams@openwave.com> for key bindings suggestions
;; and for sending help+.el package which inspired `xray-click/key',
;; `xray-display-click/key', `xray-on-click' and `xray-on-mode-line-click'
;; functions.
;;
;; Thanks to Arnaldo Mandel <am@ime.usp.br> for documentation corrections.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

