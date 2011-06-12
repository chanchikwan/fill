;; Sentence-fill hack
;; ==================
;;
;; The macros here are based on emacs/23.3/lisp/textmodes/fill.el.gz.
;; To use them without modifying emacs, you can simply execute `cat
;; hack.el >> ~/.emacs` if you have downloaded this file (say, by
;; git).  Otherwise, you can use
;;
;; curl http://fermi.mycloudnas.com/cgit.cgi/fill/plain/hack.el >> ~/.emacs

(setq tex-mode-hook   '(lambda () (setq newline-after-sentence t)))
(setq latex-mode-hook '(lambda () (setq newline-after-sentence t)))

(defcustom newline-after-sentence nil
  "Non-nil means put a new line after each sentence."
  :type 'boolean
  :group 'fill)

(defun fill-one-line (from to justify)
  (goto-char from)
  (let (linebeg)
    (while (< (point) to)
      (setq linebeg (point))
      (move-to-column (current-fill-column))
      (if (when (< (point) to)
	    ;; Find the position where we'll break the line.
	    (forward-char 1) ;Use an immediately following space, if any.
	    (fill-move-to-break-point linebeg)
	    ;; Check again to see if we got to the end of
	    ;; the paragraph.
	    (skip-chars-forward " \t")
	    (< (point) to))
	  ;; Found a place to cut.
	  (progn
	    (fill-newline)
	    (when justify
	      ;; Justify the line just ended, if desired.
	      (save-excursion
		(forward-line -1)
		(justify-current-line justify nil t))))
	
	(goto-char to)
	;; Justify this last line, if desired.
	(if justify (justify-current-line justify t t))))))

(defun fill-region-as-paragraph (from to &optional justify
				      nosqueeze squeeze-after)
  "Fill the region as one paragraph.
It removes any paragraph breaks in the region and extra newlines at the end,
indents and fills lines between the margins given by the
`current-left-margin' and `current-fill-column' functions.
\(In most cases, the variable `fill-column' controls the width.)
It leaves point at the beginning of the line following the paragraph.

Normally performs justification according to the `current-justification'
function, but with a prefix arg, does full justification instead.

From a program, optional third arg JUSTIFY can specify any type of
justification.  Fourth arg NOSQUEEZE non-nil means not to make spaces
between words canonical before filling.  Fifth arg SQUEEZE-AFTER, if non-nil,
means don't canonicalize spaces before that position.

Return the `fill-prefix' used for filling.

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (region-beginning) (region-end)
		       (if current-prefix-arg 'full))))
  (unless (memq justify '(t nil none full center left right))
    (setq justify 'full))

  ;; Make sure "to" is the endpoint.
  (goto-char (min from to))
  (setq to   (max from to))
  ;; Ignore blank lines at beginning of region.
  (skip-chars-forward " \t\n")

  (let ((from-plus-indent (point))
	(oneleft nil))

    (beginning-of-line)
    ;; We used to round up to whole line, but that prevents us from
    ;; correctly handling filling of mixed code-and-comment where we do want
    ;; to fill the comment but not the code.  So only use (point) if it's
    ;; further than `from', which means that `from' is followed by some
    ;; number of empty lines.
    (setq from (max (point) from))

    ;; Delete all but one soft newline at end of region.
    ;; And leave TO before that one.
    (goto-char to)
    (while (and (> (point) from) (eq ?\n (char-after (1- (point)))))
      (if (and oneleft
	       (not (and use-hard-newlines
			 (get-text-property (1- (point)) 'hard))))
	  (delete-backward-char 1)
	(backward-char 1)
	(setq oneleft t)))
    (setq to (copy-marker (point) t))
    ;; ;; If there was no newline, and there is text in the paragraph, then
    ;; ;; create a newline.
    ;; (if (and (not oneleft) (> to from-plus-indent))
    ;; 	(newline))
    (goto-char from-plus-indent))

  (if (not (> to (point)))
      nil ;; There is no paragraph, only whitespace: exit now.

    (or justify (setq justify (current-justification)))

    ;; Don't let Adaptive Fill mode alter the fill prefix permanently.
    (let ((fill-prefix fill-prefix))
      ;; Figure out how this paragraph is indented, if desired.
      (when (and adaptive-fill-mode
		 (or (null fill-prefix) (string= fill-prefix "")))
	(setq fill-prefix (fill-context-prefix from to))
	;; Ignore a white-space only fill-prefix
	;; if we indent-according-to-mode.
	(when (and fill-prefix fill-indent-according-to-mode
		   (string-match "\\`[ \t]*\\'" fill-prefix))
	  (setq fill-prefix nil)))

      (goto-char from)
      (beginning-of-line)

      (if (not justify)	  ; filling disabled: just check indentation
	  (progn
	    (goto-char from)
	    (while (< (point) to)
	      (if (and (not (eolp))
		       (< (current-indentation) (current-left-margin)))
		  (fill-indent-to-left-margin))
	      (forward-line 1)))

	(if use-hard-newlines
	    (remove-list-of-text-properties from to '(hard)))
	;; Make sure first line is indented (at least) to left margin...
	(if (or (memq justify '(right center))
		(< (current-indentation) (current-left-margin)))
	    (fill-indent-to-left-margin))
	;; Delete the fill-prefix from every line.
	(fill-delete-prefix from to fill-prefix)
	(setq from (point))

	;; FROM, and point, are now before the text to fill,
	;; but after any fill prefix on the first line.
	(fill-delete-newlines from to justify nosqueeze squeeze-after)
	
	(if (not newline-after-sentence)
	    (fill-one-line from to justify) ;; original innner loop
	  
	  ;; Insert a line break after each sentence
	  (goto-char from)
	  (while (< (point) to)
	    (forward-sentence)
	    (if (< (point) to) (fill-newline)))
	  ;; This is the actual filling loop.
	  (goto-char from)
	  (let (sentbeg sentend)
	    (while (< (point) to)
	      (setq sentbeg (point))
	      (end-of-line)
	      (setq sentend (point))
	      (fill-one-line sentbeg sentend justify) ;; original innner loop
	      (forward-line)))))

      ;; Leave point after final newline.
      (goto-char to)
      (unless (eobp) (forward-char 1))
      ;; Return the fill-prefix we used
      fill-prefix)))
