;;; ox-dnd.el --- Write D&D homebrew sheets using org-mode

;; Copyright (C) 2019 Alex Smith <xeals@pm.me>

;; Author: Alex Smith <xeals@pm.me>
;; URL: https://github.com/xeals/emacs-org-dnd

(require 'ox-latex)

(defgroup org-dnd nil
  "Group for customising the org-mode D&D LaTeX export backend.")

(defcustom org-dnd-use-package nil
  "Use the `book' LaTeX class when exporting a D&D org-mode file
  instead of the `dndbook' class."
  :group 'org-dnd)

(unless (assoc "dnd" org-latex-classes)
  (add-to-list
   'org-latex-classes
   `("dnd"
     ,(concat
       (format
        "\\documentclass[10pt,twoside,twocolumn,openany[CO]]{%s}
[NO-DEFAULT-PACKAGES]
\\usepackage[AUTO]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage{hyperref}"
        (if org-dnd-use-package "book" "dndbook"))
       (when org-dnd-use-package "\\n\\usepackage{dnd}"))
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(defun ordinal (n)
  (let ((str (if (numberp n) (number-to-string n) n)))
    (concat str
            (pcase (last str)
              ("1" "st")
              ("2" "nd")
              ("3" "rd")
              (_ "th")))))

(defun org-dnd--spell-level (level school)
  (org-trim
   (format "%s%s"
           (if level
               (concat (ordinal level) "-level ")
             "")
           (or (downcase school) ""))))

(defun org-dnd-spell (spell contents info)
  "Transcode a SPELL element from Org to D&D LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist holding
contextual information."
  (let ((name (org-element-property :name spell))
        (level (org-export-read-attribute :attr_spell spell :level))
        (school (org-export-read-attribute :attr_spell spell :school))
        (ct (org-export-read-attribute :attr_spell spell :cast))
        (range (org-export-read-attribute :attr_spell spell :range))
        (mats (org-export-read-attribute :attr_spell spell :comp))
        (dur (org-export-read-attribute :attr_spell spell :duration)))
    (concat "\\begin{spell}"
            (format "{%s}" (or name ""))
            (format "{%s}" (org-dnd--spell-level level school))
            (format "{%s}" (or ct ""))
            (format "{%s}" (or range ""))
            (format "{%s}" (or mats ""))
            (format "{%s}\n" (or dur ""))
            contents
            "\\end{spell}")))

(defun org-dnd--subtitle-block (subtitle contents info)
  "Transcode a SUBTITLE-BLOCK element to D&D LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist holding
contextual information."
  (let ((content (split-string contents "\n" t nil)))
    (format "\\subtitlesection{%s}{%s}"
            (car content)
            (car (cdr content)))))

(defun org-dnd--extract-actions (content)
  (org-trim
   (replace-regexp-in-string
    "\n+"
    "\n"
    (replace-regexp-in-string
     "|"
     "\n"
     (replace-regexp-in-string
      "\\\\\\(begin\\|end\\){description}"
      ""
      (replace-regexp-in-string
       "\\\\item\\[{\\([^}]*\\)}] \\(.+?\\)|\\\\"
       "\\\\begin{monsteraction}[\\1]|\\2|\\\\end{monsteraction}|\\\\"
       (replace-regexp-in-string
        "\\\\item\\[{\\([^}]*\\)}] \\(.+?\\)|\\\\"
        "\\\\begin{monsteraction}[\\1]|\\2|\\\\end{monsteraction}|\\\\"
        (replace-regexp-in-string
         "\\\\item \\([^|]*\\)"
         "\\\\monstersection{\\1}"
         (replace-regexp-in-string "\n" "|" content)))))))))

(defun org-dnd--add-legendary-action-text (name content)
  (let ((nm (downcase name)))
    (replace-regexp-in-string
     "{Legendary Actions}"
     (format "{Legendary Actions}\nThe %s can take 3 legendary actions, choosing from the options below. Only one legendary action option can be used at a time and only at the end of another creature's turn. The %s regains spent legendary actions at the start of its turn.\n" nm nm)
     content)))

(defun org-dnd-monsterbox (monster contents info)
  "Transcode a monster box from Org to a D&D LaTeX monsterbox.
CONTENTS holds the contents of the table.  INFO is a plist holding
contextual information."
  (let ((name (org-element-property :name monster)))
    (concat
     "\\begin{monsterbox}"
     (if name (format "{%s}" name) "")
     "\n"
     ;; Race and info
     (let ((size (org-export-read-attribute :attr_monster_info monster :size))
           (race (org-export-read-attribute :attr_monster_info monster :race))
           (subrace (org-export-read-attribute :attr_monster_info monster :subrace))
           (alignment (org-export-read-attribute :attr_monster_info monster :alignment)))
       (when (and size race alignment)
         (concat
          "\\begin{hangingpar}\n\\textit{"
          (format "%s %s" (capitalize size) race)
          (when subrace (format " (%s)" subrace))
          (format ", %s" alignment)
          "}\n\\end{hangingpar}\n")))
     "\\dndline%\n"
     ;; Basics
     (let ((ac (org-export-read-attribute :attr_monster_basics monster :ac))
           (hp (org-export-read-attribute :attr_monster_basics monster :hp))
           (speed (org-export-read-attribute :attr_monster_basics monster :speed))
           ;; Special speed attributes
           (burrow (org-export-read-attribute :attr_monster_basics monster :burrow))
           (climb (org-export-read-attribute :attr_monster_basics monster :climb))
           (fly (org-export-read-attribute :attr_monster_basics monster :fly))
           (hover (org-export-read-attribute :attr_monster_basics monster :hover))
           (swim (org-export-read-attribute :attr_monster_basics monster :swim)))
       (concat "\\basics[%\n"
               (format "armorclass = %s,\n" (or ac 0))
               (format "hitpoints = \\dice{%s},\n" (or hp 0))
               (format "speed = {%s ft." (or speed 0))
               (when burrow (format ", burrow %s ft." burrow))
               (when climb (format ", climb %s ft." climb))
               (when fly (format ", fly %s ft." fly))
               (when hover (format ", fly %s ft. (hover)" hover))
               (when swim (format ", swim %s ft." swim))
               "},\n]\n"))
     "\\dndline%\n"
     ;; Stats
     (let ((con (org-export-read-attribute :attr_monster_stats monster :con))
           (str (org-export-read-attribute :attr_monster_stats monster :str))
           (dex (org-export-read-attribute :attr_monster_stats monster :dex))
           (int (org-export-read-attribute :attr_monster_stats monster :int))
           (wis (org-export-read-attribute :attr_monster_stats monster :wis))
           (cha (org-export-read-attribute :attr_monster_stats monster :cha)))
       (concat "\\stats[%\n"
               (format "CON = \\stat{%s},\n" (or con 10))
               (format "STR = \\stat{%s},\n" (or str 10))
               (format "DEX = \\stat{%s},\n" (or dex 10))
               (format "INT = \\stat{%s},\n" (or int 10))
               (format "WIS = \\stat{%s},\n" (or wis 10))
               (format "CHA = \\stat{%s},\n" (or cha 10))
               "]\n"))
     "\\dndline%\n"
     ;; Details
     (let ((skills (org-export-read-attribute :attr_monster_details monster :skills))
           (saves (org-export-read-attribute :attr_monster_details monster :saves))
           (imm (org-export-read-attribute :attr_monster_details monster :imm))
           (res (org-export-read-attribute :attr_monster_details monster :res))
           (vul (org-export-read-attribute :attr_monster_details monster :vul))
           (senses (org-export-read-attribute :attr_monster_details monster :senses))
           (langs (org-export-read-attribute :attr_monster_details monster :langs))
           (cr (org-export-read-attribute :attr_monster_details monster :cr)))
       (concat "\\details[%\n"
               (when skills (format "skills = {%s},\n" skills))
               (when saves (format "savingthrows = {%s},\n" saves))
               (when imm (format "conditionimmunities = {%s},\n" imm))
               (when res (format "damageresistances = {%s},\n" res))
               (when vul (format "damagevulnerabilities = {%s},\n" vul))
               (when senses (format "senses = {%s},\n" senses))
               (when langs (format "languages = {%s},\n" langs))
               (format "challenge = {%s},\n" (or cr 0))
               "]\n"))
     "\\dndline\n"
     ;; Abilities and actions
     (org-dnd--add-legendary-action-text
      name
      (org-dnd--extract-actions contents))
     "\n\\end{monsterbox}")))

(defun org-dnd-headline (headline contents info)
  "Transcode a HEADLINE element from Org to LaTeX.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let ((text (org-latex-headline headline contents info))
        (tags (org-element-property :tags headline)))
    (if (member "map" tags)
        (progn
          (org-element-put-property headline :tags (remove "map" tags))
          (replace-regexp-in-string
           "subsection{"
           "area{"
           (org-latex-headline headline contents info)))
      (org-latex-headline headline contents info))))

;; HACK There has to be an easier way to add the package options as a derived
;; LaTeX backend.
(defun org-dnd-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (defun bool (b) (if b "true" "false"))
  (let* ((just (plist-get info :justified))
         (layout (plist-get info :layout))
         (bg (plist-get info :with-bg))
         (multitoc (plist-get info :with-multitoc))
         (class-options
          (concat
           (when just "justified,")
           (when bg (format "bg=%s," bg))
           (when (and org-dnd-use-package layout)
             (format "layout=%s," (bool org-dnd-use-package)))
           (unless multitoc "nomultitoc"))))
    (let ((body (org-latex-template contents info)))
      (replace-regexp-in-string
       "\\[CO\\]"
       (concat "," (replace-regexp-in-string
                    ",$" ""
                    class-options))
       body t))))

(defun org-dnd-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to D&D LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist holding
contextual information."
  (let ((type (downcase (org-element-property :type special-block)))
        (title (or (org-element-property :name special-block) "")))
    (pcase type
      ("commentbox"
       (concat (format "\\begin{%s}{%s}" type title)
               (org-latex--caption/label-string special-block info)
               contents
               (format "\\end{%s}" type)))
      ("paperbox"
       (concat (format "\\begin{%s}[float=!t]{%s}" type title)
               (org-latex--caption/label-string special-block info)
               contents
               (format "\\end{%s}\n" type)))
      ("quotebox"
       (concat (format "\\begin{%s}\n" type)
               contents
               (format "\\end{%s}" type)))
      ("spell" (org-dnd-spell special-block contents info))
      ("subtitle" (org-dnd--subtitle-block special-block contents info))
      ("monster" (org-dnd-monsterbox special-block contents info))
      (_ (org-latex-special-block special-block contents info)))))

(defun org-dnd-table (table contents info)
  "Transcode a table from Org to a D&D LaTeX table.
CONTENTS holds the contents of the table.  INFO is a plist holding
contextual information."
  (let ((header (car (org-element-property :header table)))
        (align (org-export-read-attribute :attr_dnd table :align))
        (color (org-export-read-attribute :attr_dnd table :color))
        (separate (org-export-read-attribute :attr_dnd table :separate)))
    (format
     "%s%s"
     (if header (format "\\header{%s}\n" header) "")
     (replace-regexp-in-string
      "begin{tabular.*"
      (format "begin{dndtable}%s%s"
              (if align (format "[%s]" align) "")
              (if color (format "[%s]" color) ""))
      (replace-regexp-in-string
       "end{tabular}"
       "end{dndtable}"
       (replace-regexp-in-string
        "{table}"
        "{table*}"
        (replace-regexp-in-string
         "\\\\\\(begin\\|end\\){center}\n?"
         ""
         (replace-regexp-in-string
          "\\\\centering"
          ""
          (replace-regexp-in-string
           "\\\\hline"
           (if (not separate) ""
             (format "\\\\end{dndtable}\n\\\\begin{dndtable}%s%s"
                     (if align (format "[%s]" align) "")
                     (if color (format "[%s]" color) "")))
           (org-latex-table table contents info))))))))))

(org-export-define-derived-backend 'dnd 'latex
  :menu-entry
  '(?l 1
       ((?d "As PDF file (D&D)" org-dnd-export-to-pdf)
        (?D "As PDF file and open (D&D)"
            (lambda (a s v b)
              (if a (org-dnd-export-to-pdf t s v b)
                (org-open-file (org-dnd-export-to-pdf nil s v b)))))))
  :options-alist
  '((:justified nil "justified" t t)
    (:layout nil "layout" t nil)
    (:with-bg nil "bg" "full" t)
    (:with-multitoc nil "mtoc" nil t)
    (:with-title nil "title" nil t)
    (:headline-levels nil "H" 5 t)
    (:latex-caption-above nil nil nil))
  :translate-alist '((headline . org-dnd-headline)
                     (template . org-dnd-template)
                     (table . org-dnd-table)
                     (special-block . org-dnd-special-block)))

;;;###autoload
(defun org-dnd-export-as-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a D&D buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org D&D Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil.

Sourced from ox-beamer."
  (interactive)
  (org-export-to-buffer 'dnd "*Org D&D Export*"
    async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

;;;###autoload
(defun org-dnd-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a D&D LaTeX file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name.

Sourced from ox-beamer."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'dnd file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-dnd-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Beamer presentation (PDF).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name.

Sourced from ox-beamer."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'dnd file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

(provide 'ox-dnd)

;;; ox-dnd.el ends here
