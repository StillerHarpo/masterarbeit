(setq-default indent-tabs-mode nil)
(require 'org-ref)
(setq org-latex-listings 'minted)
(setq org-latex-minted-options '(("fontsize" "\\scriptsize")))
(setq org-latex-prefer-user-labels 't)
(eval-after-load "ox-latex"
  '(add-to-list 'org-latex-classes
                '("scrbook" "\\documentclass{scrbook}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(defun my-latex-filter-verbatim (text backend info)
  "make verbatim enviroment to lstlisting"
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "{verbatim}" "{lstlisting}" text)))

(add-to-list 'org-export-filter-example-block-functions
             'my-latex-filter-verbatim)
