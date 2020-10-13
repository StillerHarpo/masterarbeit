(setq-default indent-tabs-mode nil)
(require 'org-ref)
(setq org-latex-listings 'minted)
(setq org-latex-minted-options '(("linenos") ("fontsize" "\\tiny")))
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
