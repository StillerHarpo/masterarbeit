#! /usr/bin/env nix-shell
#! nix-shell -i bash -p "emacsWithPackages (epkgs: [ epkgs.melpaPackages.org-ref ])" python37Packages.pygments texlive.combined.scheme-full which --pure
emacs \
   --batch \
   --eval '(setq-default indent-tabs-mode nil)' \
   --eval '(require '\''org-ref)' \
   --eval '(setq org-latex-listings '\''minted)' \
   --eval '(setq org-latex-prefer-user-labels '\''t)' \
   masterarbeit.org \
   -f org-latex-export-to-latex
latexmk -shell-escape -bibtex -f -pdfxe masterarbeit.tex
