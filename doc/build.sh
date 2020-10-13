#! /usr/bin/env nix-shell
#! nix-shell -i bash -p "emacsWithPackages (epkgs: [ epkgs.melpaPackages.org-ref ])" python37Packages.pygments texlive.combined.scheme-full which --pure
emacs \
   --batch \
   --load "init.el" \
   masterarbeit.org \
   -f org-latex-export-to-latex
latexmk -shell-escape -bibtex -f -pdfxe masterarbeit.tex
