#! /usr/bin/env nix-shell
#! nix-shell -i bash -p "emacsWithPackages (epkgs: [ epkgs.melpaPackages.org-ref ])" python37Packages.pygments texlive.combined.scheme-full which --pure
emacs \
    --batch \
    --load "init.el" \
    slides.org \
    -f org-beamer-export-to-latex \
    && latexmk -shell-escape -bibtex -f -pdfxe slides.tex
