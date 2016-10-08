/*

This is a nix expression to build Emacs and some Emacs packages I like
from source on any distro where nix is installed. This will install
all the dependencies from the nixpkgs repo and build the binary files
without interfering with the host distro.

http://nixos.org/nix/

To quickly install nix, you can run the following command:

$ curl -L http://git.io/nix-install.sh | bash

To initialize it:

$ source ~/.nix-profile/etc/profile.d/nix.sh

To build the project, type the following from the current directory:

$ nix-build

To run the newly compiled executable:

$ ./result/bin/emacs
*/
{ pkgs ? import <nixpkgs> {} }:

let
  baseEmacs = pkgs.emacs25;
  myEmacs = pkgs.lib.overrideDerivation (baseEmacs.override {
    # Use gtk3 instead of the default gtk2
    withGTK3 = false;
    withGTK2 = true;
    # Make sure imagemagick is a dependency because I regularly
    # look at pictures from Emacs
    imagemagick = pkgs.imagemagickBig;
  }) (attrs: {
    # I don't want emacs.desktop file because I only use
    # emacsclient.
    postInstall = attrs.postInstall + ''
      rm $out/share/applications/emacs.desktop
    '';
    CFLAGS = "-Og -g3";
  });
  myEmacsPackagesNg = pkgs.emacsPackagesNgGen myEmacs;
in
with myEmacsPackagesNg;
emacsWithPackages ((with melpaStablePackages; [
    ace-link # ; type o in help-mode to go to a link
    ace-window # ; manage windows with ace-like behavior
    ag # ; search using the 'ag' command (better grep)
    aggressive-indent # ; indent code automatically while typing
    all-the-icons # ; library with many icons
    amd-mode # ; handles javascript AMD module requirements
    anzu # ; more interactive query-replace
    avy # ; move fast in buffer with <C-,>
    bind-key # ; to simplify definition of shortcuts
    camcorder # ; record emacs sessions M-x camcorder-record
    company-tern #; tern backend for company mode
    counsel # ; various completions using ivy
    csharp-mode # ; C# major mode
    dash # ; list library
    define-word # ; get definition of a common word
    diff-hl # ; shows git status in buffer's fringe
    diminish # ; Shorter mode names in the modeline
    dired-imenu # ; integrates imenu in dired
    dired-toggle-sudo # ; <C-x s> to toggle sudo state of buffer
    discover # ; popus for some features
    drag-stuff # ; use <M-arrow> to move things around
    editorconfig # ; handle .editorconfig files automatically
    ethan-wspace # ; Correctly takes care of trailing spaces
    f # ; file manipulation library
    feature-mode # ; major mode for editing feature files
    fill-column-indicator # ; <M-x `fci-mode'> to show the right margin
    flycheck # ; flycheck to check files on the fly
    flycheck-cask # ; use Cask when present for dependencies
    flycheck-package # ; checks elisp package metadata
    git-auto-commit-mode # ; commit after each file save
    git-timemachine # ; view history of a file with M-x git-timemachine
    grunt # ; glue for grunt files (Javascript)
    guide-key # ; help you learn emacs
    gulp-task-runner # ; run gulp from Emacs (Javascript)
    haskell-mode # ; to edit Haskell files
    helm # ; selection/completion interface for everything
    helm-ag # ; use ag from helm
    helm-descbinds # ; integrate helm and describe-bindings <any-prefix C-h>
    helm-projectile # ; integrate projectile and helm <C-. p h>
    hydra # ; easy definition of repeatable shortcuts
    jade # ; JS development environment
    js2-mode # ; Improved Javascript editing mode
    js2-refactor # ; A Javascript refactoring library
    json-mode # ; Major mode to edit JSON files
    magit # ; Integrate git <C-x g>
    magithub # ; Integrate magit and github (needs http://hub.github.com/)
    markdown-mode # ; Major mode for markdown format
    markdown-preview-mode # ; preview HTML while writing Markdown
    multiple-cursors # ; Control multiple cursors with <C-S-c C-S-c>
    nix-mode # ; major mode to edit nix expressions
    org-vcard # ; used by vdirel
    orgtbl-show-header # ; <M-x orgtbl-show-header> to show the header of the current column in the minibuffer
    ox-twbs # ; use twitter bootstrap to export org files to HTML
    paredit # ; edit lisp AST instead of characters
    paren-face # ; hide parenthesis in elisp code
    password-store # ; get passwords from the 'pass' command
    powerline # ; required by zerodark theme
    pdf-tools # to view PDF files inside Emacs
    pillar # ; Major mode for pier/pillar-formatted text files
    pos-tip # ; make tool-tips appear nicely
    projectile # ; many functions on projects
    refine #; edit list interactively
    runner # ; Associate external applications to file extensions
    s # ; string library
    skeletor # ; facilitates the creation of new project
    slack  # ; slack.com client for Emacs
    smartscan # ; <M-n> <M-p> to move between same symbol in buffer
    use-package # ; to structure my init.el file
    tern # ; Javascript code analyzer
    visible-mark # ; show the current mark
    web-mode # ; Major mode for editing HTML files and templates
    wgrep-helm # ; edit grep buffers
    yaml-mode # ; to edit *.yml files (including .travis.yml)
    yasnippet # ; expand snippets of text
    xref-js2 # ; Jump to references/definitions using ag & js2-mode's AST (JavaScript)
    ]) ++ (with melpaPackages; [
    expand-region # ; <C-x => repeadly to mark regions
    # status : https://github.com/magnars/expand-region.el/issues/202
    org-caldav # ; sync between org and caldav
    # status: https://github.com/dengste/org-caldav/issues/97
    prodigy # ; manage external services from within Emacs
    # status : https://github.com/rejeep/prodigy.el/issues/89
    undo-tree # ; <C-x u> to show the undo tree
    # status: wait for nix update
    zoom-frm # ; increase/decrease font size for all buffers <C-x C-+>
    # status: ABANDONED email sent (code is on emacs wiki)
  ]) ++ (with elpaPackages; [
    auctex # ; LaTeX mode
    beacon # ; highlight my cursor when scrolling
    nameless # ; hide current package name everywhere in elisp code
    seq # ; sequence-manipulation libraryy
  ]) ++ [
    emacs-source-directory # give Emacs access to its own C sources
  ])
