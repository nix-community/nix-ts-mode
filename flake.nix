{
  description = "An Emacs major mode for editing Nix expressions, powered by tree-sitter.";

  outputs = {
    systems,
    nixpkgs-unstable,
    nixpkgs-master,
    emacs-unstable,
    nix-emacs-ci,
    pre-commit-nix,
    ...
  }: let
    supportedSystems = import systems;
    forAllSystems = nixpkgs-unstable.lib.genAttrs supportedSystems;
  in {
    packages = forAllSystems (system: let
      versions = ["emacs-release-snapshot" "emacs-snapshot"];

      pkgs = nixpkgs-master.legacyPackages.${system};

      addNixGrammar = emacs:
        with pkgs;
          (emacsPackagesFor emacs).emacsWithPackages (epkgs: [
            (epkgs.manualPackages.treesit-grammars.with-grammars
              (grammars: [
                grammars.tree-sitter-nix
              ]))
          ]);
    in
      builtins.listToAttrs (builtins.map (version: {
          name = version;
          value = addNixGrammar nix-emacs-ci.packages.${system}.${version};
        })
        versions));

    devShells = forAllSystems (system: let
      pkgs = import nixpkgs-unstable {
        inherit system;

        overlays = [
          (_: _: {
            emacs = emacs-unstable.packages.${system}.emacs-git.overrideAttrs (prev: {
              passthru =
                prev.passthru
                // {
                  withTreeSitter = true;
                };
            });
          })
          emacs-unstable.overlays.default
        ];
      };

      pkgs-master = import nixpkgs-master {
        inherit system;

        overlays = [emacs-unstable.overlays.default];
      };

      default-init = pkgs.emacsPackages.trivialBuild {
        pname = "default-init";
        requiresPackages = [pkgs.emacsPackages.aggressive-indent];

        src = pkgs.writeText "default.el" ''
          (use-package aggressive-indent
           :hook
           (emacs-lisp-mode . aggressive-indent-mode))

          (defun reload-nix-buffers ()
            (interactive)
            (mapc
            (lambda (buffer)
              (with-current-buffer buffer
                (unless buffer-read-only
            (when (derived-mode-p 'nix-ts-mode)
              (unload-feature 'nix-ts-mode t)
              (load-file "nix-ts-mode.el")
              (nix-ts-mode)
              (treesit-inspect-mode)))))
            (buffer-list)))
        '';
      };

      emacsWithPackages = (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs: [
        epkgs.aggressive-indent
        default-init
        (pkgs-master.emacsPackages.manualPackages.treesit-grammars.with-grammars
          (grammars: [
            grammars.tree-sitter-nix
          ]))
      ]);

      pre-commit-check = pre-commit-nix.lib.${system}.run {
        src = ./.;
        hooks = {
          alejandra.enable = true;
          deadnix.enable = true;
          statix.enable = true;
          commitizen.enable = true;
        };
      };
    in {
      default = pkgs.mkShell {
        name = "nix-ts-mode-shell";

        packages = with pkgs; [
          emacsWithPackages
          pre-commit-nix.packages.${system}.statix
        ];

        shellHook = ''
          ${pre-commit-check.shellHook}
        '';
      };
    });
  };

  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs";
    emacs-unstable.url = "github:nix-community/emacs-overlay";
    nix-emacs-ci.url = "github:purcell/nix-emacs-ci";
    systems.url = "github:nix-systems/default";
    pre-commit-nix.url = "github:cachix/pre-commit-hooks.nix";
  };
}
