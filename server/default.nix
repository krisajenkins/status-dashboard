{ pkgs ? import <nixpkgs> {}
, dashboardSrc ? ./.
}:

(import ./stack.nix {
  inherit dashboardSrc pkgs;
})."status-dashboard"
