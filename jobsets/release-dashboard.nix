{ nixpkgs ? <nixpkgs>
, dashboardSrc ? ../.
}:
let pkgs = import nixpkgs {};
in {
  dashboardClient = pkgs.callPackage ../client {
    dashboardSrc = builtins.toPath (dashboardSrc + "/client");
  };

  dashboardServer = pkgs.callPackage ../server {
    dashboardSrc = builtins.toPath (dashboardSrc + "/server");
  };
}
