{ pkgs ? import <nixpkgs> {}
, dashboardSrc ? ./.
}:

pkgs.stdenv.mkDerivation {
  buildInputs = with pkgs; [ elmPackages.elm ];
  name = "status-dashboard-client";
  src = dashboardSrc;

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"
  '';

  buildPhase = ''
    elm package install --yes
    elm make src/Main.elm --output dist/app.js
  '';

  installPhase = ''
    mv dist $out
  '';
}
