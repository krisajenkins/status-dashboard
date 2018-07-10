{ pkgs ? import <nixpkgs> {}
, dashboardSrc ? ./.
}:

pkgs.stdenv.mkDerivation {
  buildInputs = with pkgs; [ elmPackages.elm ];
  name = "status-dashboard-client";
  src = dashboardSrc;

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"
    elm package install --yes
  '';

  buildPhase = ''
    elm make src/Main.elm --output dist/app.js
    cp -r static/* dist/
  '';

  installPhase = ''
    mv dist $out
  '';
}
