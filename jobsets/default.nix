{ nixpkgs ? <nixpkgs>
, declInput ? {
    uri = "https://github.com/krisajenkins/status-dashboard.git";
    rev = "refs/heads/master";
  }
, dashboardPrsJSON ? ./simple-pr-dummy.json
}:
let pkgs = import nixpkgs {};

    dashboardPrs = builtins.fromJSON (builtins.readFile dashboardPrsJSON );

    mkGitSrc = { repo, branch ? "refs/heads/master", deepClone ? false }: {
      type = "git";
      value = repo + " " + branch + (if deepClone then " deepClone" else "");
      emailresponsible = false;
    };

    mkJob = { name, description, nixexprinput ? "jobsetSrc", nixexprpath, extraInputs }: {
      inherit name;
      value = {
        inherit description nixexprinput nixexprpath;

        inputs = {
          jobsetSrc = mkGitSrc {
            repo = declInput.uri;
            branch = declInput.rev;
          };

          nixpkgs = mkGitSrc {
            repo = "https://github.com/NixOS/nixpkgs-channels";
            branch = "refs/heads/nixos-18.03";
          };
        } // extraInputs;

        enabled = 1;
        hidden = false;
        checkinterval = 90;
        schedulingshares = 100;
        emailoverride = "";
        enableemail = false;
        keepnr = 3;
      };
    };

    mkDashboardJob = { name, description, dashboardBranch }:
      mkJob {
        inherit name description;
        nixexprpath = "jobsets/release-dashboard.nix";
        extraInputs = {
          dashboardSrc = mkGitSrc {
            repo = "https://github.com/krisajenkins/status-dashboard.git";
            branch = dashboardBranch;
            deepClone = true;
          };
        };
      };

    dashboardJobsetDefinition = pkgs.lib.listToAttrs (
      [
        (mkDashboardJob {
          name = "master";
          description = "master";
          dashboardBranch = "refs/heads/master";
        })
      ]
      ++
      (pkgs.lib.mapAttrsToList
        (
          num:
          info: mkDashboardJob {
            name = "dashboard-PR-${num}";
            description = info.title;
            dashboardBranch = info.head.sha;
          }
        )
        dashboardPrs
      )
    );

    jobsetDefinition = dashboardJobsetDefinition;
in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF

    cat <<EOF
    ${builtins.toJSON jobsetDefinition}
    EOF

    cat > $out <<EOF
    ${builtins.toJSON jobsetDefinition}
    EOF
  '';
}
