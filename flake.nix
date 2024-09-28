{
  description = "An automated Character Sheet for Fogent Roleplay";

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config = {
        allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
          "vscode-with-extensions"
          "vscode"
        ];
      };
    };
  in 
  {
    packages.${system} = {

      default = pkgs.writeShellScriptBin "run" ''
        nix develop -c -- code .
      '';
      
      test = pkgs.writeShellScriptBin "fa" ''

        ${pkgs.figlet}/bin/figlet "Fallen is awesome!"
      '';

    };
    

    devShells.${system}.default =
      let

      in pkgs.mkShell rec {
        name = "FRCS";
        buildInputs = with pkgs; [
          dotnet-sdk_8
          nodejs_20
          gnome.gnome-terminal
          bashInteractive
          (vscode-with-extensions.override  {
            vscode = pkgs.vscode;
            vscodeExtensions = with pkgs.vscode-extensions; [
              jnoortheen.nix-ide
              mhutchie.git-graph
              ms-dotnettools.csharp
              ionide.ionide-fsharp
              #vscodevim.vim
            ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
              {
                name = "vscode-edit-csv";
                publisher = "janisdd";
                version = "0.8.2";
                sha256 = "sha256-DbAGQnizAzvpITtPwG4BHflUwBUrmOWCO7hRDOr/YWQ=";
              }
              {
                name = "vscode-dotnet-runtime";
                publisher = "ms-dotnettools";
                version = "2.0.5";
                sha256 = "sha256-acP3NULTNNyPw5052ZX1L+ymqn9+t4ydoCns29Ta1MU=";
              }
            ];
          })

          (pkgs.writeShellScriptBin "iDontKnow" ''
            ${pkgs.figlet}/bin/figlet "IDontKnow"
          '')
        ];

        shellHook = ''
          export PS1+="${name}> "
          echo "Welcome to the Fogent Roleplay Character Sheet Shell"
        '';
      };
  }; 

}

