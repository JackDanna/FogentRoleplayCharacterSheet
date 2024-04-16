{
  description = "A automated Character Sheet for Fogent Roleplay";

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
        psql_setup_file = pkgs.writeText "setup.sql" ''
          DO
          $do$
          BEGIN
            IF NOT EXISTS ( SELECT FROM pg_catalog.pg_roles WHERE rolname = 'hourglass') THEN
              CREATE ROLE hourglass CREATEDB LOGIN;
            END IF;
          END
          $do$
        '';

        postgres_setup = ''
          export PGDATA=$PWD/postgres_data
          export PGHOST=$PWD/postgres
          export LOG_PATH=$PWD/postgres/LOG
          export PGDATABASE=postgres
          export DATABASE_CLEANER_ALLOW_REMOTE_DATABASE_URL=true
          if [ ! -d $PGHOST ]; then
            mkdir -p $PGHOST
          fi
          if [ ! -d $PGDATA ]; then
            echo 'Initializing postgresql database...'
            LC_ALL=C.utf8 initdb $PGDATA --auth=trust >/dev/null
          fi
        '';

        start_postgres = pkgs.writeShellScriptBin "start_postgres" ''
          pg_ctl start -l $LOG_PATH -o "-c listen_addresses= -c unix_socket_directories=$PGHOST"
          psql -f ${psql_setup_file} > /dev/null
        '';

        stop_postgres = pkgs.writeShellScriptBin "stop_postgres" ''
          pg_ctl -D $PGDATA stop
        '';
      in pkgs.mkShell rec {
        name = "FRCS";
        buildInputs = with pkgs; [
          dotnet-sdk_8
          nodejs_20
          postgresql_16
          start_postgres
          stop_postgres

          gnome.gnome-terminal
          bashInteractive
          (vscode-with-extensions.override  {
            vscode = pkgs.vscode;
            vscodeExtensions = with pkgs.vscode-extensions; [
              jnoortheen.nix-ide
              mhutchie.git-graph
              ms-dotnettools.csharp
              ionide.ionide-fsharp
              vscodevim.vim
            ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
              {
                name = "vscode-edit-csv";
                publisher = "janisdd";
                version = "0.8.2";
                sha256 = "sha256-DbAGQnizAzvpITtPwG4BHflUwBUrmOWCO7hRDOr/YWQ=";
              }
            ];
          })

          (pkgs.writeShellScriptBin "iDontKnow" ''
            ${pkgs.figlet}/bin/figlet "IDontKnow"
          '')
        ];

        shellHook = ''
          ${postgres_setup}

          export PS1+="${name}> "
          echo "Welcome to the Fogent Roleplay Character Sheet Shell"
        '';
      };
  }; 

}

