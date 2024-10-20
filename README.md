# Fogent Roleplay Character Sheet

This is a application for managing Fogent Roleplay Characters. It was built from the [SAFE Stack](https://safe-stack.github.io/). It was created using the dotnet [SAFE Template](https://safe-stack.github.io/docs/template-overview/). If you want to learn more about the template why not start with the [quick start](https://safe-stack.github.io/docs/quickstart/) guide?

## Part 1: Installing pre-requisites:

### Option A: Install pre-requisites with flake.nix

You'll need to install/enable the following pre-requisites in order to build the app.

- Install [Nix](https://nixos.org/)
    - Enable [Flakes](https://nixos.wiki/wiki/Flakes)

Enter this repositories directory and run the following on the command line:

```bash
nix run
```

This will install all dependencies and development tools. You'll then be droped into a vscode enviroment with all the recommend F# plugins installed.

### Option B: Install pre-requisites manually

You'll need to install the following pre-requisites in order to build SAFE applications

* [.NET SDK](https://www.microsoft.com/net/download) 8.0 or higher
* [Node 18](https://nodejs.org/en/download/) or higher
* [NPM 9](https://www.npmjs.com/package/npm) or higher

## Part 2: Starting the application

The application uses the [FogentRoleplayData](https://github.com/JackDanna/FogentRoleplayData) repository as a git submodule, which contains .csv files with all the data on the settings it supports. Run the following command to pull it down.

```bash
git submodule update --recursive --remote
```

Before you run the project **for the first time only** you must install dotnet "local tools" with this command:

```bash
dotnet tool restore
```

To concurrently run the server and the client components in watch mode use the following command:

```bash
dotnet run
```

Then open `http://localhost:8080` in your browser.

The build project in root directory contains a couple of different build targets. You can specify them after `--` (target name is case-insensitive).

To run concurrently server and client tests in watch mode (you can run this command in parallel to the previous one in new terminal):

```bash
dotnet run -- RunTests
```

Client tests are available under `http://localhost:8081` in your browser and server tests are running in watch mode in console.

Finally, there are `Bundle` and `Azure` targets that you can use to package your app and deploy to Azure, respectively:

```bash
dotnet run -- Bundle
dotnet run -- Azure
```

## SAFE Stack Documentation

If you want to know more about the full Azure Stack and all of it's components (including Azure) visit the official [SAFE documentation](https://safe-stack.github.io/docs/).

You will find more documentation about the used F# components at the following places:

* [Saturn](https://saturnframework.org/)
* [Fable](https://fable.io/docs/)
* [Elmish](https://elmish.github.io/elmish/)
