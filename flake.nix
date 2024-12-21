{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    zig.url = "github:mitchellh/zig-overlay";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    # eww.url = "github:elkowar/eww?rev=de232de41b428b2caf1f41e021e67c0b86e65908";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bqnlsp.url = "sourcehut:~detegr/bqnlsp";
    taffybarr.url = "path:/home/m3/.config/taffybarr";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    zig,
    bqnlsp,
    taffybarr,
    neovim-nightly-overlay,
    home-manager,
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
    };
    inherit (nixpkgs) lib;
  in {
    nixosConfigurations = {
      m3 = lib.nixosSystem {
        inherit system;
        modules = [
          ./configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {
              zigpkgs = zig.packages.${system};
              # nvim = nvim.packages.${system}.default;
              # eww = eww.packages.${system};
              bqnlsp = bqnlsp.packages.${system}.lsp;
              taffybarr = taffybarr.packages.${system}.default;
              nvim-nightly = neovim-nightly-overlay.packages.${system}.default;
            };
            home-manager.users.m3 = {
              imports = [./home.nix];
            };
          }
        ];
      };
    };
  };
}
