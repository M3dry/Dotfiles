{ config, pkgs, ... }:
 
{
  imports = [ ./hardware-configuration.nix ];
 
  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
 
  networking.hostName = "SamsungSmartFridgeV2";
  networking.networkmanager.enable = true;
 
  time.timeZone = "Europe/Prague";
 
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
 
  services.geoclue2.enable = true;

  services.xserver.enable = true;
  services.xserver.libinput.mouse.accelProfile = "flat";
  services.xserver.videoDrivers = ["nvidia"];
  services.xserver.displayManager.startx.enable = true;
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };
 
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
    wireplumber.enable = true;
  };
 
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.defaultUserShell = pkgs.zsh;
  users.users.m3 = {
    isNormalUser = true;
    description = "m3";
    extraGroups = [ "networkmanager" "wheel" ];
    password = "pass";
  };

  security.polkit.enable = true;
  security.doas.enable = true;
  security.sudo.enable = false;
  security.doas.extraRules = [{
    users = [ "m3" ];
    keepEnv = true;
    persist = true;
  }];
 
  nixpkgs.config.allowUnfree = true;
  # the devil has my soul
  nixpkgs.config.joypixels.acceptLicense = true;
    
  programs.zsh = {
    enable = true;
    interactiveShellInit =
      let
        sources = with pkgs; [
          "${zsh-autopair}/share/zsh/zsh-autopair/autopair.zsh"
          "${zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
          "${zsh-you-should-use}/share/zsh/plugins/you-should-use/you-should-use.plugin.zsh"
          "${zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
        ];
        plugins = builtins.concatStringsSep "\n" (map (source: "source ${source}") sources);
      in
      ''
      ${plugins}
      '';
  };
  environment.systemPackages = with pkgs; [
    unzip killall htop
    gnumake gcc12 git wget
    # ZSH PLUGINs
    zsh-autopair
    zsh-you-should-use
    zsh-autosuggestions
    zsh-fast-syntax-highlighting
  ];

  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };
 
  programs.slock.enable = true;
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
 
  system.stateVersion = "22.11"; # Did you read the comment?
}
