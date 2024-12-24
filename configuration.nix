{
  pkgs,
  ...
}: {
  imports = [./hardware-configuration.nix];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostName = "SamsungSmartFridgeV2";
  networking.networkmanager.enable = true;
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      8080
    ];
    extraCommands = "iptables -A OUTPUT -m owner --gid-owner no-internet -j DROP";
  };

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

  xdg.portal.enable = true;
  xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gtk];
  xdg.portal.config.common.default = "*";
  # xdg.portal = {
  #   enable = true;
  #   extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  # };
  services.flatpak.enable = true;

  services.xserver.wacom.enable = true;
  services.xserver.digimend.enable = true;

  services.geoclue2.enable = true;
  services.ratbagd.enable = true;
  services.blueman.enable = true;

  services.xrdp.enable = true;
  services.xrdp.defaultWindowManager = "xmonad";
  services.xrdp.openFirewall = true;
  services.xrdp.port = 4200;

  services.libinput.enable = true;
  services.libinput.mouse.accelProfile = "flat";

  services.xserver.enable = true;
  services.xserver.videoDrivers = ["nvidia"];
  services.xserver.displayManager.startx.enable = true;
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
  services.xserver = {
    xkb.layout = "us";
    xkb.variant = "";
  };

  hardware.pulseaudio.enable = false;
  hardware.bluetooth.enable = true;
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
    extraGroups = ["networkmanager" "wheel" "input"];
    password = "pass";
  };
  nix.settings.trusted-users = ["m3"];

  users.groups.no-internet = {};

  security.polkit.enable = true;
  security.doas.enable = true;
  security.sudo.enable = true;
  security.doas.extraRules = [
    {
      users = ["m3"];
      keepEnv = true;
      persist = true;
    }
  ];

  nixpkgs.config.allowUnfree = true;
  # the devil has my soul
  nixpkgs.config.joypixels.acceptLicense = true;

  programs.zsh = {
    enable = true;
    interactiveShellInit = let
      sources = with pkgs; [
        "${zsh-autopair}/share/zsh/zsh-autopair/autopair.zsh"
        "${zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
        "${zsh-you-should-use}/share/zsh/plugins/you-should-use/you-should-use.plugin.zsh"
        "${zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
      ];
      plugins = builtins.concatStringsSep "\n" (map (source: "source ${source}") sources);
    in ''
      ${plugins}
    '';
  };
  environment.systemPackages = with pkgs; [
    xorg.xhost
    jmtpfs
    unzip
    killall
    htop
    gnumake
    gcc12
    git
    wget
    xrdp
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

  documentation.enable = true;
  documentation.man.enable = true;
  documentation.dev.enable = true;

  programs.slock.enable = true;
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = pkgs.pinentry-qt;
  };

  system.stateVersion = "22.11"; # Did you read the comment?

  services.udev.extraRules = ''
    # 8Bitdo F30 P1
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo FC30 GamePad", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8Bitdo F30 P2
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo FC30 II", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8Bitdo N30
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo NES30 GamePad", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8Bitdo SF30
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo SFC30 GamePad", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8Bitdo SN30
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo SNES30 GamePad", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8Bitdo F30 Pro
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo FC30 Pro", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8Bitdo N30 Pro
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo NES30 Pro", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8Bitdo SF30 Pro
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo SF30 Pro", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8Bitdo SN30 Pro
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo SN30 Pro", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8BitDo SN30 Pro+; Bluetooth; USB
    SUBSYSTEM=="input", ATTRS{name}=="8BitDo SN30 Pro+", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo SF30 Pro   8BitDo SN30 Pro+", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8Bitdo F30 Arcade
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo Joy", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8Bitdo N30 Arcade
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo NES30 Arcade", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8Bitdo ZERO
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo Zero GamePad", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8Bitdo Retro-Bit xRB8-64
    SUBSYSTEM=="input", ATTRS{name}=="8Bitdo N64 GamePad", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # 8BitDo Pro 2; Bluetooth; USB
    SUBSYSTEM=="input", ATTRS{name}=="8BitDo Pro 2", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    SUBSYSTEM=="input", ATTR{id/vendor}=="2dc8", ATTR{id/product}=="6003", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # Alpha Imaging Technology Corp.
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="114d", ATTRS{idProduct}=="8a12", TAG+="uaccess"
    # ASTRO Gaming C40 Controller; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="9886", ATTRS{idProduct}=="0025", MODE="0660", TAG+="uaccess"
    # Betop PS4 Fun Controller
    KERNEL=="hidraw*", ATTRS{idVendor}=="11c0", ATTRS{idProduct}=="4001", MODE="0660", TAG+="uaccess"
    # Hori RAP4
    KERNEL=="hidraw*", ATTRS{idVendor}=="0f0d", ATTRS{idProduct}=="008a", MODE="0660", TAG+="uaccess"
    # Hori HORIPAD 4 FPS
    KERNEL=="hidraw*", ATTRS{idVendor}=="0f0d", ATTRS{idProduct}=="0055", MODE="0660", TAG+="uaccess"
    # Hori HORIPAD 4 FPS Plus
    KERNEL=="hidraw*", ATTRS{idVendor}=="0f0d", ATTRS{idProduct}=="0066", MODE="0660", TAG+="uaccess"
    # Hori HORIPAD S; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0f0d", ATTRS{idProduct}=="00c1", MODE="0660", TAG+="uaccess"
    # Hori Nintendo Switch HORIPAD Wired Controller; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0f0d", ATTRS{idProduct}=="00c1", MODE="0660", TAG+="uaccess"
    # HTC
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="0bb4", ATTRS{idProduct}=="2c87", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="0bb4", ATTRS{idProduct}=="0306", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="0bb4", ATTRS{idProduct}=="0309", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="0bb4", ATTRS{idProduct}=="030a", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="0bb4", ATTRS{idProduct}=="030b", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="0bb4", ATTRS{idProduct}=="030c", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="0bb4", ATTRS{idProduct}=="030e", TAG+="uaccess"
    # HTC VIVE Cosmos; USB; https://gitlab.com/fabis_cafe/game-devices-udev/-/issues/1/ #EXPERIMENTAL
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="0bb4", ATTRS{idProduct}=="0313", TAG+="uaccess"
    SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTRS{idVendor}=="057e", ATTRS{idProduct}=="0315", MODE="0660", TAG+="uaccess"
    SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTRS{idVendor}=="057e", ATTRS{idProduct}=="0323", MODE="0660", TAG+="uaccess"
    # Logitech F310 Gamepad; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="046d", ATTRS{idProduct}=="c216", MODE="0660", TAG+="uaccess"
    # Logitech F710 Wireless Gamepad; USB #EXPERIMENTAL
    KERNEL=="hidraw*", ATTRS{idVendor}=="046d", ATTRS{idProduct}=="c21f", MODE="0660", TAG+="uaccess"
    # Mad Catz Street Fighter V Arcade FightPad PRO
    KERNEL=="hidraw*", ATTRS{idVendor}=="0738", ATTRS{idProduct}=="8250", MODE="0660", TAG+="uaccess"
    # Mad Catz Street Fighter V Arcade FightStick TE S+
    KERNEL=="hidraw*", ATTRS{idVendor}=="0738", ATTRS{idProduct}=="8384", MODE="0660", TAG+="uaccess"
    # Microsoft Xbox360 Controller; USB #EXPERIMENTAL
    SUBSYSTEM=="usb", ATTRS{idVendor}=="045e", ATTRS{idProduct}=="028e", MODE="0660", TAG+="uaccess"
    SUBSYSTEMS=="input", ATTRS{name}=="Microsoft X-Box 360 pad", MODE="0660", TAG+="uaccess"
    # Microsoft Xbox 360 Wireless Receiver for Windows; USB
    SUBSYSTEM=="usb", ATTRS{idVendor}=="045e", ATTRS{idProduct}=="0719", MODE="0660", TAG+="uaccess"
    SUBSYSTEMS=="input", ATTRS{name}=="Xbox 360 Wireless Receiver", MODE="0660", TAG+="uaccess"
    # Microsoft Xbox One S Controller; bluetooth; USB #EXPERIMENTAL
    KERNEL=="hidraw*", KERNELS=="*045e:02ea*", MODE="0660", TAG+="uaccess"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="045e", ATTRS{idProduct}=="02ea", MODE="0660", TAG+="uaccess"
    # Nacon PS4 Revolution Pro Controller
    KERNEL=="hidraw*", ATTRS{idVendor}=="146b", ATTRS{idProduct}=="0d01", MODE="0660", TAG+="uaccess"
    # Nintendo Switch Pro Controller; bluetooth; USB
    KERNEL=="hidraw*", KERNELS=="*057E:2009*", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", ATTRS{idVendor}=="057e", ATTRS{idProduct}=="2009", MODE="0660", TAG+="uaccess"
    # Nintendo GameCube Controller / Adapter; USB
    SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTRS{idVendor}=="057e", ATTRS{idProduct}=="0337", MODE="0660", TAG+="uaccess"
    # NVIDIA Shield Portable (2013 - NVIDIA_Controller_v01.01 - In-Home Streaming only)
    KERNEL=="hidraw*", ATTRS{idVendor}=="0955", ATTRS{idProduct}=="7203", ENV{ID_INPUT_JOYSTICK}="1", ENV{ID_INPUT_MOUSE}="", MODE="0660", TAG+="uaccess"
    # NVIDIA Shield Controller (2017 - NVIDIA_Controller_v01.04); bluetooth
    KERNEL=="hidraw*", KERNELS=="*0955:7214*", MODE="0660", TAG+="uaccess"
    # NVIDIA Shield Controller (2015 - NVIDIA_Controller_v01.03); USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0955", ATTRS{idProduct}=="7210", ENV{ID_INPUT_JOYSTICK}="1", ENV{ID_INPUT_MOUSE}="", MODE="0660", TAG+="uaccess"
    # PDP Afterglow Deluxe+ Wired Controller; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0e6f", ATTRS{idProduct}=="0188", MODE="0660", TAG+="uaccess"
    # PDP Nintendo Switch Faceoff Wired Pro Controller; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0e6f", ATTRS{idProduct}=="0180", MODE="0660", TAG+="uaccess"
    # PDP Wired Fight Pad Pro for Nintendo Switch; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0e6f", ATTRS{idProduct}=="0185", MODE="0666", TAG+="uaccess"
    # Personal Communication Systems, Inc. Twin USB Gamepad; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0810", ATTRS{idProduct}=="e301", MODE="0660", TAG+="uaccess"
    SUBSYSTEM=="input", ATTRS{name}=="Twin USB Gamepad*", ENV{ID_INPUT_JOYSTICK}="1", TAG+="uaccess"
    # PowerA Wired Controller for Nintendo Switch; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="20d6", ATTRS{idProduct}=="a711", MODE="0660", TAG+="uaccess"
    # PowerA Zelda Wired Controller for Nintendo Switch; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="20d6", ATTRS{idProduct}=="a713", MODE="0660", TAG+="uaccess"
    # PowerA Wireless Controller for Nintendo Switch; bluetooth
    # We have to use ATTRS{name} since VID/PID are reported as zeros.
    # We use sh instead of udevadm directly becuase we need to
    # use '*' glob at the end of "hidraw" name since we don't know the index it'd have.
    # Thanks @https://github.com/ValveSoftware
    # KERNEL=="input*", ATTRS{name}=="Lic Pro Controller", RUN{program}+="sh -c 'udevadm test-builtin uaccess /sys/%p/../../hidraw/hidraw*'"
    # Razer Raiju PS4 Controller
    KERNEL=="hidraw*", ATTRS{idVendor}=="1532", ATTRS{idProduct}=="1000", MODE="0660", TAG+="uaccess"
    # Razer Panthera Arcade Stick
    KERNEL=="hidraw*", ATTRS{idVendor}=="1532", ATTRS{idProduct}=="0401", MODE="0660", TAG+="uaccess"
    # Sony PlayStation Strikepack; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="05c5", MODE="0660", TAG+="uaccess"
    # Sony PlayStation DualShock 3; bluetooth; USB
    KERNEL=="hidraw*", KERNELS=="*054C:0268*", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="0268", MODE="0660", TAG+="uaccess"
    ## Motion Sensors
    SUBSYSTEM=="input", KERNEL=="event*|input*", KERNELS=="*054C:0268*", TAG+="uaccess"
    # Sony PlayStation DualShock 4; bluetooth; USB
    KERNEL=="hidraw*", KERNELS=="*054C:05C4*", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="05c4", MODE="0660", TAG+="uaccess"
    # Sony PlayStation DualShock 4 Slim; bluetooth; USB
    KERNEL=="hidraw*", KERNELS=="*054C:09CC*", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="09cc", MODE="0660", TAG+="uaccess"
    # Sony PlayStation DualShock 4 Wireless Adapter; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="0ba0", MODE="0660", TAG+="uaccess"
    # Sony DualSense Wireless-Controller; bluetooth; USB
    KERNEL=="hidraw*", KERNELS=="*054C:0CE6*", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="0ce6", MODE="0660", TAG+="uaccess"
    # PlayStation VR; USB
    SUBSYSTEM=="usb", ATTR{idVendor}=="054c", ATTR{idProduct}=="09af", MODE="0660", TAG+="uaccess"
    # Valve generic(all) USB devices
    SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", MODE="0660", TAG+="uaccess"
    # Valve Steam Controller write access
    KERNEL=="uinput", SUBSYSTEM=="misc", TAG+="uaccess", OPTIONS+="static_node=uinput"
    # Valve HID devices; bluetooth; USB
    KERNEL=="hidraw*", KERNELS=="*28DE:*", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", ATTRS{idVendor}=="28de", MODE="0660", TAG+="uaccess"
    # Valve
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="1043", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="1142", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2000", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2010", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2011", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2012", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2021", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2022", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2050", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2101", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2102", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2150", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2300", MODE="0660", TAG+="uaccess"
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="28de", ATTRS{idProduct}=="2301", MODE="0660", TAG+="uaccess"
    # Zeroplus(ZP) appears to be a tech-provider for variouse other companies.
    # They all use the ZP ID. Because of this, they are grouped in this rule.
    # Armor PS4 Armor 3 Pad; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0c12", ATTRS{idProduct}=="0e10", MODE="0660", TAG+="uaccess"
    # EMiO PS4 Elite Controller; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0c12", ATTRS{idProduct}=="1cf6", MODE="0660", TAG+="uaccess"
    # Hit Box Arcade HIT BOX PS4/PC version; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0c12", ATTRS{idProduct}=="0ef6", MODE="0660", TAG+="uaccess"
    # Nyko Xbox Controller; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0c12", ATTRS{idProduct}=="8801", MODE="0660", TAG+="uaccess"
    # Unknown-Brand Xbox Controller; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0c12", ATTRS{idProduct}=="8802", MODE="0660", TAG+="uaccess"
    # Unknown-Brand Xbox Controller; USB
    KERNEL=="hidraw*", ATTRS{idVendor}=="0c12", ATTRS{idProduct}=="8810", MODE="0660", TAG+="uaccess"
  '';
}
