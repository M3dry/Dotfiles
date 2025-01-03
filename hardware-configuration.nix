# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:
{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" "v4l2loopback" ];
  boot.blacklistedKernelModules = [ "hid-thrustmaster" ];
  boot.extraModulePackages = with config.boot.kernelPackages; [ hid-tmff2 v4l2loopback.out ];
  boot.extraModprobeConfig = ''
    options v4l2loopback exclusive_caps=1 card_label="DROID Cam"
  '';
  boot.kernelParams = [ "clearcpuid=514" ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/2033460c-cb0d-4ce0-88a2-da6ff72fc37c";
      fsType = "ext4";
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/8939-02B2";
      fsType = "vfat";
    };

  fileSystems."/home/m3/my-stuff/Drives/1T" =
    { device = "/dev/disk/by-uuid/ffed52e8-3150-47d8-b373-f56b55b4c15a";
      fsType = "ext4";
    };

  fileSystems."/home/m3/my-stuff/Music" =
    { device = "/dev/disk/by-uuid/2bb3d55d-2865-4203-ad45-36676bddb33d";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/1c5c10b7-fcb1-4b2b-afa4-7e7035e2fa6e"; }
    ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp34s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlo1.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;
  hardware.graphics.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];

  hardware.nvidia = {
    modesetting.enable = true;
    open = false;
  };
}
