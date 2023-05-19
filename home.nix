{ config, pkgs, zigpkgs, system, ... }:
let
  my-stuff = "$HOME/my-stuff";
in
{
  home = {
    username = "m3";
    homeDirectory = "/home/m3";
    stateVersion = "22.11";
    sessionVariables = {
      GOPATH = "$HOME/.local/go";
      EDITOR = "nvim";
      VISUAL = "emacsclient -c";
      TERMINAL = "st";
      BROWSER = "firefox";
      PAGER = "nvimpager";
      MANPAGER = "less";
      COLORTERM = "truecolor";
      XENVIRONMENT = "$HOME/.config/x11/xresources";
    };
    sessionPath = [
      "${config.xdg.configHome}/emacs/bin"
      "$HOME/.local/bin"
      "$HOME/.cargo/bin"
    ];
    file = {
      ".bashrc".source = ./dots/.bashrc;
      ".xinitrc".source = ./dots/.xinitrc;
      ".themes/paledeep".source = ./dots/paledeep;
      ".icons/paledeep-icons".source = ./dots/paledeep-icons;
      ".local/bin" = {
          source = ./dots/bin;
          recursive = true;
      };
    };
  };

  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    desktop = "${my-stuff}/Desktop";
    download = "${my-stuff}/Downloads";
    documents = "${my-stuff}/Documents";
    pictures = "${my-stuff}/Pictures";
    templates = "${my-stuff}/Templates";
    publicShare = "${my-stuff}/Public";
    music = "${my-stuff}/Music";
    videos = "${my-stuff}/Videos";
  };
  xdg.configFile = {
    "alacritty/alacritty.yml".source = ./dots/alacritty.yml;
    eww = {
        source = ./dots/eww;
        recursive = true;
    };
    "picom/picom.conf".source = ./dots/picom.conf;
    xmonad = {
      source = ./dots/xmonad;
      recursive = true;
    };
    taffybar = {
      source = ./dots/taffybar;
      recursive = true;
    };
    nvim = {
      source = ./dots/nvim;
      recursive = true;
    };
    doom = {
      source = ./dots/doom;
      recursive = true;
    };
    x11 = {
      source = ./dots/x11;
      recursive = true;
    };
    cmus = {
      source = ./dots/cmus;
      recursive = true;
    };
    zsh = {
      source = ./dots/zsh;
      recursive = true;
    };
    "htop/htoprc".source = ./dots/htoprc;
    "mimeapps.list".source = ./dots/mimeapps.list;
    "proj.conf".source = ./dots/proj.conf;
  };

  xsession.enable = true;
  xsession.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
  };

  programs.home-manager.enable = true;
  programs.git = {
    enable = true;
    userEmail = "m3dry@tutamail.com";
    userName = "m3dry";
    extraConfig = {
      init = {
        defaultBranch = "main";
      };
    };
  };
  programs.password-store = {
    enable = true;
    settings.PASSWORD_STORE_DIR = "$HOME/my-stuff/pass/";
  };
  programs.browserpass = {
    enable = true;
    browsers = [ "firefox" "chromium" ];
  };
  programs.man.generateCaches = true;
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
  };
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
  };
  programs.zsh = {
      enable = true;
      initExtraFirst = builtins.readFile ./dots/.zshrc;
      shellAliases = {
        ll = "exa -lar --color=always --group-directories-first --icons";
        lg = "exa -gar --color=always --group-directories-first --icons";
        rm = "rm -i";
        ka = "killall";
        v = "nvim";
        ".." = "cd .." ;
        "..." = "cd ../..";
        mount = "doas mount ";
        umount = "doas umount ";
        config = "/usr/bin/git --git-dir=$HOME/Dotfiles/ --work-tree=$HOME";
        xload = "xrdb merge ~/.config/x11/xresources && kill -USR1 $(pidof st)";
        nsxiv = "nsxiv -a";
        sxiv = "nsxiv -a";
        yta-aac = "youtube-dl --extract-audio --audio-format aac";
        yta-best = "youtube-dl --extract-audio --audio-format best";
        yta-flac = "youtube-dl --extract-audio --audio-format flac";
        yta-m4a = "youtube-dl --extract-audio --audio-format m4a";
        yta-mp3 = "youtube-dl --extract-audio --audio-format mp3";
        yta-opus = "youtube-dl --extract-audio --audio-format opus";
        yta-vorbis = "youtube-dl --extract-audio --audio-format vorbis";
        yta-wav = "youtube-dl --extract-audio --audio-format wav";
        ytv-best = "youtube-dl -f bestvideo+bestaudio";
        hh = "$HOME";
        mm = "$HOME/my-stuff";
        mp = "$HOME/my-stuff/Pictures";
        mpw = "$HOME/my-stuff/Pictures/wallpapers";
        mps = "$HOME/my-stuff/Pictures/snips";
        md = "$HOME/my-stuff/Downloads";
        mdr = "$HOME/my-stuff/Drives";
        mdo = "$HOME/my-stuff/Documents";
        mb = "$HOME/my-stuff/Books";
        mpr = "$HOME/my-stuff/Projects";
        mmu = "$HOME/my-stuff/Music";
        mvi = "$HOME/my-stuff/Videos";
        cc = "$HOME/.config/flake/dots";
        cf = "$HOME/.config/flake";
        cx = "$HOME/.config/flake/dots/xmonad";
        ct = "$HOME/.config/flake/dots/taffybar";
        ce = "$HOME/.config/flake/dots/eww";
        cdo = "$HOME/.config/flake/dots/doom";
        cdw = "$HOME/.config/dwm";
        cs = "$HOME/.config/st";
        cdm = "$HOME/.config/dmenu";
        lc = "$HOME/.local";
        lcb = "$HOME/.local/bin";
      };
      dotDir = ".config/zsh";
      history = {
        path = "${config.xdg.dataHome}/zsh/zsh_history";
        save = 10000000;
      };
  };

  home.packages = with pkgs; [
    rustdesk
    nsxiv
    git gh
    pcmanfm
    zathura
    anki-bin
    nvimpager
    alacritty
    prismlauncher
    docker-compose
    docker-machine
    clang-tools_15
    firefox chromium
    yt-dlp youtube-dl
    gimp-with-plugins
    ripgrep fd exa fzf bat
    lutris steam grapejuice
    jq imagemagick trash-cli
    libqalculate qalculate-gtk
    numlockx xdo xdotool xorg.xkill
    xcape xorg.setxkbmap xorg.xrdb
    xorg.xset xclip xsel xorg.xprop
    (dmenu.overrideAttrs (oldAttrs: rec {
      src = builtins.fetchTarball {
        url = "https://github.com/m3dry/dmenu/archive/master.tar.gz";
        sha256 = "1fv7ngsyvx1a0z9b52z46lvspwycs2vynri8wv11391zivig0j60";
      };
    }))
    (st.overrideAttrs (oldAttrs: rec {
      src = builtins.fetchTarball {
        url = "https://github.com/m3dry/st/archive/master.tar.gz";
        sha256 = "1y04wfzryn2wlrgxldk74xvbj8f7vj31lrj2dlyw4sf31mcrymyb";
      };
      makeFlags = oldAttrs.makeFlags ++ [ "PREFIX=$(out)" ];
      buildInputs = oldAttrs.buildInputs ++ [ harfbuzz gd ];
      nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ autoPatchelfHook ];
    }))
    # EMACS
    sqlite
    pdf2svg
    gnuplot
    binutils
    texlive.combined.scheme-full
    (aspellWithDicts (dicts: with dicts; [
      en
      en-computers
      en-science
    ]))
    # AUDIO/VIDEO
    mpv
    cmus
    spotdl
    ffmpeg
    spotify
    pamixer
    playerctl
    pulsemixer
    # RICING
    eww
    xwallpaper
    lxappearance
    picom-jonaburg
    # FONTs
    nerdfonts
    joypixels
    emacs-all-the-icons-fonts
    # LANGUAGEs
    go
    zigpkgs.master
    ghc
    lua5_4
    clojure
    python3
    # RUST
    rustup
    cargo-insta
    # LSPs
    taplo
    gopls
    clojure-lsp
    rust-analyzer
    haskell-language-server
    sumneko-lua-language-server
    python310Packages.python-lsp-server
    nodePackages.bash-language-server
    nodePackages.svelte-language-server
    nodePackages.typescript-language-server
    nodePackages.vscode-json-languageserver-bin
    nodePackages.vscode-css-languageserver-bin
    nodePackages.vscode-html-languageserver-bin
    # LINTers
    statix
    shellcheck
    # DEBUGers
    vscode-extensions.vadimcn.vscode-lldb
    # FORMATters
    alejandra
    nodePackages.prettier
    stylua
    haskellPackages.fourmolu
  ];

  fonts.fontconfig.enable = true;

  services.playerctld.enable = true;
  services.emacs.enable = true;
  services.taffybar.enable = true;
  services.status-notifier-watcher.enable = true;
  services.redshift = {
    enable = true;
    provider = "geoclue2";
    temperature = {
      day = 5700;
      night = 2000;
    };
  };
}
