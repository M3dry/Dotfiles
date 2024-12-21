{ config, pkgs, zigpkgs, bqnlsp, taffybarr, nvim-nightly, system, ... }:
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
      XINITRC = "$HOME/.config/x11/xinitrc";
      XCOMPOSECACHE = "${config.xdg.cacheHome}/X11/xcompose";
      WINEPREFIX = "${config.xdg.dataHome}/wine";
      RUSTUP_HOME = "${config.xdg.dataHome}/rustup";
      CARGO_HOME = "${config.xdg.dataHome}/cargo";
      OPAMROOT = "${config.xdg.dataHome}/opam";
      GTK2_RC_FILES = "$HOME/.config/gtk-2.0/gtkrc";
      DOCKER_CONFIG = "$HOME/.config/docker";
    };
    shellAliases = {
      ll = "eza -lar --group-directories-first";
      lg = "eza -gar --group-directories-first";
      rm = "rm -i";
      ka = "killall";
      v = "nvim";
      ".." = "cd .." ;
      "..." = "cd ../..";
      mount = "doas mount ";
      umount = "doas umount ";
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
      ce = "$HOME/.config/flake/dots/eww";
      cdo = "$HOME/.config/flake/dots/doom";
      cdw = "$HOME/.config/dwm";
      cs = "$HOME/.config/st";
      cdm = "$HOME/.config/dmenu";
      lc = "$HOME/.local";
      lcb = "$HOME/.local/bin";
    };
    sessionPath = [
      "${config.xdg.configHome}/emacs/bin"
      "$HOME/.local/bin"
      "$HOME/.cargo/bin"
    ];
    file = {
      ".bashrc".source = ./dots/.bashrc;
      "${config.xdg.dataHome}/paledeep".source = ./dots/paledeep;
      "${config.xdg.dataHome}/paledeep-icons".source = ./dots/paledeep-icons;
      ".local/bin" = {
          source = ./dots/bin;
          recursive = true;
      };
      ".config/picom/picom.conf".source = ./dots/picom.conf;
      ".config/alacritty/alacritty.yml".source = ./dots/alacritty.yml;
      ".config/htop/htoprc".source = ./dots/htoprc;
      ".config/mimeapps.list".source = ./dots/mimeapps.list;
      ".config/proj.conf".source = ./dots/proj.conf;
      ".config/cmus/custom.theme".source = ./dots/cmus/custom.theme;
      #".config/xmonad" = {
      #  source = ./dots/xmonad;
      #  recursive = true;
      #};
      ".config/eww" = {
          source = ./dots/eww;
          recursive = true;
      };
      # ".config/nvim" = {
      #   source = ./dots/nvim;
      #   recursive = true;
      # };
      ".config/doom" = {
        source = ./dots/doom;
        recursive = true;
      };
      ".config/zsh" = {
        source = ./dots/zsh;
        recursive = true;
      };
      ".config/x11" = {
        source = ./dots/x11;
        recursive = true;
      };
      ".config/tmux" = {
        source = ./dots/tmux;
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

  xsession.enable = true;
  xsession.numlock.enable = true;
  xsession.scriptPath = ".config/x11/xsession";
  xsession.profilePath = ".config/x11/xprofile";
  xsession.profileExtra = ''
  picom &
  setxkbmap us,cz ,qwerty -option 'ctrl:nocaps' 'grp:ctrls_toggle' &
  xcape -e 'Control_L=Escape' &
  xrdb .config/x11/xresources &
  xset r rate 400 60 &
  taffybarr &
  xrandr --output DP-0 --primary --output DP-2 --mode 1920x1200 --right-of DP-0
  eww open --screen DP-2 audio --restart &
  eww open --screen DP-2 left &
  nice xwinwrap -g 1920x1200+0+0 -b -s -fs -st -sp -ni -nf -ov -fdt -- mpv --loop -wid WID --really-quiet --framedrop=vo --no-audio --panscan="1.0" ~/my-stuff/Pictures/wallpapers/alonso.mp4 &
  '';
  xsession.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./dots/xmonad/xmonad.hs;
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
  programs.obs-studio.enable = true;
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
    plugins = [
      pkgs.vimPlugins.nvim-treesitter.withAllGrammars
    ];
  };
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
  };
  programs.zsh = {
      enable = true;
      initExtraFirst = builtins.readFile ./dots/.zshrc;
      dotDir = ".config/zsh";
      history = {
        path = "${config.xdg.dataHome}/zsh/zsh_history";
        save = 10000000;
      };
  };

  home.packages = with pkgs; [
    sqlite
    xournalpp
    droidcam opentrack
    taffybarr
    librsvg
    nsxiv
    piper
    git gh
    pcmanfm
    zathura
    obsidian
    openjdk8
    nvimpager
    alacritty
    prismlauncher
    docker-compose
    clang-tools gcc13
    chromium firefox
    tree-sitter nodejs
    yt-dlp
    gimp
    ripgrep fd eza fzf bat
    lutris airshipper oversteer mangohud wineWowPackages.stagingFull winetricks mono path-of-building
    jq imagemagick trash-cli
    man-pages man-pages-posix
    libqalculate qalculate-gtk
    numlockx xdo xdotool xorg.xkill
    xcape xorg.setxkbmap xorg.xrdb
    xorg.xset xclip xsel xorg.xprop
    (retroarch.override {
      cores = with libretro; [
        mgba
        citra
        snes9x
        nestopia
        gambatte
        mupen64plus
        beetle-psx-hw
        pcsx2
      ];
    }) duckstation pcsx2
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
    # TMUX
    tmux
    tmuxp
    tmux-sessionizer
    # EMACS
    sqlite
    pdf2svg
    gnuplot
    texlive.combined.scheme-full
    (aspellWithDicts (dicts: with dicts; [
      en
      en-computers
      en-science
    ]))
    # AUDIO/VIDEO
    mpv
    cmus
    ffmpeg
    spotify
    pamixer
    playerctl
    pulsemixer
    # RICING
    eww
    xwallpaper
    lxappearance
    picom
    # FONTs
    nerdfonts
    joypixels
    emacs-all-the-icons-fonts
    # LANGUAGEs
    go
    (haskell.packages.ghc98.ghcWithPackages (pkgs: with pkgs; [
      cabal-install
      haskell-language-server
      hlint
      fourmolu
      hoogle
    ]))
    opam
    lua5_4
    python3
    zigpkgs.master
    babashka clojure leiningen
    elmPackages.elm elmPackages.elm-test elmPackages.elm-live
    elixir
    # RUST
    rustup
    cargo-insta
    # BQN
    cbqn bqn386 bqnlsp
    # LSPs
    taplo
    gopls
    clojure-lsp
    sumneko-lua-language-server
    nodePackages_latest.bash-language-server
    nodePackages_latest.svelte-language-server
    nodePackages_latest.typescript-language-server
    vscode-langservers-extracted
    nodePackages_latest."@tailwindcss/language-server"
    elmPackages.elm-language-server
    elixir-ls
    # LINTers
    statix
    shellcheck
    # DEBUGers
    vscode-extensions.vadimcn.vscode-lldb
    # FORMATters
    stylua
    alejandra
    nodePackages.prettier
    elmPackages.elm-format
  ];

  fonts.fontconfig.enable = true;

  services.mpris-proxy.enable = true;
  services.playerctld.enable = true;
  services.emacs.enable = true;
  services.status-notifier-watcher.enable = true;
  services.redshift = {
    enable = true;
    latitude = 49.337669373657114;
    longitude = 18.004264670426636;
    temperature = {
      day = 5700;
      night = 3500;
    };
  };
}
