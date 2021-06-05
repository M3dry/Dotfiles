#!/bin/sh

title=$(cmus-remote -Q | rg "title" | sed 's/tag title //')
album=$(cmus-remote -Q | rg "album " | sed 's/tag album //')
artist=$(cmus-remote -Q | rg "albumartist " | sed 's/tag albumartist //')
[ -z "$artist" ] && artist="No artist"
number=$(cmus-remote -Q | rg "tracknumber" | sed 's/tag tracknumber //')
musstat="$title/$album:$number"
[ "$musstat" = "/:" ] && musstat="Nothing is playing"
volume="VOL:$(cmus-remote -Q | rg "vol_right" | sed 's/set vol_right //')"
[ $(cmus-remote -C "set shuffle?" | sed "s/.*=// ; s/'//") = "true" ] && shuffle="Shuffle" || shuffle="No shuffle"
[ $(cmus-remote -C "set repeat_current?" | sed "s/.*=// ; s/'//") = "true" ] && repeat="Repeat" || repeat="No repeat"
[ $(cmus-remote -Q | rg "status" | sed "s/status //") = "playing" ] && status="Playing" || status="Paused"
info=$(echo "$artist - $title/$album:$number 🔈$volume% $shuffle$status$repeat")

cat <<EOF | xmenu "$@" | sh &
Applications
	Web browsers
		Firefox	firefox
		Chromium	chromium
	Discord	discord
	Spotify	spotify
	Calculator	qalculate-gtk
Terminals
	St	st
	Xterm	xterm
	Alacritty	alacritty
	Nvim	$TERMINAL nvim
Emacs
	Dashboard	emacsclient -c
	Dired	emacsclient -c -e '(dired nil)'
	Elfeed	emacsclient -c -e '(elfeed)'
	Ibuffer	emacsclient -c -e '(ibuffer)'

Dwm
	Layouts
		[]= Tile	dwmc setlayoutex 0
		(@) Spiral	dwmc setlayoutex 1
		><> Floating	dwmc setlayoutex 2
		[D] Deck	dwmc setlayoutex 3
		### Nrowgrid	dwmc setlayoutex 4
		TTT Bstack	dwmc setlayoutex 5
		|M| Centeredmaster	dwmc setlayoutex 6
		[M] Monocle	dwmc setlayoutex 7
		HHH Grid	dwmc setlayoutex 8
	Toggle bar	dwmc togglebar
	Toggle vacant	dwmc togglevacant
	Toggle padding	dwmc togglepadding
	Quit dwm	dwmc quit 0
	Restart dwm	dwmc quit 1
1
	View	dwmc viewex 0
	Toggle view	dwmc toggleviewex 0
	Tag	dwmc tagex 0
	Tag with	dwmc tagwithex 0
	Toggle tag	dwmc toggletagex 0
2
	View	dwmc viewex 1
	Toggle view	dwmc toggleviewex 1
	Tag	dwmc tagex 1
	Tag with	dwmc tagwithex 1
	Toggle tag	dwmc toggletagex 1
3
	View	dwmc viewex 2
	Toggle view	dwmc toggleviewex 2
	Tag	dwmc tagex 2
	Tag with	dwmc tagwithex 2
	Toggle tag	dwmc toggletagex 2
4
	View	dwmc viewex 3
	Toggle view	dwmc toggleviewex 3
	Tag	dwmc tagex 3
	Tag with	dwmc tagwithex 3
	Toggle tag	dwmc toggletagex 3
5
	View	dwmc viewex 4
	Toggle view	dwmc toggleviewex 4
	Tag	dwmc tagex 4
	Tag with	dwmc tagwithex 4
	Toggle tag	dwmc toggletagex 4
6
	View	dwmc viewex 5
	Toggle view	dwmc toggleviewex 5
	Tag	dwmc tagex 5
	Tag with	dwmc tagwithex 5
	Toggle tag	dwmc toggletagex 5
7
	View	dwmc viewex 6
	Toggle view	dwmc toggleviewex 6
	Tag	dwmc tagex 6
	Tag with	dwmc tagwithex 6
	Toggle tag	dwmc toggletagex 6
8
	View	dwmc viewex 7
	Toggle view	dwmc toggleviewex 7
	Tag	dwmc tagex 7
	Tag with	dwmc tagwithex 7
	Toggle tag	dwmc toggletagex 7
9
	View	dwmc viewex 8
	Toggle view	dwmc toggleviewex 8
	Tag	dwmc tagex 8
	Tag with	dwmc tagwithex 8
	Toggle tag	dwmc toggletagex 8
Dmenu scripts
	Dmenu	dmenu_run -l 5 -g 10 -p 'Run:'
	Volume	volume-script
	Edit configs	Booky 'emacsclient -c -a emacs' '><' 'Cconfig'
	Bookmarks	Booky 'firefox' ':' 'Bconfig'
	Music	music-changer cmus
	Switch	switch
	Emojis	emoji-script
	Calculator	calc
	Passmenu	passmenu2 -F -p 'Passwords:'
	Man pages	manview
	Allmenu	allemnu
	Shut	shut

Cmus
	$artist	music-changer queue
	$musstat	music-changer play
	$volume	dmenu < /dev/null -p "$info" -h 24 | xargs -I {} cmus-remote -v '{}'%
	$status	[ "$status" = "Playing" ] && cmus-remote -u || cmus-remote -p
	$repeat	cmus-remote -C "toggle repeat_current"
	$shuffle	cmus-remote	-S

Lock		slock
Shutdown		loginctl poweroff
Reboot			loginctl reboot
EOF
