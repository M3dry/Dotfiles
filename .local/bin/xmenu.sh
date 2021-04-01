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

cat <<EOF | xmenu | sh &
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
	View
		1	dwmc viewex 0
		2	dwmc viewex 1
		3	dwmc viewex 2
		4	dwmc viewex 3
		5	dwmc viewex 4
		6	dwmc viewex 5
		7	dwmc viewex 6
		8	dwmc viewex 7
		9	dwmc viewex 8
	Toggle view
		1	dwmc toggleviewex 0
		2	dwmc toggleviewex 1
		3	dwmc toggleviewex 2
		4	dwmc toggleviewex 3
		5	dwmc toggleviewex 4
		6	dwmc toggleviewex 5
		7	dwmc toggleviewex 6
		8	dwmc toggleviewex 7
		9	dwmc toggleviewex 8
	Tag
		1	dwmc tagex 0
		2	dwmc tagex 1
		3	dwmc tagex 2
		4	dwmc tagex 3
		5	dwmc tagex 4
		6	dwmc tagex 5
		7	dwmc tagex 6
		8	dwmc tagex 7
		9	dwmc tagex 8
	Toggle tag
		1	dwmc toggletagex 0
		2	dwmc toggletagex 1
		3	dwmc toggletagex 2
		4	dwmc toggletagex 3
		5	dwmc toggletagex 4
		6	dwmc toggletagex 5
		7	dwmc toggletagex 6
		8	dwmc toggletagex 7
		9	dwmc toggletagex 8
	Quit dwm	dwmc quit 0
	Restart dwm	dwmc quit 1
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
	$volume	dmenu < /dev/null -p "$info" -fn "mononoki nerd font:pixelsize=19:antialias=true:autohint=true" | xargs -I {} cmus-remote -v '{}'%
	$status	[ "$status" = "Playing" ] && cmus-remote -u || cmus-remote -p
	$repeat	cmus-remote -C "toggle repeat_current"
	$shuffle	cmus-remote	-S

Lock		slock
Shutdown		loginctl poweroff
Reboot			loginctl reboot
EOF

# Applications
# 	IMG:./icons/web.png	Web Browser	firefox
# 	IMG:./icons/gimp.png	Image editor	gimp
# Terminal (xterm)	xterm
# Terminal (urxvt)	urxvt
# Terminal (st)		st
