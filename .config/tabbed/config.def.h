/* See LICENSE file for copyright and license details. */

/* appearance */
static const char font[]        = "mononoki Nerd Font Mono:size=9:antialias=true:autohint=true";
static const char* normbgcolor  = "#111111";
static const char* normfgcolor  = "#b7b7b7";
static const char* selbgcolor   = "#51afef";
static const char* selfgcolor   = "#000000";
static const char* urgbgcolor   = "#000000";
static const char* urgfgcolor   = "#ff0000";
static const char before[]      = "<";
static const char after[]       = ">";
static const char titletrim[]   = "...";
static const int  tabwidth      = 200;
static const Bool foreground    = True;
static       Bool urgentswitch  = False;

/*
 * Where to place a new tab when it is opened. When npisrelative is True,
 * then the current position is changed + newposition. If npisrelative
 * is False, then newposition is an absolute position.
 */
static int  newposition   = 0;
static Bool npisrelative  = False;

#define SETPROP(p) { \
        .v = (char *[]){ "/bin/sh", "-c", \
                "prop=\"`xwininfo -children -id $1 | grep '^     0x' |" \
                "sed -e's@^ *\\(0x[0-9a-f]*\\) \"\\([^\"]*\\)\".*@\\1 \\2@' |" \
                "xargs -0 printf %b | dmenu -l 10 -w $1`\" &&" \
                "xprop -id $1 -f $0 8s -set $0 \"$prop\"", \
                p, winid, NULL \
        } \
}

#define M ControlMask|Mod4Mask
#define C ControlMask
#define S ShiftMask
#define A Mod4Mask

static Key keys[] = {
	/* modifier             key           function     argument */
	{ M,                    XK_Return,    focusonce,   { 0 } },
	{ M,                    XK_Return,    spawn,       { 0 } },
	{ M,                    XK_l,         rotate,      { .i = +1 } },
	{ M,                    XK_h,         rotate,      { .i = -1 } },
	{ M,                    XK_j,         movetab,     { .i = -1 } },
	{ M,                    XK_k,         movetab,     { .i = +1 } },
	{ M,                    XK_Tab,       rotate,      { .i = 0 } },
	{ M,                    XK_grave,     spawn,       SETPROP("_TABBED_SELECT_TAB") },
	{ M,                    XK_1,         move,        { .i = 0 } },
	{ M,                    XK_2,         move,        { .i = 1 } },
	{ M,                    XK_3,         move,        { .i = 2 } },
	{ M,                    XK_4,         move,        { .i = 3 } },
	{ M,                    XK_5,         move,        { .i = 4 } },
	{ M,                    XK_6,         move,        { .i = 5 } },
	{ M,                    XK_7,         move,        { .i = 6 } },
	{ M,                    XK_8,         move,        { .i = 7 } },
	{ M,                    XK_9,         move,        { .i = 8 } },
	{ M,                    XK_0,         move,        { .i = 9 } },
	{ M,                    XK_q,         killclient,  { 0 } },
	{ M,                    XK_u,         focusurgent, { 0 } },
	{ M|S,                  XK_u,         toggle,      { .v = (void*) &urgentswitch } },
	{ C,                    XK_Super_L,     showbar,     { .i = 1 } },
	{ A,                    XK_Control_L, showbar,     { .i = 1 } },
};

static Key keyreleases[] = {
	/* modifier             key           function     argument */
	{ C|A,                  XK_Super_L,     showbar,     { .i = 0 } },
	{ C|A,                  XK_Control_L, showbar,     { .i = 0 } },

};
