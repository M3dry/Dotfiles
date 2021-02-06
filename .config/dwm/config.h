static const unsigned int borderpx       = 2;   /* border pixel of windows */
static const unsigned int borderpxB      = 0;   /* border pixel of bar */
static const unsigned int snap           = 0;   /* snap pixel */
static const int swallowfloating         = 1;   /* 1 means swallow floating windows by default */
static const unsigned int gappih         = 3;   /* horiz inner gap between windows */
static const unsigned int gappiv         = 3;   /* vert inner gap between windows */
static const unsigned int gappoh         = 3;   /* horiz outer gap between windows and screen edge */
static const unsigned int gappov         = 3;   /* vert outer gap between windows and screen edge */
static const int smartgaps               = 1;   /* 1 means no outer gap when there is only one window */
static const int showbar                 = 1;   /* 0 means no bar */
static const int topbar                  = 1;   /* 0 means bottom bar */
static const int bar_height              = 20;  /* 0 means derive from font, >= 1 explicit height */
static const int vertpad                 = 0;   /* vertical padding of bar */
static const int sidepad                 = 0;   /* horizontal padding of bar */
static const int focusonwheel            = 0;
static int floatposgrid_x                = 5;   /* float grid columns */
static int floatposgrid_y                = 5;   /* float grid rows */
static const unsigned int systrayspacing = 2;   /* systray spacing */
static const int showsystray             = 1;   /* 0 means no systray */
static const char buttonbar[]            = "";
static int tagindicatortype              = INDICATOR_BOTTOM_BAR;
static int tiledindicatortype            = INDICATOR_NONE;
static int floatindicatortype            = INDICATOR_NONE;
static int fakefsindicatortype           = INDICATOR_TOP_LEFT_SQUARE;
static int floatfakefsindicatortype      = INDICATOR_PLUS;
static const char *fonts[]               = {"mononoki Nerd Font Mono:size=9:antialias=true:autohint=true",
                                            "JoyPixels:size=8:antialias=true:autohint=true"};

static char c000000[]                    = "#000000"; // placeholder value
static char normfgcolor[]                = "#ffffff";
static char normbgcolor[]                = "#000000";
static char normbordercolor[]            = "#292d3e";
static char normfloatcolor[]             = "#292d3e";

static char selfgcolor[]                 = "#ffffff";
static char selbgcolor[]                 = "#51afef";
static char selbordercolor[]             = "#111111";
static char selfloatcolor[]              = "#ecbe7b";

static char titlenormfgcolor[]           = "#f0f0f0";
static char titlenormbgcolor[]           = "#292d3e";
static char titlenormbordercolor[]       = "#292d3e";
static char titlenormfloatcolor[]        = "#292d3e";

static char titleselfgcolor[]            = "#ffffff";
static char titleselbgcolor[]            = "#f07178";
static char titleselbordercolor[]        = "#292d3e";
static char titleselfloatcolor[]         = "#51afef";

static char tagsnormfgcolor[]            = "#f0f0f0";
static char tagsnormbgcolor[]            = "#000000";
static char tagsnormbordercolor[]        = "#292d3e";
static char tagsnormfloatcolor[]         = "#292d3e";

static char tagsselfgcolor[]             = "#f0f0f0";
static char tagsselbgcolor[]             = "#51afef";
static char tagsselbordercolor[]         = "#292d3e";
static char tagsselfloatcolor[]          = "#51afef";

static char hidfgcolor[]                 = "#000000";
static char hidbgcolor[]                 = "#f07178";
static char hidbordercolor[]             = "#f07178";
static char hidfloatcolor[]              = "#f07178";

static char urgfgcolor[]                 = "#f0f0f0";
static char urgbgcolor[]                 = "#222222";
static char urgbordercolor[]             = "#ff0000";
static char urgfloatcolor[]              = "#db8fd9";

static char normTTBbgcolor[]             = "#111111";
static char normLTRbgcolor[]             = "#111111";
static char normMONObgcolor[]            = "#111111";
static char normGRIDbgcolor[]            = "#111111";
static char normGRD1bgcolor[]            = "#111111";
static char normGRD2bgcolor[]            = "#111111";
static char normGRDMbgcolor[]            = "#51afef";
static char normHGRDbgcolor[]            = "#51afef";
static char normDWDLbgcolor[]            = "#111111";
static char normSPRLbgcolor[]            = "#292d3e";
static char normfloatbgcolor[]           = "#292d3e";
static char actTTBbgcolor[]              = "#292d3e";
static char actLTRbgcolor[]              = "#292d3e";
static char actMONObgcolor[]             = "#292d3e";
static char actGRIDbgcolor[]             = "#292d3e";
static char actGRD1bgcolor[]             = "#292d3e";
static char actGRD2bgcolor[]             = "#292d3e";
static char actGRDMbgcolor[]             = "#292d3e";
static char actHGRDbgcolor[]             = "#292d3e";
static char actDWDLbgcolor[]             = "#292d3e";
static char actSPRLbgcolor[]             = "#292d3e";
static char actfloatbgcolor[]            = "#292d3e";
static char selTTBbgcolor[]              = "#51afef";
static char selLTRbgcolor[]              = "#51afef";
static char selMONObgcolor[]             = "#51afef";
static char selGRIDbgcolor[]             = "#51afef";
static char selGRD1bgcolor[]             = "#51afef";
static char selGRD2bgcolor[]             = "#51afef";
static char selGRDMbgcolor[]             = "#51afef";
static char selHGRDbgcolor[]             = "#51afef";
static char selDWDLbgcolor[]             = "#51afef";
static char selSPRLbgcolor[]             = "#51afef";
static char selfloatbgcolor[]            = "#ecbe7b";


static char *colors[][ColCount] = {
	/*                       fg                bg                border                float */
	[SchemeNorm]         = { normfgcolor,      normbgcolor,      normbordercolor,      normfloatcolor },
	[SchemeSel]          = { selfgcolor,       selbgcolor,       selbordercolor,       selfloatcolor },
	[SchemeTitleNorm]    = { titlenormfgcolor, titlenormbgcolor, titlenormbordercolor, titlenormfloatcolor },
	[SchemeTitleSel]     = { titleselfgcolor,  titleselbgcolor,  titleselbordercolor,  titleselfloatcolor },
	[SchemeTagsNorm]     = { tagsnormfgcolor,  tagsnormbgcolor,  tagsnormbordercolor,  tagsnormfloatcolor },
	[SchemeTagsSel]      = { tagsselfgcolor,   tagsselbgcolor,   tagsselbordercolor,   tagsselfloatcolor },
	[SchemeHid]          = { hidfgcolor,       hidbgcolor,       hidbordercolor,       hidfloatcolor },
	[SchemeUrg]          = { urgfgcolor,       urgbgcolor,       urgbordercolor,       urgfloatcolor },
	[SchemeFlexActTTB]   = { titleselfgcolor,  actTTBbgcolor,    actTTBbgcolor,        c000000 },
	[SchemeFlexActLTR]   = { titleselfgcolor,  actLTRbgcolor,    actLTRbgcolor,        c000000 },
	[SchemeFlexActMONO]  = { titleselfgcolor,  actMONObgcolor,   actMONObgcolor,       c000000 },
	[SchemeFlexActGRID]  = { titleselfgcolor,  actGRIDbgcolor,   actGRIDbgcolor,       c000000 },
	[SchemeFlexActGRD1]  = { titleselfgcolor,  actGRD1bgcolor,   actGRD1bgcolor,       c000000 },
	[SchemeFlexActGRD2]  = { titleselfgcolor,  actGRD2bgcolor,   actGRD2bgcolor,       c000000 },
	[SchemeFlexActGRDM]  = { titleselfgcolor,  actGRDMbgcolor,   actGRDMbgcolor,       c000000 },
	[SchemeFlexActHGRD]  = { titleselfgcolor,  actHGRDbgcolor,   actHGRDbgcolor,       c000000 },
	[SchemeFlexActDWDL]  = { titleselfgcolor,  actDWDLbgcolor,   actDWDLbgcolor,       c000000 },
	[SchemeFlexActSPRL]  = { titleselfgcolor,  actSPRLbgcolor,   actSPRLbgcolor,       c000000 },
	[SchemeFlexActFloat] = { titleselfgcolor,  actfloatbgcolor,  actfloatbgcolor,      c000000 },
	[SchemeFlexInaTTB]   = { titlenormfgcolor, normTTBbgcolor,   normTTBbgcolor,       c000000 },
	[SchemeFlexInaLTR]   = { titlenormfgcolor, normLTRbgcolor,   normLTRbgcolor,       c000000 },
	[SchemeFlexInaMONO]  = { titlenormfgcolor, normMONObgcolor,  normMONObgcolor,      c000000 },
	[SchemeFlexInaGRID]  = { titlenormfgcolor, normGRIDbgcolor,  normGRIDbgcolor,      c000000 },
	[SchemeFlexInaGRD1]  = { titlenormfgcolor, normGRD1bgcolor,  normGRD1bgcolor,      c000000 },
	[SchemeFlexInaGRD2]  = { titlenormfgcolor, normGRD2bgcolor,  normGRD2bgcolor,      c000000 },
	[SchemeFlexInaGRDM]  = { titlenormfgcolor, normGRDMbgcolor,  normGRDMbgcolor,      c000000 },
	[SchemeFlexInaHGRD]  = { titlenormfgcolor, normHGRDbgcolor,  normHGRDbgcolor,      c000000 },
	[SchemeFlexInaDWDL]  = { titlenormfgcolor, normDWDLbgcolor,  normDWDLbgcolor,      c000000 },
	[SchemeFlexInaSPRL]  = { titlenormfgcolor, normSPRLbgcolor,  normSPRLbgcolor,      c000000 },
	[SchemeFlexInaFloat] = { titlenormfgcolor, normfloatbgcolor, normfloatbgcolor,     c000000 },
	[SchemeFlexSelTTB]   = { titleselfgcolor,  selTTBbgcolor,    selTTBbgcolor,        c000000 },
	[SchemeFlexSelLTR]   = { titleselfgcolor,  selLTRbgcolor,    selLTRbgcolor,        c000000 },
	[SchemeFlexSelMONO]  = { titleselfgcolor,  selMONObgcolor,   selMONObgcolor,       c000000 },
	[SchemeFlexSelGRID]  = { titleselfgcolor,  selGRIDbgcolor,   selGRIDbgcolor,       c000000 },
	[SchemeFlexSelGRD1]  = { titleselfgcolor,  selGRD1bgcolor,   selGRD1bgcolor,       c000000 },
	[SchemeFlexSelGRD2]  = { titleselfgcolor,  selGRD2bgcolor,   selGRD2bgcolor,       c000000 },
	[SchemeFlexSelGRDM]  = { titleselfgcolor,  selGRDMbgcolor,   selGRDMbgcolor,       c000000 },
	[SchemeFlexSelHGRD]  = { titleselfgcolor,  selHGRDbgcolor,   selHGRDbgcolor,       c000000 },
	[SchemeFlexSelDWDL]  = { titleselfgcolor,  selDWDLbgcolor,   selDWDLbgcolor,       c000000 },
	[SchemeFlexSelSPRL]  = { titleselfgcolor,  selSPRLbgcolor,   selSPRLbgcolor,       c000000 },
	[SchemeFlexSelFloat] = { titleselfgcolor,  selfloatbgcolor,  selfloatbgcolor,      c000000 },
};

static const char *const autostart[] = {
    "zsh", "-c", "picom", NULL,
    "zsh", "-c", "dwmblocks", NULL,
    "zsh", "-c", "numlockx on", NULL,
    "zsh", "-c", "xwallpaper --stretch my-stuff/pictures/dwm-unix.png", NULL,
    "zsh", "-c", "/usr/bin/emacs --daemon", NULL,
    "zsh", "-c", "redshift", NULL,
    "zsh", "-c", "unclutter --timeout 5", NULL,
    "zsh", "-c", "dunst", NULL,
    "zsh", "-c", "playerctld", NULL,
    "zsh", "-c", "clipmenud", NULL,
    "zsh", "-c", "eww daemon", NULL,
    "zsh", "-c", "setxkbmap us,cz ,qwerty -option 'caps:escape' 'grp:shifts_toggle'", NULL,
    "zsh", "-c", "lxsession", NULL,
    "zsh", "-c", "xrdb .config/x11/xresources", NULL,
    "zsh", "-c", "sleep 3 && eww open status", NULL,
    NULL /* terminate */
};

const char *spcmd1[] = {"st", "-n", "spterm", "-t", "stSCP", "-g", "144x41", NULL };
const char *spcmd2[] = {"st", "-n", "spmus", "-t", "cmusSCP", "-g", "144x41", "-e", "cmus", NULL };
const char *spcmd3[] = {"qalculate-gtk", "--title", "spcal", NULL };
static Sp scratchpads[] = {
   /* name          cmd  */
   {"spterm",      spcmd1},
   {"spmus",       spcmd2},
   {"spcal",       spcmd3},
};

/* Tags
 * In a traditional dwm the number of tags in use can be changed simply by changing the number
 * of strings in the tags array. This build does things a bit different which has some added
 * benefits. If you need to change the number of tags here then change the NUMTAGS macro in dwm.c.
 *
 * Examples:
 *
 *  1) static char *tagicons[][NUMTAGS*2] = {
 *         [DEFAULT_TAGS] = { "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I" },
 *     }
 *
 *  2) static char *tagicons[][1] = {
 *         [DEFAULT_TAGS] = { "•" },
 *     }
 *
 * The first example would result in the tags on the first monitor to be 1 through 9, while the
 * tags for the second monitor would be named A through I. A third monitor would start again at
 * 1 through 9 while the tags on a fourth monitor would also be named A through I. Note the tags
 * count of NUMTAGS*2 in the array initialiser which defines how many tag text / icon exists in
 * the array. This can be changed to *3 to add separate icons for a third monitor.
 *
 * For the second example each tag would be represented as a bullet point. Both cases work the
 * same from a technical standpoint - the icon index is derived from the tag index and the monitor
 * index. If the icon index is is greater than the number of tag icons then it will wrap around
 * until it an icon matches. Similarly if there are two tag icons then it would alternate between
 * them. This works seamlessly with alternative tags and alttagsdecoration patches.
 */
static char *tagicons[][NUMTAGS] = {
	[DEFAULT_TAGS]        = { "1", "2", "3", "4", "5", "6", "7", "8", "9" },
};

/* grid of tags */
#define SWITCHTAG_UP                1 << 0
#define SWITCHTAG_DOWN              1 << 1
#define SWITCHTAG_LEFT              1 << 2
#define SWITCHTAG_RIGHT             1 << 3
#define SWITCHTAG_TOGGLETAG         1 << 4
#define SWITCHTAG_TAG               1 << 5
#define SWITCHTAG_VIEW              1 << 6
#define SWITCHTAG_TOGGLEVIEW        1 << 7

static const int tagrows = 2;

/* There are two options when it comes to per-client rules:
 *  - a typical struct table or
 *  - using the RULE macro
 *
 * A traditional struct table looks like this:
 *    // class      instance  title  wintype  tags mask  isfloating  monitor
 *    { "Gimp",     NULL,     NULL,  NULL,    1 << 4,    0,          -1 },
 *    { "Firefox",  NULL,     NULL,  NULL,    1 << 7,    0,          -1 },
 *
 * The RULE macro has the default values set for each field allowing you to only
 * specify the values that are relevant for your rule, e.g.
 *
 *    RULE(.class = "Gimp", .tags = 1 << 4)
 *    RULE(.class = "Firefox", .tags = 1 << 7)
 *
 * Refer to the Rule struct definition for the list of available fields depending on
 * the patches you enable.
 */
static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 *	WM_WINDOW_ROLE(STRING) = role
	 *	_NET_WM_WINDOW_TYPE(ATOM) = wintype
	 */
	RULE(.wintype = WTYPE "DIALOG", .isfloating = 1)
	RULE(.wintype = WTYPE "UTILITY", .isfloating = 1)
	RULE(.wintype = WTYPE "TOOLBAR", .isfloating = 1)
	RULE(.wintype = WTYPE "SPLASH", .isfloating = 1)
	RULE(.class = "Brave", .tags = 1)
	// Swallow
	RULE(.instance = "st", .isterminal = 1)
	RULE(.class = "Alacritty", .isterminal = 1)
	RULE(.class = "XTerm", .isterminal = 1)
	// noSwallow
	RULE(.title = "Event Tester", .noswallow = 1)
	RULE(.class = "Xephyr", .noswallow = 1, .isfloating = 1, .iscentered = 1)
	RULE(.class = "Gimp", .tags = 1 << 8, .isfloating = 1, .switchtag = 3, .noswallow = 1, .floatpos = "50% 50% 100% 100%")
	// Scratchpads
	RULE(.instance = "spterm", .tags = SPTAG(0), .isfloating = 1, .iscentered = 1)
	RULE(.instance = "spmus", .tags = SPTAG(1), .isfloating = 1, .iscentered = 1)
	RULE(.title = "spcal", .tags = SPTAG(2), .isfloating = 1, .iscentered = 1, .floatpos = "50% 50% 50% 50%")
};

static const MonitorRule monrules[] = {
	/* monitor  tag   layout  mfact  nmaster  showbar  topbar */
	{  1,       -1,   2,      -1,    -1,      -1,      -1     }, // use a different layout for the second monitor
	{  -1,      -1,   0,      -1,    -1,      -1,      -1     }, // default
};


/* Bar rules allow you to configure what is shown where on the bar, as well as
 * introducing your own bar modules.
 *
 *    monitor:
 *      -1  show on all monitors
 *       0  show on monitor 0
 *      'A' show on active monitor (i.e. focused / selected) (or just -1 for active?)
 *    bar - bar index, 0 is default, 1 is extrabar
 *    alignment - how the module is aligned compared to other modules
 *    widthfunc, drawfunc, clickfunc - providing bar module width, draw and click functions
 *    name - does nothing, intended for visual clue and for logging / debugging
 */
static const BarRule barrules[] = {
	/* monitor  bar    alignment         widthfunc                drawfunc                clickfunc                name */
	{ -1,       0,     BAR_ALIGN_LEFT,   width_tags,              draw_tags,              click_tags,              "tags" },
	{ -1,       0,     BAR_ALIGN_RIGHT,  width_systray,           draw_systray,           click_systray,           "systray" },
	{ -1,       0,     BAR_ALIGN_RIGHT,  width_status2d,          draw_status2d,          click_statuscmd,         "status2d" },
	{ -1,       0,     BAR_ALIGN_RIGHT,  width_taggrid,           draw_taggrid,           click_taggrid,           "taggrid" },
	{ -1,       0,     BAR_ALIGN_LEFT,   width_ltsymbol,          draw_ltsymbol,          click_ltsymbol,          "layout" },
	{ -1,       0,     BAR_ALIGN_LEFT,   width_flexwintitle,      draw_flexwintitle,      click_flexwintitle,      "flexwintitle" },
	{ -1,       0,     BAR_ALIGN_LEFT,   width_wintitle_hidden,   draw_wintitle_hidden,   click_wintitle_hidden,   "wintitle_hidden" },
};

/* layout(s) */
static const float mfact     = 0.5; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int nstack      = 0;    /* number of clients in primary stack area */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */
#define FORCE_VSPLIT 1

static const Layout layouts[] = {
	/* symbol     arrange function, { nmaster, nstack, layout, master axis, stack axis, secondary stack axis, symbol func } */
	{ "[]=",      flextile,         { -1, -1, SPLIT_VERTICAL, TOP_TO_BOTTOM, TOP_TO_BOTTOM, 0, NULL } }, // default tile layout
	{ "(@)",      flextile,         { -1, -1, SPLIT_VERTICAL, 0, SPIRAL, 0, NULL } }, // fibonacci spiral
 	{ "><>",      NULL,             {0} },    /* no layout function means floating behavior */
	{ "[D]",      flextile,         { -1, -1, SPLIT_VERTICAL, TOP_TO_BOTTOM, MONOCLE, 0, NULL } }, // deck
	{ "[::",      flextile,         { -1, -1, SPLIT_VERTICAL, GAPPLESSGRID, GAPPLESSGRID, 0, NULL } }, // gappless grid
	{ ":::",      nrowgrid, }, // nrowgrid grid
	{ "TTT",      flextile,         { -1, -1, SPLIT_HORIZONTAL, LEFT_TO_RIGHT, LEFT_TO_RIGHT, 0, NULL } }, // bstack
	{ "|M|",      flextile,         { -1, -1, SPLIT_CENTERED_VERTICAL, TOP_TO_BOTTOM, TOP_TO_BOTTOM, TOP_TO_BOTTOM, NULL } }, // centeredmaster
	{ "[M]",      flextile,         { -1, -1, NO_SPLIT, MONOCLE, MONOCLE, 0, NULL } }, // monocle
	{ NULL,       NULL,             {0} },
};

/* key definitions */
#define M Mod4Mask // modkey
#define S ShiftMask // shift
#define C ControlMask // ctrl
#define A Mod1Mask // alt
#define Agr Mod3Mask // altGr
#define Sgr Mod5Mask // shiftGr

#define TAGKEYS(KEY,TAG) \
	{ A,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ C,                       KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ M,                       KEY,      toggletag,      {.ui = 1 << TAG} }, \
	{ A|S,                     KEY,      tag,            {.ui = 1 << TAG} }, \
	{ C|A,                     KEY,      tagwith,        {.ui = 1 << TAG} }, \
	{ M|S,                     KEY,      swaptags,       {.ui = 1 << TAG} }, \
	{ A|M,                     KEY,      tagnextmon,     {.ui = 1 << TAG} }, \
	{ A|M|S,                   KEY,      tagprevmon,     {.ui = 1 << TAG} },



/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

#include <X11/XF86keysym.h>
static Key on_empty_keys[] = {
	/* modifier key            function                argument */
	{ 0,        XK_w,          spawn,                  SHCMD("$BROWSER") },
};

static Key keys[] = {
	/* modifier                     key            function                argument */
  /* Spawing preograms*/
	{ A,                       XK_Return,     spawn,                  SHCMD("$TERMINAL") },
	{ A|S,                     XK_c,          spawn,                  SHCMD("st htop") },
	{ A|S,                     XK_z,          spawn,                  SHCMD("playerctl play-pause") },
	{ A,                       XK_e,          spawn,                  SHCMD("$TERMINAL $EDITOR") },
	{ M,                       XK_e,          spawn,                  SHCMD("emacsclient -c -a emacs") },
	{ M|S,                     XK_c,          spawn,                  SHCMD("emacsclient -c -a e'macs' --eval '(ibuffer)'") },
	{ A,                       XK_w,          spawn,                  SHCMD("$BROWSER") },
	{ A|C,                     XK_w,          spawn,                  SHCMD("$BROWSER youtube.com https://fosstodon.org/about twitch.tv https://mail.protonmail.com/login") },
	{ A,                       XK_b,          spawn,                  SHCMD("freetube") },
	{ A,                       XK_o,          spawn,                  SHCMD("chromium") },
	{ A,                       XK_Escape,     spawn,                  SHCMD("xkill") },
	{ C|A,                     XK_d,          spawn,                  SHCMD("discord") },
	{ A|S,                     XK_u,          spawn,                  SHCMD("scrot '%Y-%m-%d_%H:%M.png' -e 'mv $f ~/my-stuff/pictures/snips/'") },
	{ A,                       XK_p,          spawn,                  SHCMD("pcmanfm") },
	{ A,                       XK_a,          spawn,                  SHCMD("$TERMINAL vifmrun") },
	{ C,                       XK_m,          spawn,                  SHCMD("multimc") },
	{ M|C|A,                   XK_l,          spawn,                  SHCMD("slock") },
	{ C|A,                     XK_z,          spawn,                  SHCMD("playerctl play-pause") },
  /* Dmenu scripts */
	{ A|S,                     XK_Return,     spawn,                  SHCMD("dmenu_run -l 5 -g 10 -p 'Run:'") },
	{ A|S,                     XK_s,          spawn,                  SHCMD("switch") },
	{ A,                       XK_c,          spawn,                  SHCMD("volume-script") },
	{ A|C,                     XK_Return,     spawn,                  SHCMD("Booky 'st nvim' '><'") },
	{ A|S,                     XK_w,          spawn,                  SHCMD("Booky 'librewolf' ':'") },
	{ A|S,                     XK_e,          spawn,                  SHCMD("emoji-script") },
	{ A|S,                     XK_d,          spawn,                  SHCMD("calc") },
	{ A|S,                     XK_v,          spawn,                  SHCMD("manview") },
	{ A,                       XK_z,          spawn,                  SHCMD("music-changer cmus") },
	{ A|S,                     XK_p,          spawn,                  SHCMD("passmenu2 -F -p 'Passwords:'") },
	{ A|S,                     XK_a,          spawn,                  SHCMD("allmenu") },
	{ A|C,                     XK_q,          spawn,                  SHCMD("shut") },
	{ A|C,                     XK_c,          spawn,                  SHCMD("clipmenu -F -l 20 -g 5 -p 'Copy: '") },
  /* MultiMedia keys */
	{ 0, XF86XK_AudioPrev,     spawn,                  SHCMD("playerctl --player cmus previous") },
	{ 0, XF86XK_AudioNext,     spawn,                  SHCMD("playerctl --player cmus next") },
	{ 0, XF86XK_AudioPlay,     spawn,                  SHCMD("playerctl --player cmus play-pause") },
	{ 0, XF86XK_AudioLowerVolume, spawn,               SHCMD("pamixer --allow-boost -d 1 ; pkill -rtmin+7 dwmblocks") },
	{ 0, XF86XK_AudioRaiseVolume, spawn,               SHCMD("pamixer --allow-boost -i 1 ; pkill -rtmin+7 dwmblocks") },
  /* DWM keybindings */
	{ A,                       XK_F5,         xrdb,                   {.v = NULL } },
	{ M|S,                     XK_u,          setkeymode,             {.ui = COMMANDMODE} },
	{ A,                       XK_n,          togglebar,              {0} },
	{ C|A,                     XK_space,      focusmaster,            {0} },
	{ A,                       XK_s,          swapfocus,              {.i = -1 } },
	{ A|S,                     XK_h,          switchcol,              {0} },
	{ A,                       XK_apostrophe, zoom,                   {0} },
	{ A,                       XK_j,          focusstack,             {.i = +2 } },
	{ A,                       XK_k,          focusstack,             {.i = -2 } },
	{ M|A,                     XK_j,          focusstack,             {.i = +1 } },
	{ M|A,                     XK_k,          focusstack,             {.i = -1 } },
	{ M,                       XK_z,          showhideclient,         {0} },
	{ C,                       XK_j,          pushdown,               {0} },
	{ C,                       XK_k,          pushup,                 {0} },
	{ A,                       XK_h,          setmfact,               {.f = -0.05} },
	{ A,                       XK_l,          setmfact,               {.f = +0.05} },
	{ A|S,                     XK_j,          setcfact,               {.f = +0.25} },
	{ A|S,                     XK_k,          setcfact,               {.f = -0.25} },
	{ A|S,                     XK_u,          setcfact,               {0} },
	{ A,                       XK_bracketleft,incnmaster,             {.i = +1 } },
	{ A,                       XK_bracketright,incnmaster,            {.i = -1 } },
	{ M|C,                     XK_bracketleft,incnstack,              {.i = +1 } },
	{ M|C,                     XK_bracketright,incnstack,             {.i = -1 } },
	{ A,                       XK_Down,       moveresize,             {.v = "0x 25y 0w 0h" } },
	{ A,                       XK_Up,         moveresize,             {.v = "0x -25y 0w 0h" } },
	{ A,                       XK_Right,      moveresize,             {.v = "25x 0y 0w 0h" } },
	{ A,                       XK_Left,       moveresize,             {.v = "-25x 0y 0w 0h" } },
	{ A|S,                     XK_Down,       moveresize,             {.v = "0x 0y 0w 25h" } },
	{ A|S,                     XK_Up,         moveresize,             {.v = "0x 0y 0w -25h" } },
	{ A|S,                     XK_Right,      moveresize,             {.v = "0x 0y 25w 0h" } },
	{ A|S,                     XK_Left,       moveresize,             {.v = "0x 0y -25w 0h" } },
	{ M,                       XK_x,          transfer,               {0} },
	{ M|C,                     XK_x,          reorganizetags,         {0} },
	{ M|S,                     XK_equal,      incrgaps,               {.i = +1 } },
	{ M|S,                     XK_minus,      incrgaps,               {.i = -1 } },
	{ M|S,                     XK_0,          togglegaps,             {0} },
	{ M|C,                     XK_0,          defaultgaps,            {0} },
	{ A,                       XK_Tab,        view,                   {0} },
	{ A,                       XK_q,          killclient,             {0} },
	{ A|S,                     XK_q,          killunsel,              {0} },
	{ M|S,                     XK_Escape,     quit,                   {0} },
	{ M|C|S,                   XK_q,          quit,                   {1} },
	/* Layouts */
	{ A,                       XK_t,          setlayout,              {.v = &layouts[0]} },
	{ A,                       XK_v,          setlayout,              {.v = &layouts[1]} },
	{ A|S,                     XK_f,          setlayout,              {.v = &layouts[2]} },
	{ A,                       XK_d,          setlayout,              {.v = &layouts[3]} },
	{ A,                       XK_g,          setlayout,              {.v = &layouts[4]} },
	{ A|S,                     XK_g,          setlayout,              {.v = &layouts[5]} },
	{ A|S,                     XK_b,          setlayout,              {.v = &layouts[6]} },
	{ A|S,                     XK_m,          setlayout,              {.v = &layouts[7]} },
	{ A,                       XK_m,          setlayout,              {.v = &layouts[8]} },

	{ A,                       XK_space,      togglefloating,         {0} },
	{ A,                       XK_u,          togglescratch,          {.ui = 0 } },
    { A,                       XK_i,          togglescratch,          {.ui = 1 } },
    { A,                       XK_y,          togglescratch,          {.ui = 2 } },
	{ A|S,                     XK_space,      unfloatvisible,         {0} },
	{ A,                       XK_f,          togglefullscreen,       {0} },
	{ A|C,                     XK_f,          togglefakefullscreen,   {0} },
	{ C,                       XK_s,          togglesticky,           {0} },
	{ M,                       XK_comma,      focusmon,               {.i = -1 } },
	{ M,                       XK_period,     focusmon,               {.i = +1 } },
	{ M|S,                     XK_comma,      tagmon,                 {.i = -1 } },
	{ M|S,                     XK_period,     tagmon,                 {.i = +1 } },
	{ M|A|C,                   XK_comma,      tagswapmon,             {.i = +1 } },
	{ M|A|C,                   XK_period,     tagswapmon,             {.i = -1 } },
	{ M|C,                     XK_Left,       viewtoleft,             {0} },
	{ M|C,                     XK_Right,      viewtoright,            {0} },
	{ M|A,                     XK_Left,       tagandviewtoleft,       {0} },
	{ M|A,                     XK_Right,      tagandviewtoright,      {0} },
	{ M,                       XK_semicolon,  tagtoleft,              {0} },
	{ M,                       XK_apostrophe, tagtoright,             {0} },
	{ M,                       XK_Up,         switchtag,              { .ui = SWITCHTAG_UP    | SWITCHTAG_VIEW } },
	{ M,                       XK_Down,       switchtag,              { .ui = SWITCHTAG_DOWN  | SWITCHTAG_VIEW } },
	{ M,                       XK_Right,      switchtag,              { .ui = SWITCHTAG_RIGHT | SWITCHTAG_VIEW } },
	{ M,                       XK_Left,       switchtag,              { .ui = SWITCHTAG_LEFT  | SWITCHTAG_VIEW } },
	{ M|S,                     XK_Up,         switchtag,              { .ui = SWITCHTAG_UP    | SWITCHTAG_TAG | SWITCHTAG_VIEW } },
	{ M|S,                     XK_Down,       switchtag,              { .ui = SWITCHTAG_DOWN  | SWITCHTAG_TAG | SWITCHTAG_VIEW } },
	{ M|S,                     XK_Right,      switchtag,              { .ui = SWITCHTAG_RIGHT | SWITCHTAG_TAG | SWITCHTAG_VIEW } },
	{ M|S,                     XK_Left,       switchtag,              { .ui = SWITCHTAG_LEFT  | SWITCHTAG_TAG | SWITCHTAG_VIEW } },
	/* Note that due to key limitations the below example kybindings are defined with a Mod3Mask,
	 * which is not always readily available. Refer to the patch wiki for more details. */
	/* Client position is limited to monitor window area */
	// { Agr,               XK_u,           floatpos,               {.v = "-26x -26y" } }, // ↖
	// { Agr,               XK_i,           floatpos,               {.v = "  0x -26y" } }, // ↑
	// { Agr,               XK_o,           floatpos,               {.v = " 26x -26y" } }, // ↗
	// { Agr,               XK_j,           floatpos,               {.v = "-26x   0y" } }, // ←
	// { Agr,               XK_l,           floatpos,               {.v = " 26x   0y" } }, // →
	// { Agr,               XK_m,           floatpos,               {.v = "-26x  26y" } }, // ↙
	// { Agr,               XK_comma,       floatpos,               {.v = "  0x  26y" } }, // ↓
	// { Agr,               XK_period,      floatpos,               {.v = " 26x  26y" } }, // ↘
	// /* Absolute positioning (allows moving windows between monitors) */
	// { Agr|C,             XK_u,           floatpos,               {.v = "-26a -26a" } }, // ↖
	// { Agr|C,             XK_i,           floatpos,               {.v = "  0a -26a" } }, // ↑
	// { Agr|C,             XK_o,           floatpos,               {.v = " 26a -26a" } }, // ↗
	// { Agr|C,             XK_j,           floatpos,               {.v = "-26a   0a" } }, // ←
	// { Agr|C,             XK_l,           floatpos,               {.v = " 26a   0a" } }, // →
	// { Agr|C,             XK_m,           floatpos,               {.v = "-26a  26a" } }, // ↙
	// { Agr|C,             XK_comma,       floatpos,               {.v = "  0a  26a" } }, // ↓
	// { Agr|C,             XK_period,      floatpos,               {.v = " 26a  26a" } }, // ↘
	// /* Resize client, client center position is fixed which means that client expands in all directions */
	// { Agr|S,             XK_u,           floatpos,               {.v = "-26w -26h" } }, // ↖
	// { Agr|S,             XK_i,           floatpos,               {.v = "  0w -26h" } }, // ↑
	// { Agr|S,             XK_o,           floatpos,               {.v = " 26w -26h" } }, // ↗
	// { Agr|S,             XK_j,           floatpos,               {.v = "-26w   0h" } }, // ←
	// { Agr|S,             XK_k,           floatpos,               {.v = "800W 800H" } }, // ·
	// { Agr|S,             XK_l,           floatpos,               {.v = " 26w   0h" } }, // →
	// { Agr|S,             XK_m,           floatpos,               {.v = "-26w  26h" } }, // ↙
	// { Agr|S,             XK_comma,       floatpos,               {.v = "  0w  26h" } }, // ↓
	// { Agr|S,             XK_period,      floatpos,               {.v = " 26w  26h" } }, // ↘
	// /* Client is positioned in a floating grid, movement is relative to client's current position */
	// { Agr|A,             XK_u,           floatpos,               {.v = "-1p -1p" } }, // ↖
	// { Agr|A,             XK_i,           floatpos,               {.v = " 0p -1p" } }, // ↑
	// { Agr|A,             XK_o,           floatpos,               {.v = " 1p -1p" } }, // ↗
	// { Agr|A,             XK_j,           floatpos,               {.v = "-1p  0p" } }, // ←
	// { Agr|A,             XK_k,           floatpos,               {.v = " 0p  0p" } }, // ·
	// { Agr|A,             XK_l,           floatpos,               {.v = " 1p  0p" } }, // →
	// { Agr|A,             XK_m,           floatpos,               {.v = "-1p  1p" } }, // ↙
	// { Agr|A,             XK_comma,       floatpos,               {.v = " 0p  1p" } }, // ↓
	// { Agr|A,             XK_period,      floatpos,               {.v = " 1p  1p" } }, // ↘
	{ M|S,                    XK_i,           cyclelayout,            {.i = -1 } },
	{ M|S,                    XK_p,           cyclelayout,            {.i = +1 } },
	{ M|C,                    XK_minus,       setborderpx,            {.i = -1 } },
	{ M|C,                    XK_equal,       setborderpx,            {.i = +1 } },
	{ M|A,                    XK_0,           setborderpx,            {.i = 0 } },
	{ M|A|S,                  XK_comma,       tagallmon,              {.i = +1 } },
	{ M|A|S,                  XK_period,      tagallmon,              {.i = -1 } },
	TAGKEYS(                  XK_1,                                  0)
	TAGKEYS(                  XK_2,                                  1)
	TAGKEYS(                  XK_3,                                  2)
	TAGKEYS(                  XK_4,                                  3)
	TAGKEYS(                  XK_5,                                  4)
	TAGKEYS(                  XK_6,                                  5)
	TAGKEYS(                  XK_7,                                  6)
	TAGKEYS(                  XK_8,                                  7)
	TAGKEYS(                  XK_9,                                  8)
};

static Key cmdkeys[] = {
	/* modifier                    keys                     function         argument */
	{ 0,                           XK_Escape,               clearcmd,        {0} },
	{ C,                           XK_c,                    clearcmd,        {0} },
	{ 0,                           XK_i,                    setkeymode,      {.ui = INSERTMODE} },
	{ 0,                           XK_t,                    spawn,           SHCMD("dmenu_run") },
};

static Command commands[] = {
	/* modifier (4 keys)                          keysyms (4 keys)                                function         argument */
	{ {S, 0, 0, 0},   {XK_o, XK_e, 0, 0},   spawn,           SHCMD("dmenu_run") },
	{ {S, 0, 0, 0},   {XK_w, 0, 0, 0},      spawn,           SHCMD("st") },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask           button          function        argument */
	{ ClkButton,		    0,                   Button1,	     spawn,		     SHCMD("st -e htop") },
	{ ClkWinTitle,          0,                   Button1,        showhideclient, {0} },
	{ ClkWinTitle,          0,                   Button2,        zoom,           {0} },
	{ ClkStatusText,        0,                   Button1,        sigdwmblocks,   {.i = 1} },
	{ ClkStatusText,        0,                   Button2,        sigdwmblocks,   {.i = 2} },
	{ ClkStatusText,        0,                   Button3,        sigdwmblocks,   {.i = 3} },
	{ ClkStatusText,        S,                   Button3,        sigdwmblocks,   {.i = 4} },
	{ ClkStatusText,        C,                   Button3,        sigdwmblocks,   {.i = 5} },
	{ ClkClientWin,         A,                   Button1,        movemouse,      {0} },
	{ ClkClientWin,         A|C,                 Button1,        placemouse,     {.i = 1} },
	{ ClkClientWin,         A,                   Button2,        togglefloating, {0} },
	{ ClkClientWin,         A,                   Button3,        resizemouse,    {0} },
	{ ClkClientWin,         A|S,                 Button3,        dragcfact,      {0} },
	{ ClkClientWin,         A|S,                 Button1,        dragmfact,      {0} },
	{ ClkTagBar,            0,                   Button1,        view,           {0} },
	{ ClkTagBar,            0,                   Button3,        toggleview,     {0} },
	{ ClkTagBar,            A,                   Button1,        tag,            {0} },
	{ ClkTagBar,            A,                   Button3,        toggletag,      {0} },
};
