/* See LICENSE file for copyright and license details. */

/* appearance */
/*  Display modes of the tab bar: never shown, always shown, shown only in  */
/*  monocle mode in the presence of several windows.                        */
/*  Modes after showtab_nmodes are disabled.                                */
enum showtab_modes { showtab_never, showtab_auto, showtab_nmodes, showtab_always};
static const int showtab			= showtab_auto;        /* Default tab bar show mode */
static const int toptab				= 0;               /* False means bottom tab bar */

static const unsigned int borderpx  = 2;   /* border pixel of windows */
static const unsigned int snap      = 0;   /* snap pixel */
static const int swallowfloating    = 1;   /* 1 means swallow floating windows by default */
static const int decorhints         = 1;   /* 1 means respect decoration hints */
static const unsigned int gappih    = 8;   /* horiz inner gap between windows */
static const unsigned int gappiv    = 8;   /* vert inner gap between windows */
static const unsigned int gappoh    = 1;   /* horiz outer gap between windows and screen edge */
static const unsigned int gappov    = 1;   /* vert outer gap between windows and screen edge */
static       int smartgaps          = 1;   /* 1 means no outer gap when there is only one window */
static const int focusonwheel       = 0;
static const int showbar            = 1;   /* 0 means no bar */
static const int topbar             = 1;   /* 0 means bottom bar */
static const int user_bh            = 24;  /* 0 means that dwm will calculate bar height, >= 1 means dwm will user_bh as bar height */
static const unsigned int systraypinning = 0;   /* 0: sloppy systray follows selected monitor, >0: pin systray to monitor X */
static const unsigned int systrayspacing = 2;   /* systray spacing */
static const int systraypinningfailfirst = 1;   /* 1: if pinning fails, display systray on the first monitor, False: display systray on the last monitor*/
static const int showsystray        = 1;     /* 0 means no systray */
static const char *fonts[]          = { "mononoki Nerd Font Mono:size=12:antialias=true:autohint=true" };

static const char normfg[]                = "#51afef";
static const char selfg[]                 = "#ffffff";
static const char normbg[]                = "#111111";
static const char selbg[]                 = "#292d3e";
static const char normfloatwinborder[]    = "#000000";
static const char selfloatwinborder[]     = "#ffffff";
static const char normstickyborder[]      = "#000000";
static const char selstickyborder[]       = "#98be65";
static const char normstickyfloatborder[] = "#000000";
static const char selstickyfloatborder[]  = "#8acc35";
static const char normfakefullscr[]       = "#408ab2";
static const char selfakefullscr[]        = "#b869e5";
static const char normfakefullscrfloat[]  = "#289fe0";
static const char selfakefullscrfloat[]   = "#9b1be5";
/* Tab Bar */
static const char normtabfg[]             = "#ffffff";
static const char seltabfg[]              = "#000000";
static const char normtabbg[]             = "#3071db";
static const char seltabbg[]              = "#3071db";
/* Inverter mon */
static const char norminvmonbg[]          = "#3071db";
static const char norminvmonfg[]          = "#ffffff";
/* Lay constout borders */
static const char normtileborder[]        = "#292d3e";
static const char normfibonacciborder[]   = "#292d3e";
static const char normfloatborder[]       = "#292d3e";
static const char normdeckborder[]        = "#292d3e";
static const char normnrowgridborder[]    = "#292d3e";
static const char normbstackborder[]      = "#292d3e";
static const char normcenmasterborder[]   = "#292d3e";
static const char normmonocleborder[]     = "#292d3e";
static const char normgaplessgridborder[] = "#292d3e";
static const char seltileborder[]         = "#ff6c6b";
static const char selfibonacciborder[]    = "#ff6c6b";
static const char selfloatborder[]        = "#16cc31";
static const char seldeckborder[]         = "#ff6c6b";
static const char selnrowgridborder[]     = "#ff6c6b";
static const char selbstackborder[]       = "#c678dd";
static const char selcenmasterborder[]    = "#ff6c6b";
static const char selmonocleborder[]      = "#ff6c6b";
static const char selgaplessgridborder[]  = "#ff6c6b";

static const char *colors[][10]  = {
	/*                  fg      bg      float               sticky            sticky + float         fakefullscreen   fakefullscreen + float */
	[SchemeNorm]    = { normfg, normbg, normfloatwinborder, normstickyborder, normstickyfloatborder, normfakefullscr, normfakefullscrfloat },
	[SchemeSel]     = { selfg,  selbg,  selfloatwinborder,  selstickyborder,  selstickyfloatborder,  selfakefullscr,  selfakefullscrfloat },
	/* Tabs             fg         bg */
	[SchemeTabNorm] = { normtabfg, normtabbg },
	[SchemeTabSel]  = { seltabfg,  seltabbg},
	/* Unfocused mon    fg            bg */
	[SchemeInvMon]  = { norminvmonfg, norminvmonbg },
	/* Win borders          tile            fibonacci            float            deck            nrowgrid            bstack            centeredmaster       monocle            gaplessgrid */
	[SchemeNormLayout]  = { normtileborder, normfibonacciborder, normfloatborder, normdeckborder, normnrowgridborder, normbstackborder, normcenmasterborder, normmonocleborder, normgaplessgridborder },
	[SchemeSelLayout]   = { seltileborder,  selfibonacciborder,  selfloatborder,  seldeckborder,  selnrowgridborder,  selbstackborder,  selcenmasterborder,  selmonocleborder,  selgaplessgridborder },
    /* if you change layout order you also need to change the order here */
};

typedef struct {
	const char *name;
	const void *cmd;
} Sp;
const char *spcmd1[] = {"st", "-c", "spterm", "-t", "stSCP", "-g", "144x41", NULL };
const char *spcmd2[] = {"st", "-c", "spmus", "-t", "cmusSCP", "-g", "144x41", "-e", "cmus", NULL };
const char *spcmd3[] = {"qalculate-gtk", "--title", "spcal", NULL };
static Sp scratchpads[] = {
   /* name          cmd  */
   {"spterm",      spcmd1},
   {"spmus",       spcmd2},
   {"spcal",       spcmd3},
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title       tags mask     switchtotag     iscentered   isfloating   isterminal    noswallow   monitor */
    /* Scratchpads */
	{ "spterm",	  NULL,	      NULL,	      SPTAG(0),	    0,              1,           1,			  0,            0,          -1 }, /* St */
	{ "spmus",	  NULL,	      NULL,	      SPTAG(1),	    0,              1,           1,			  0,            0,          -1 }, /* cmus */
	{ NULL,		  NULL,	      "spcal",	  SPTAG(2),	    0,              1,           1,			  0,            0,          -1 }, /* qalculate-gtk */
    /* Terminals */
	{ "St",       NULL,       NULL,       0,            0,              0,           0,           1,            0,          -1 },
	{ "Alacritty",NULL,       NULL,       0,            0,              0,           0,           1,            0,          -1 },
	{ "XTerm",    NULL,       NULL,       0,            0,              0,           0,           1,            0,          -1 },
    /* Noswallow */
	{ NULL,       "Navigator",NULL,       1,            1,              0,           0,           0,            1,          -1 }, /* firefox */
	{ NULL,       NULL,       "Event Tester", 0,        0,              0,           0,           0,            1,          -1 }, /* xev */
	{ "Xephyr",   NULL,       NULL,       0,            0,              1,           1,           0,            1,          -1 }, /* xephyr */
	{ "Gimp",     NULL,       NULL,       1 << 8,       3,              1,           1,           0,            1,          -1 }, /* gimp */
	{ NULL,       NULL,       "glxgears", 0,            0,              1,           1,           0,            1,          -1 },
};

static const MonitorRule monrules[] = {
	/* monitor  tag  layout  mfact  nmaster  showbar  topbar */
	{  1,       -1,  5,      -1,    -1,      -1,      -1     }, // use a different layout for the second monitor
	{  -1,      -1,  0,      -1,    -1,      -1,      -1     }, // default
};

/* layout(s) */
static const float mfact     = 0.5;  /* factor of master area size [0.05..0.95] */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */
static const int nmaster     = 1;    /* number of clients in master area */
static const int attachbelow = 1;    /* 1 means attach after the currently active window */

#define FORCE_VSPLIT 1  /* nrowgrid layout: force two clients to always split vertically */

#include <X11/XF86keysym.h>
#include "vanitygaps.c"

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "(@)",      spiral },
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[D]",      deck },
	{ "###",      nrowgrid },
	{ "TTT",      bstack },
	{ "|M|",      centeredmaster },
	{ "[M]",      monocle },
	{ "HHH",      horizgrid },
	{ NULL,       NULL },
};

/* key definitions */
#define M Mod4Mask
#define A Mod1Mask
#define S ShiftMask

#define C ControlMask
#define TAGKEYS(KEY,TAG) \
	{ A,                       KEY,      comboview,      {.ui = 1 << TAG} }, \
	{ C,                       KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ M,                       KEY,      toggletag,      {.ui = 1 << TAG} }, \
	{ A|S,                     KEY,      combotag,       {.ui = 1 << TAG} }, \
	{ A|C,                     KEY,      tagwith,        {.ui = 1 << TAG} }, \
	{ M|S,                     KEY,      swaptags,       {.ui = 1 << TAG} }, \
	{ A|M,                     KEY,      tagnextmon,     {.ui = 1 << TAG} }, \
	{ A|M|S,                   KEY,      tagprevmon,     {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }


static Key keys[] = {
	/* modifier                     key        function        argument */
    /* Spawing preograms */
	{ A,                       XK_Return,     spawn,                  SHCMD("$TERMINAL") },
	{ A|S,                     XK_c,          spawn,                  SHCMD("$TERMINAL htop") },
	{ A|S,                     XK_z,          spawn,                  SHCMD("playerctl play-pause") },
	{ A,                       XK_e,          spawn,                  SHCMD("$TERMINAL $EDITOR") },
	{ M,                       XK_e,          spawn,                  SHCMD("emacsclient -c -a emacs") },
	{ M|S,                     XK_c,          spawn,                  SHCMD("emacsclient -c -a e'macs' --eval '(ibuffer)'") },
	{ A,                       XK_w,          spawn,                  SHCMD("xdo activate -N LibreWolf || librewolf") },
	{ A,                       XK_o,          spawn,                  SHCMD("chromium") },
	{ A,                       XK_Escape,     spawn,                  SHCMD("xkill") },
	{ C|A,                     XK_d,          spawn,                  SHCMD("discord") },
	{ A|S,                     XK_u,          spawn,                  SHCMD("import my-stuff/Pictures/snips/$(date +'%H:%M:%S').png") },
	{ A,                       XK_p,          spawn,                  SHCMD("pcmanfm") },
	{ A,                       XK_a,          spawn,                  SHCMD("$TERMINAL vifmrun") },
	{ C,                       XK_m,          spawn,                  SHCMD("multimc") },
	{ M|C|A,                   XK_l,          spawn,                  SHCMD("slock") },
	{ C|A,                     XK_z,          spawn,                  SHCMD("playerctl play-pause") },
    /* Dmenu scripts */
	{ A|S,                     XK_Return,     spawn,                  SHCMD("dmenu_run -l 5 -g 10 -p 'Run:'") },
	{ A|S,                     XK_s,          spawn,                  SHCMD("switch") },
	{ A,                       XK_c,          spawn,                  SHCMD("volume-script") },
	{ A|C,                     XK_Return,     spawn,                  SHCMD("Booky 'st nvim' '><' 'Cconfig'") },
	{ A|S,                     XK_w,          spawn,                  SHCMD("Booky 'librewolf' ':' 'Bconfig'") },
	{ A|S,                     XK_e,          spawn,                  SHCMD("emoji-script") },
	{ A|S,                     XK_d,          spawn,                  SHCMD("calc") },
	{ A|S,                     XK_v,          spawn,                  SHCMD("manview") },
	{ A,                       XK_z,          spawn,                  SHCMD("music-changer cmus") },
	{ M,                       XK_p,          spawn,                  SHCMD("passmenu2 -F -p 'Passwords:'") },
	{ A|S,                     XK_a,          spawn,                  SHCMD("allmenu") },
	{ A|C,                     XK_q,          spawn,                  SHCMD("shut") },
    /* MultiMedia keys */
	{ 0, XF86XK_AudioPrev,                    spawn,                  SHCMD("playerctl --player cmus previous") },
	{ 0, XF86XK_AudioNext,                    spawn,                  SHCMD("playerctl --player cmus next") },
	{ 0, XF86XK_AudioPlay,                    spawn,                  SHCMD("playerctl --player cmus play-pause") },
	{ 0, XF86XK_AudioLowerVolume,             spawn,                  SHCMD("pamixer --allow-boost -d 1 ; killall dwmStatus && dwmStatus &") },
	{ 0, XF86XK_AudioRaiseVolume,             spawn,                  SHCMD("pamixer --allow-boost -i 1 ; killall dwmStatus && dwmStatus &") },
    /* DWM keybindings */
	{ A,                       XK_q,          killclient,             {0} },
	{ A|S,                     XK_q,          killunsel,              {0} },
	{ A,                       XK_n,          togglebar,              {0} },
	{ A|S,                     XK_h,          setmfact,               {.f = -0.05} },
	{ A|S,                     XK_l,          setmfact,               {.f = +0.05} },
	{ A|S,                     XK_j,          setcfact,               {.f = +0.25} },
	{ A|S,                     XK_k,          setcfact,               {.f = -0.25} },
	{ A|C,                     XK_u,          setcfact,               {0} },
	{ A,                       XK_bracketleft,incnmaster,             {.i = +1 } },
	{ A,                       XK_bracketright,incnmaster,            {.i = -1 } },
	{ M,                       XK_space,      focusmaster,            {0} },
	{ A,                       XK_h,          focusdir,               {.i = 0 } }, // left
	{ A,                       XK_l,          focusdir,               {.i = 1 } }, // right
	{ A,                       XK_k,          focusdir,               {.i = 2 } }, // up
	{ A,                       XK_j,          focusdir,               {.i = 3 } }, // down
	{ M,                       XK_h,          focusstack,             {.i = 0 } },
	{ M,                       XK_l,          focusstack,             {.i = 1 } },
    /* Layouts */
	{ A,                       XK_t,          setlayout,              {.v = &layouts[0]} },
	{ A,                       XK_v,          setlayout,              {.v = &layouts[1]} },
	{ A|S,                     XK_f,          setlayout,              {.v = &layouts[2]} },
	{ A,                       XK_d,          setlayout,              {.v = &layouts[3]} },
	{ A,                       XK_g,          setlayout,              {.v = &layouts[4]} },
	{ A,                       XK_b,          setlayout,              {.v = &layouts[5]} },
	{ A|S,                     XK_m,          setlayout,              {.v = &layouts[6]} },
	{ A,                       XK_m,          setlayout,              {.v = &layouts[7]} },
	{ A|S,                     XK_g,          setlayout,              {.v = &layouts[8]} },
	{ A|S,                     XK_t,          tabmode,                {-1} },
	{ A|C,                     XK_i,          cyclelayout,            {.i = -1 } },
	{ A|C,                     XK_p,          cyclelayout,            {.i = +1 } },
	{ A,                       XK_0,          view,                   {.ui = ~0 } },
	{ A,                       XK_Tab,        goback,                 {0} },
	{ A|S,                     XK_n,          shiftviewclients,       { .i = +1 } },
	{ A|S,                     XK_p,          shiftviewclients,       { .i = -1 } },
    /* Window manipulation */
	{ A,                       XK_semicolon,  zoom,                   {0} },
	{ M,                       XK_j,          pushdown,               {0} },
	{ M,                       XK_k,          pushup,                 {0} },
	{ A,                       XK_space,      togglefloating,         {0} },
	{ A|S,                     XK_space,      unfloatvisible,         {0} },
	{ M,                       XK_s,          togglesticky,           {0} },
	{ A,                       XK_f,          togglefullscr,          {0} },
	{ A|C,                     XK_f,          togglefakefullscreen,   {0} },
	{ A,                       XK_u,          togglescratch,          {.ui = 0 } },
    { A,                       XK_i,          togglescratch,          {.ui = 1 } },
    { A,                       XK_y,          togglescratch,          {.ui = 2 } },
    /* Monitors */
	{ A,                       XK_comma,      focusmon,               {.i = -1 } },
	{ A,                       XK_period,     focusmon,               {.i = +1 } },
	{ A|ShiftMask,             XK_comma,      tagmon,                 {.i = -1 } },
	{ A|ShiftMask,             XK_period,     tagmon,                 {.i = +1 } },
    /* Moveresize */
	{ A|C,                     XK_j,          moveresize,             {.v = "0x 25y 0w 0h" } },
	{ A|C,                     XK_k,          moveresize,             {.v = "0x -25y 0w 0h" } },
	{ A|C,                     XK_l,          moveresize,             {.v = "25x 0y 0w 0h" } },
	{ A|C,                     XK_h,          moveresize,             {.v = "-25x 0y 0w 0h" } },
	{ M|C,                     XK_j,          moveresize,             {.v = "0x 0y 0w 25h" } },
	{ M|C,                     XK_k,          moveresize,             {.v = "0x 0y 0w -25h" } },
	{ M|C,                     XK_l,          moveresize,             {.v = "0x 0y 25w 0h" } },
	{ M|C,                     XK_h,          moveresize,             {.v = "0x 0y -25w 0h" } },
    /* Gaps */
	{ A|S,                     XK_equal,      incrgaps,               {.i = +1 } },
	{ A|S,                     XK_minus,      incrgaps,               {.i = -1 } },
	{ A|S,                     XK_0,          defaultgaps,            {0} },
	{ A|C,                     XK_0,          togglegaps,             {0} },
	TAGKEYS(                   XK_1,                                  0)
	TAGKEYS(                   XK_2,                                  1)
	TAGKEYS(                   XK_3,                                  2)
	TAGKEYS(                   XK_4,                                  3)
	TAGKEYS(                   XK_5,                                  4)
	TAGKEYS(                   XK_6,                                  5)
	TAGKEYS(                   XK_7,                                  6)
	TAGKEYS(                   XK_8,                                  7)
	TAGKEYS(                   XK_9,                                  8)
	{ M|S,                     XK_Escape,     quit,                   {0} }, 
	{ A|C|S,                   XK_q,          quit,                   {1} }, 
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkClientWin,         A,              Button1,        movemouse,      {0} },
	{ ClkClientWin,         A,              Button2,        togglefloating, {0} },
	{ ClkClientWin,         A,              Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            A,              Button1,        tag,            {0} },
	{ ClkTagBar,            A,              Button3,        toggletag,      {0} },
	{ ClkTabBar,            0,              Button1,        focuswin,       {0} },
};

#include "dwmc.c"
/* signal definitions */
/* signum must be greater than 0 */
/* trigger signals using `xsetroot -name "fsignal:<signame> [<type> <value>]"` */
static Signal signals[] = {
	/* signum           function */
	{ "focusstack",     focusstack },
	{ "setmfact",       setmfact },
	{ "togglebar",      togglebar },
	{ "incnmaster",     incnmaster },
	{ "togglefloating", togglefloating },
	{ "focusmon",       focusmon },
	{ "tagmon",         tagmon },
	{ "zoom",           zoom },
	{ "view",           view },
	{ "viewall",        viewall },
	{ "viewex",         viewex },
	{ "toggleview",     view },
	{ "toggleviewex",   toggleviewex },
	{ "tag",            tag },
	{ "tagall",         tagall },
	{ "tagex",          tagex },
	{ "toggletag",      tag },
	{ "toggletagex",    toggletagex },
	{ "killclient",     killclient },
	{ "quit",           quit },
	{ "setlayout",      setlayout },
	{ "setlayoutex",    setlayoutex },
};
