/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 2;   /* border pixel of windows */
static const unsigned int snap      = 0;   /* snap pixel */
static const int swallowfloating    = 1;   /* 1 means swallow floating windows by default */
static const unsigned int gappih    = 4;   /* horiz inner gap between windows */
static const unsigned int gappiv    = 4;   /* vert inner gap between windows */
static const unsigned int gappoh    = 4;   /* horiz outer gap between windows and screen edge */
static const unsigned int gappov    = 4;   /* vert outer gap between windows and screen edge */
static       int smartgaps          = 1;   /* 1 means no outer gap when there is only one window */
static const int focusonwheel       = 0;
static const int showbar            = 1;   /* 0 means no bar */
static const int topbar             = 1;   /* 0 means bottom bar */
static const int user_bh            = 20;  /* 0 means that dwm will calculate bar height, >= 1 means dwm will user_bh as bar height */
static const char *fonts[]          = {"mononoki Nerd Font Mono:size=9:antialias=true:autohint=true",
                                       "JoyPixels:size=8:antialias=true:autohint=true"};

static const char normfg[]           = "#f0f0f0";
static const char selfg[]            = "#ffffff";
static const char normbg[]           = "#111111";
static const char selbg[]            = "#292d3e";
static const char normbar[]          = "#111111";
static const char selbar[]           = "#292d3e";
static const char normborder[]       = "#111111";
static const char selborder[]        = "#ff6c6b";
static const char normfloatborder[]  = "#111111";
static const char selfloatborder[]   = "#ffffff";

static const char *colors[][5]  = {
	/*               fg      bg      bar      border      float border*/
	[SchemeNorm] = { normfg, normbg, normbar, normborder, normfloatborder },
	[SchemeSel]  = { selfg,  selbg,  selbar,  selborder,  selfloatborder },
};

typedef struct {
	const char *name;
	const void *cmd;
} Sp;
const char *spcmd1[] = {"st", "-n", "spterm", "-t", "stSCP", "-g", "144x41", NULL };
const char *spcmd2[] = {"st", "-n", "spmus", "-t", "cmusSCP", "-g", "144x41", "-e", "cmus", NULL };
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
	{ "Gimp",     NULL,       NULL,       1 << 8,       1,              0,           1,           0,            0,          -1 },
	{ "Firefox",  NULL,       NULL,       0,            0,              0,           0,           0,            0,          -1 },
	{ NULL,		  "spterm",	  NULL,	      SPTAG(0),	    0,              1,           1,			  0,            0,          -1 },
	{ NULL,		  "spmus",	  NULL,	      SPTAG(1),	    0,              1,           1,			  0,            0,          -1 },
	{ NULL,		  NULL,	      "spcal",	  SPTAG(2),	    0,              1,           1,			  0,            0,          -1 },
	{ "st",       NULL,       NULL,       0,            0,              0,           0,           1,            0,          -1 },
	{ NULL,       NULL,       "Event Tester", 0,        0,              0,           0,           0,            1,          -1 }, /* xev */
};

/* layout(s) */
static const float mfact     = 0.5;  /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */
static const int attachbelow = 1;    /* 1 means attach after the currently active window */

#define FORCE_VSPLIT 1  /* nrowgrid layout: force two clients to always split vertically */
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
	{ "HHH",      grid },
	{ NULL,       NULL },
};

void swaptags(const Arg *arg);

/* key definitions */
#define M Mod4Mask
#define A Mod1Mask
#define S ShiftMask

#define C ControlMask
#define TAGKEYS(KEY,TAG) \
	{ A,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ C,                       KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ M,                       KEY,      toggletag,      {.ui = 1 << TAG} }, \
	{ A|S,                     KEY,      tag,            {.ui = 1 << TAG} }, \
	{ A|C,                     KEY,      tagwith,        {.ui = 1 << TAG} }, \
	{ M|S,                     KEY,      swaptags,       {.ui = 1 << TAG} }, \
	{ A|M,                     KEY,      tagnextmon,     {.ui = 1 << TAG} }, \
	{ A|M|S,                   KEY,      tagprevmon,     {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

#include <X11/XF86keysym.h>
static Key keys[] = {
	/* modifier                     key        function        argument */
    /* Spawing preograms */
	{ A,                       XK_Return,     spawn,                  SHCMD("$TERMINAL") },
	{ A|S,                     XK_c,          spawn,                  SHCMD("$TERMINAL htop") },
	{ A|S,                     XK_z,          spawn,                  SHCMD("playerctl play-pause") },
	{ A,                       XK_e,          spawn,                  SHCMD("$TERMINAL $EDITOR") },
	{ M,                       XK_e,          spawn,                  SHCMD("emacsclient -c -a emacs") },
	{ M|S,                     XK_c,          spawn,                  SHCMD("emacsclient -c -a e'macs' --eval '(ibuffer)'") },
	{ A,                       XK_w,          spawn,                  SHCMD("$BROWSER") },
	{ A|C,                     XK_w,          spawn,                  SHCMD("$BROWSER youtube.com twitch.tv mail.protonmail.com/login") },
	{ A,                       XK_b,          spawn,                  SHCMD("freetube") },
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
	{ A|S,                     XK_p,          spawn,                  SHCMD("passmenu2 -F -p 'Passwords:'") },
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
	{ A,                       XK_j,          focusstack,             {.i = +1 } },
	{ A,                       XK_k,          focusstack,             {.i = -1 } },
	{ A,                       XK_h,          setmfact,               {.f = -0.05} },
	{ A,                       XK_l,          setmfact,               {.f = +0.05} },
	{ A|S,                     XK_j,          setcfact,               {.f = +0.25} },
	{ A|S,                     XK_k,          setcfact,               {.f = -0.25} },
	{ A|C,                     XK_u,          setcfact,               {0} },
	{ A,                       XK_bracketleft,incnmaster,             {.i = +1 } },
	{ A,                       XK_bracketright,incnmaster,            {.i = -1 } },
	{ M,                       XK_space,      focusmaster,            {0} },
    /* Gaps */
	{ M|S,                     XK_equal,      incrgaps,               {.i = +1 } },
	{ M|S,                     XK_minus,      incrgaps,               {.i = -1 } },
	{ M|S,                     XK_0,          togglegaps,             {0} },
	{ M|C,                     XK_0,          defaultgaps,            {0} },
    /* Layouts */
	{ A,                       XK_t,          setlayout,              {.v = &layouts[0]} },
	{ A,                       XK_v,          setlayout,              {.v = &layouts[1]} },
	{ A|S,                     XK_f,          setlayout,              {.v = &layouts[2]} },
	{ A,                       XK_d,          setlayout,              {.v = &layouts[3]} },
	{ A,                       XK_g,          setlayout,              {.v = &layouts[4]} },
	{ A|S,                     XK_b,          setlayout,              {.v = &layouts[5]} },
	{ A|S,                     XK_m,          setlayout,              {.v = &layouts[6]} },
	{ A,                       XK_m,          setlayout,              {.v = &layouts[7]} },
	{ A|C,                     XK_i,          cyclelayout,            {.i = -1 } },
	{ A|C,                     XK_p,          cyclelayout,            {.i = +1 } },
	{ A,                       XK_0,          view,                   {.ui = ~0 } },
	{ A,                       XK_Tab,        goback,                 {0} },
    /* Window manipulation */
	{ A,                       XK_apostrophe, zoom,                   {0} },
	{ M,                       XK_j,          pushdown,               {0} },
	{ M,                       XK_k,          pushup,                 {0} },
	{ A,                       XK_space,      togglefloating,         {0} },
	{ C,                       XK_s,          togglesticky,           {0} },
	{ A,                       XK_f,          togglefullscr,          {0} },
	{ A,                       XK_u,          togglescratch,          {.ui = 0 } },
    { A,                       XK_i,          togglescratch,          {.ui = 1 } },
    { A,                       XK_y,          togglescratch,          {.ui = 2 } },
    /* Monitors */
	{ A,                       XK_comma,      focusmon,               {.i = -1 } },
	{ A,                       XK_period,     focusmon,               {.i = +1 } },
	{ A|ShiftMask,             XK_comma,      tagmon,                 {.i = -1 } },
	{ A|ShiftMask,             XK_period,     tagmon,                 {.i = +1 } },
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
};

void
swaptags(const Arg *arg)
{
	unsigned int newtag = arg->ui & TAGMASK;
	unsigned int curtag = selmon->tagset[selmon->seltags];

	if (newtag == curtag || !curtag || (curtag & (curtag-1)))
		return;

	for (Client *c = selmon->clients; c != NULL; c = c->next) {
		if((c->tags & newtag) || (c->tags & curtag))
			c->tags ^= curtag ^ newtag;

		if(!c->tags) c->tags = newtag;
	}

	selmon->tagset[selmon->seltags] = newtag;

	focus(NULL);
	arrange(selmon);
}
