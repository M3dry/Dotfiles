/* See LICENSE file for copyright and license details. */
/* Default settings; can be overriden by command line. */

static int topbar = 1;                      /* -b  option; if 0, dmenu appears at bottom */
static int instant = 0;                     /* -n  option; if 1, selects matching item without the need to press enter */
static int center = 0;                      /* -c  option; if 0, dmenu won't be centered on the screen */
static int min_width = 500;                 /* minimum width when centered */
static int colorprompt = 0;                 /* -p  option; if 1, prompt uses SchemeSel, otherwise SchemeNorm */
static int fuzzy = 0;                      /* -F  option; if 0, dmenu doesn't use fuzzy matching     */
static int incremental = 0;                 /* -r  option; if 1, outputs text each time a key is pressed */
/* -fn option overrides fonts[0]; default X11 font or font set */
static const char *fonts[] = {
	"mononoki nerd font:pixelsize=13:antialias=true:autohint=true",
	"joypixels:pixelsize=14:antialias=true:autohint=true"
};
static const char *prompt      = NULL;      /* -p  option; prompt to the left of input field */
static const char *dynamic     = NULL;      /* -dy option; dynamic command to run on input change */
static const char *symbol_1 = "<";
static const char *symbol_2 = ">";


static const char *colors[][2] =
{
	/*                        fg         bg       */
	[SchemeNorm] =          { "#f0f0f0", "#111111" },
	[SchemeSel] =           { "#ffffff", "#292d3e" },
	[SchemeOut] =           { "#000000", "#000000" },
	[SchemeMid] =           { "#f0f0f0", "#161616" },
	[SchemeSelHighlight] =  { "#f0f0f0", "#161616" },
	[SchemeNormHighlight] = { "#ecbe7b", "#000000" },
};
/* -l option; if nonzero, dmenu uses vertical list with given number of lines */
static unsigned int lines      = 0;
/* -g option; if nonzero, dmenu uses a grid comprised of columns and lines */
static unsigned int columns    = 0;
static unsigned int lineheight = 0;         /* -h option; minimum height of a menu line     */

/*
 * Characters not considered part of a word while deleting words
 * for example: " /?\"&[]"
 */
static const char worddelimiters[] = " ";

/* Size of the window border */
static unsigned int border_width = 0;

