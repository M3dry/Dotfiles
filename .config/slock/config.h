/* user and group to drop privileges to */
static const char *user  = "m3";
static const char *group = "m3";

static const char *colorname[NUMCOLS] = {
	[INIT] =   "#111111",     /* after initialization */
	[INPUT] =  "#282d3e",   /* during input */
	[FAILED] = "#f07178",   /* wrong password */
	[CAPS] =   "#005577",         /* CapsLock on */
};

/* treat a cleared input like a wrong password (color) */
static const int failonclear = 1;

/* default message */
static const char * message = "";

/* text color */
static const char * text_color = "#ffffff";

/* text size (must be a valid size) */
static const char * font_name = "6x13";
