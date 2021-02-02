//Modify this file to change what commands output to your statusbar, and recompile using the make command.
static const Block blocks[] = {
	/*Icon*/	/*Command*/	 	/*Update Interval*/	/*Update Signal*/
  {"",     "memory",       1,        1},
  {"",     "disk-space",   3600,     2},
  {"",     "cpu",          30,       3},
  {"",     "temp",         15,       4},
  {"",     "gpu",          30,       5},
  {"",     "pacupdate",    3600,     6},
  {"",     "volume",       0,        7},
  {"",     "network",      3,        9},
  {"",     "corona",       3600,    10},
  {"",     "kernel",       0,       11},
  {"",     "upt",          60,      12},
  {"",     "moonphase",    18000,   13},
  {"",     "clock",        1,       15},

};

//sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim = ' ';
