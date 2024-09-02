//
// Example source code exhibiting a (fixed) bug with bad literal offsets.
// The =A in bug-global-rel.s was being assembled incorrectly.
//
// After compiling this into bug-global-rel.s, assemble it with
// ../build/gas/as-new -a=bug-global-rel.lst -o bug-global-rel.obj bug-global-rel.s
//
int glueball = 1234;

int bumpy(int x)
{
	glueball = 444;
	glueball = 2*x;
	glueball = 888;
	glueball = 77*x*x;
	glueball = 444;
	glueball = 888;
	return glueball;
}
