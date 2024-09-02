//
// Example source code exhibiting a (fixed) bug with bad literal offsets.
// The =A in bug-relative.s was being assembled incorrectly.
//
// After compiling this into bug-relative.s, assemble it with
// ../build/gas/as-new -a=bug-relative.lst -o bug-relative.obj bug-relative.s
//
int funky(int x, int y)
{
	int xx = x + 44;
	int yy = y + 999;
	int zz = x * y;
	return zz;
}
