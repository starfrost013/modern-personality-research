static char *SCCSID = "@(#)sysimain.c	10.9 87/02/26";

/*
 * sysimain.c - sysinit C routines to load and interpret EXE files.
 */

#include		<devhdr.h>	/* "C" version of devhdr.inc */
#include		<sysini.h>	/* "C" version of sysini.inc */
#include		<sysinvar.h>	/* "C" version of sysinvar.inc */
#include		<newexe.h>
#include		<dos.h>


#ifdef DEBUG
#  ifndef SYSDEBUG
#    ifdef VERBOSE
#      define SYSDEBUG	~4		/* all but relocation fixups */
#    else
#      define SYSDEBUG	1		/* minimal output */
#    endif
#  endif
   int sysDebug = SYSDEBUG;
#  define DBPRT(m, a)	if (sysDebug & m) dprintf a ; else
#else
#  define DBPRT(m, a)
#endif
#ifdef PAGING
#  define PAGESIZE	4096
#endif


/* seg off to far pointer */
#define sotofar(seg, off) \
	(((char far *) (((long)(unsigned) (seg)) << 16)) + (unsigned) (off))

/* real mode seg, off to physical address */
#define sotop(seg, off) \
	((((long) (unsigned) (seg)) << PARASHIFT) + (unsigned) (off))

/* physical address to real mode seg, off */
#define ptoseg(p)	((unsigned) ((p) >> PARASHIFT))
#define ptooff(p)	((unsigned) (p) & 0x000f)

/* physical address to real mode far pointer */
#define ptofar(p) \
	(((char far *) (((long) (p) << 12) & 0xffff0000L)) + ptooff(p))

/* extract the segment of a far pointer to a far pointer */
#define FPF_SEG(fp) (*((unsigned far *) &(fp) + 1))

/* extract the offset of a far pointer to a far pointer */
#define FPF_OFF(fp) (*((unsigned far *) &(fp)))

/* round a physical address or size up to the next paragraph */
#define PARA(a) 	(((a) + 15L) & ~15L)

/* round a physical address up to the next tileable address */
#ifdef PAGING
#define TILE(a) 	(((a) + (long)(PAGESIZE-1)) & ~((long)(PAGESIZE-1)))
#else
#define TILE(a) 	(((a) + 127L) & ~127L)
#endif

#ifndef PAGING
/* round a physical address or size up to the next multiple of HeaderSize */
#define HDRROUND(p)	(((p) + HeaderSize - 1) & ~(HeaderSize - 1))
#endif

#define	NSTILE	NSPRELOAD		/* overload PRELOAD bit with TILE */

#define LOADSEG   	0x1070	/* physical load segment */
#define LOADALLSEG	0x80	/* segment of LOADALL buffer */
#define LOADALLLEN	102	/* size of LOADALL buffer */
#define	SELINCR		8	/* selector increment */
#define ARENASIZE	30	/* # of ArenaInfo entries */
#define	PARASHIFT	4	/* paragraph shift count */
#define	KSHIFT		10	/* 1k shift count */

struct arena ArenaInfo[ARENASIZE];

struct MsInitVars MsInitVars;
static struct SysInitVars SysInitVars;
#ifndef PAGING
static int HeaderSize;
#endif
static int NoHighMem;
static unsigned SelBase;

extern int (far *DosInit)();
extern unsigned MemLimit;

extern int open(char far *, int);
extern int close(int);
extern long lseek(int, long, int);
extern unsigned read(int, char far *, unsigned);

extern int far BiosInit(int, int, int, struct SysInitVars far *);
extern int far BiosReadDos(unsigned, unsigned);
extern int far BiosReInit();

extern int fcopy(char far *, char far *, unsigned);
extern int bcopy(char far *, char far *, unsigned);
extern int setds(int);

void getexeinfo(int, int, int, long *);
int applyrel(int);
checkoffset(unsigned, long);
int clearbss(int);
void editddhdr(struct SysDev far *, unsigned short, unsigned short);


/*
 * Global data used to load and relocate the Bios, Dos and device drivers.
 */

#define SEGMAX	20

struct seginfo {
	long	 si_psize;		/* physical size */
	long	 si_vsize;		/* virtual size */
	long	 si_dstaddr;		/* final physical address */
	long	 si_srcaddr;		/* interim physical address */
	unsigned si_loadseg;		/* segment address after loading */
	unsigned si_sel;		/* final selector address */
};

struct exe_hdr exehdr;
struct new_exe newexe;
struct new_seg segtbl[SEGMAX+1];	/* exact copy of file's segtbl */
struct seginfo seginfo[SEGMAX+1];




/*
 * sysinit - Finish up loading the bios and then load the dos.
 *
 *	- Finish the bios load by moving the bios' segments into position,
 *	  performing any relocation as specified in the new EXE image.
 *	- Call BiosInit to initialize itself and return important information.
 *	- Move sysinit's segments into high memory, leaving room for memory
 *	  arena header, then adjust our segment register values to point to
 *	  the moved code/data.
 *	- Call the bios to read in the dos new EXE image.
 *	- Move dos' segments into position and apply relocation.
 */
sysinit(bx, cx, dx, cs, ds)
int bx;
int cx;
int dx;
int cs;
int ds;
{
	register int i;
	register int sysicode, sysidata;
	struct arena *ap;
	int dosloadseg;
	int bioscodeseg, biosdataseg;
	long endaddr;
	long sysicodesize, sysicodestart;
	long sysidatasize, sysidatastart;

	DBPRT(1,("bx=%x, cx=%x, dx=%x, cs=%x, ds=%x\n", bx, cx, dx, cs, ds));
#ifndef PAGING
	/*
	 * If the ne_heap field was zero or not a power of 2, reset
	 * sysinit's idea of the physical arena header size to the default.
	 */
	if (HeaderSize == 0 || ((HeaderSize-1) & HeaderSize)) {
		HeaderSize = 32;
	}
	DBPRT(1,("HeaderSize=%x,NoHighMem=%x\n", HeaderSize, NoHighMem));
#else
	DBPRT(1,("NoHighMem=%x\n", NoHighMem));
#endif

	ap = &ArenaInfo[0];
#ifndef PAGING
	/*
	 * Set up the begin sentinel arena header.  This must be located
	 * at exactly HeaderSize bytes below the header for the first
	 * segment, because DOS routines depend on the difference between
	 * the first two physical addresses being the arena header size.
	 */
	ap->a_paddr = sotop(LOADALLSEG, 0) - 2*HeaderSize;
	ap->a_flags = A_FREE;		/* begin sentinel */
	ap++;

	ap->a_paddr = sotop(LOADALLSEG, 0) - HeaderSize;
	ap->a_flags = A_FIXED | A_DATA | A_MAP;
	ap->a_sel = LOADALLSEG;
	ap++;

	endaddr = HDRROUND(sotop(LOADALLSEG, 0) + LOADALLLEN + HeaderSize);
#else
	endaddr = TILE((long) PAGESIZE);
#endif

	getexeinfo(LOADSEG, ptoseg(endaddr), 1, &endaddr);

	/*
	 * Handle all BIOS segments.  Sysinit's code and data are the next to
	 * last and last segments, respectively, and will be handled specially.
	 */
	sysicode = newexe.ne_cseg - 1;
	sysidata = newexe.ne_cseg;
	for (i = 1; i < sysicode; i++) {
#ifdef PAGING
		ap->a_paddr = seginfo[i].si_dstaddr;
#else
		ap->a_paddr = seginfo[i].si_dstaddr - HeaderSize;
#endif
		if ((segtbl[i].ns_flags & NSTYPE) == NSDATA) {
			/*
			 *  We have to tile bios data for RDevInit()
			 */
			ap->a_flags = A_FIXED | A_DATA | A_MAP;
			ap->a_sel = SelBase = biosdataseg = seginfo[i].si_sel;
		} else if ((segtbl[i].ns_flags & NSTYPE) == NSCODE) {
			ap->a_flags = A_FIXED | A_CODE;
			bioscodeseg = seginfo[i].si_sel;
		} else {
			DBPRT(-1,("bad bios segment type\n"));
			fatal();
		}
		ap++;
		applyrel(i);		/* perform fixups */
		copyseg(i);		/* move segment to final position */
		clearbss(i);		/* and clear unitialized memory */
	}

	i--;				/* index last bios segment */
#ifdef PAGING
	endaddr =TILE(seginfo[i].si_srcaddr + seginfo[i].si_vsize);
#else
	endaddr =TILE(seginfo[i].si_srcaddr + seginfo[i].si_vsize + HeaderSize);
#endif
	dosloadseg = ptoseg(endaddr);

	applyrel(sysicode);		/* perform sysinit code fixups */
	applyrel(sysidata);		/* perform sysinit data fixups */

	BiosInit(bx, cx, dx, (struct SysInitVars far *) &SysInitVars);

	/*
	 * Set up real mode device link chain.
	 * Only the first pointer needs to be converted since it is
	 * an old driver and so has the header in the code segment
	 */

	FPF_SEG(SysInitVars.si_3xdevlist) = bioscodeseg;
	MsInitVars.MsInit_3xDeviceList = SysInitVars.si_3xdevlist;
	DBPRT(1,("devlist=%lp, 3xdevlist=%lp, buffers=%x, orgfinalseg=%x\n",
		SysInitVars.si_devlist,
		SysInitVars.si_3xdevlist,
		SysInitVars.si_buffers,
		SysInitVars.si_finalseg));

#ifndef PAGING
	/*
	 *  Bump si_finalseg (DOS' start address) up to the next
	 *  paragraph that aligns on a HeaderSize boundary.
	 */
	SysInitVars.si_finalseg =
		ptoseg(HDRROUND(sotop(SysInitVars.si_finalseg, 0)));
#else
	SysInitVars.si_finalseg =
		ptoseg(TILE(sotop(SysInitVars.si_finalseg, 0)));
#endif
	if (dosloadseg < SysInitVars.si_finalseg) {
		dosloadseg = SysInitVars.si_finalseg;
	}
	if (NoHighMem) {
		SysInitVars.si_highmem = 0;
	}

	DBPRT(1,("dosloadseg=%x, finalseg=%x, mem=%uk/%uk, defdrive=%x\n",
		dosloadseg,
		SysInitVars.si_finalseg,
		SysInitVars.si_lowmem, SysInitVars.si_highmem,
		SysInitVars.si_defdrive));


	MsInitVars.MsInit_DefDrive = SysInitVars.si_defdrive;
	MsInitVars.MsInit_DeviceList = SysInitVars.si_devlist;
	editddhdr(MsInitVars.MsInit_DeviceList, biosdataseg, bioscodeseg);

#ifdef PAGING
	sysidatasize = seginfo[sysidata].si_vsize;
	sysicodesize = seginfo[sysicode].si_vsize;
#else
	sysidatasize = HDRROUND(seginfo[sysidata].si_vsize);
	sysicodesize = HDRROUND(seginfo[sysicode].si_vsize);
#endif

	/*
	 *  MsInit depends on free memory immediately prior to sysinit's data
	 *  segment, so the code segment must lie just below the high end of
	 *  low memory, and the data segment below the code segment.
	 */
#ifdef PAGING
	sysicodestart = ((((long) SysInitVars.si_lowmem << KSHIFT) -
	    sysicodesize) & ~(PAGESIZE - 1));
	sysicodesize =  ((long) SysInitVars.si_lowmem << KSHIFT)
				      - sysicodestart;
#else
	sysicodestart = ((long) SysInitVars.si_lowmem << KSHIFT)
				      - (sysicodesize + HeaderSize);
#endif
#ifdef PAGING
	sysidatastart = (sysicodestart - sysidatasize) & ~(PAGESIZE - 1);
#else
	sysidatastart = sysicodestart - (sysidatasize + HeaderSize);
#endif

	seginfo[sysicode].si_dstaddr = sysicodestart;
	seginfo[sysicode].si_psize = seginfo[sysicode].si_vsize;
	copyseg(sysicode);

	/*
	 *  Set i to the lowest paragraph occupied by sysinit's moved segments.
	 *  BiosReadDos() uses this value to determine the highest location in
	 *  low memory available for a scratch buffer to be used while reading
	 *  the DOS into memory.
	 */
	i = ptoseg(sysidatastart);

	/*
	 * Transfer to moved code and data segments, but copy the data segment
	 * immediately prior to resetting ds and ss, so that all variables
	 * maintain their latest values.  Resetsegs() handles the stack
	 * parameters and return offset.
	 */
	seginfo[sysidata].si_dstaddr = sysidatastart;
	seginfo[sysidata].si_psize = seginfo[sysidata].si_vsize;
	copyseg(sysidata);
	DBPRT(1,("about to transfer to %x:$ (new ds/ss=%x) ...",
	    ptoseg(sysicodestart), i));
	resetsegs(ptoseg(sysicodestart), i);
	DBPRT(1,("done\n"));

#ifdef DEBUG
#ifdef CALLDEBUGGER
	debug();		/* ok to call debugger; code won't move again */
#endif
#endif

	/* returns next free paragraph after Dos image */
	i = BiosReadDos(dosloadseg, i);
	DBPRT(2,("Dos end=%x, size=%x\n", i, (i - dosloadseg) << PARASHIFT));

	endaddr = sotop(i, 0);
	getexeinfo(dosloadseg, SysInitVars.si_finalseg, 0, &endaddr);

	FP_SEG(DosInit) =
	    ptoseg(seginfo[FP_SEG(newexe.ne_csip)].si_dstaddr);
	FP_OFF(DosInit) = FP_OFF(newexe.ne_csip);
	DBPRT(1,("DosInit=%lp", DosInit));	/* system debugger adds '\n' */
	DBPRT(~1,("\n"));			/* except in this case */

	for (i = 1; i <= newexe.ne_cseg; i++) {
#ifdef PAGING
		ap->a_paddr = seginfo[i].si_dstaddr;
#else
		ap->a_paddr = seginfo[i].si_dstaddr - HeaderSize;
#endif
		ap->a_flags = A_FIXED | A_CODE | A_MAP;
		if ((segtbl[i].ns_flags & NSTYPE) == NSDATA) {
			ap->a_flags = A_FIXED | A_DATA | A_MAP;
		}
		ap->a_sel = seginfo[i].si_sel;
		ap++;
		applyrel(i);
		copyseg(i);
		clearbss(i);
	}
	MsInitVars.MsInit_ArenaInfo = (struct arena far *) &ArenaInfo[0];

#ifdef PAGING
	endaddr = TILE(endaddr);
	ap->a_paddr = endaddr;			/* free memory */
#else
	endaddr = HDRROUND(endaddr);
	ap->a_paddr = endaddr + HeaderSize;	/* free memory */
#endif
	ap->a_flags = A_FREE;
	ap++;

#ifdef PAGING
	ap->a_paddr = sysidatastart;		/* sysinit's data */
#else
	ap->a_paddr = sysidatastart - HeaderSize; /* sysinit's data */
#endif
	ap->a_flags = A_FIXED | A_DATA | A_SYSINIT;
	MemLimit = ptoseg(ap->a_paddr);
	ap++;

#ifdef PAGING
	ap->a_paddr = sysicodestart;		/* sysinit's code */
#else
	ap->a_paddr = sysicodestart - HeaderSize; /* sysinit's code */
#endif
	ap->a_flags = A_FIXED | A_CODE | A_SYSINIT;
	ap++;

	ap->a_paddr = sysicodestart + sysicodesize;/* screen, roms, etc */
	ap->a_flags = A_FIXED;
	/*
	 * If there is no high memory, make the entry after sysinit into
	 * an end sentinel.
	 */
	if (SysInitVars.si_highmem == 0) {
		ap->a_flags |= A_END;
	} else {
		ap++;

		ap->a_paddr = 1024L*1024L;		/* high memory */
		ap->a_flags = A_FREE;
		ap++;

#ifdef PAGING
		ap->a_paddr = 1024L*1024L +		/* end sentinel */
		    ((long) SysInitVars.si_highmem << KSHIFT);
		ap->a_flags = A_FIXED | A_END;
#else
		ap->a_paddr = 1024L*1024L +		/* end sentinel */
		    ((long) SysInitVars.si_highmem << KSHIFT) - HeaderSize;
		ap->a_flags = A_FREE | A_END;
#endif
	}
}


#ifdef DEBUG

dumpmsvars()
{
	register int i, flags;
	long paddr;

	DBPRT(2,(" devlist=%lp, DosInit=%lp\n",
	    MsInitVars.MsInit_DeviceList, DosInit));
	if ((sysDebug & 2) == 0) {
		return;
	}
	dprintf("ArenaInfo:\n");
	for (i = 0; i < ARENASIZE; i++) {
		flags = MsInitVars.MsInit_ArenaInfo[i].a_flags;
		paddr = MsInitVars.MsInit_ArenaInfo[i].a_paddr;
		dprintf("  paddr=%lx, size=%lx, sel=%x, flags=%x",
			paddr,
			(flags & A_END)?
#ifdef PAGING
			    0L :
#else
			    (long) HeaderSize :
#endif
			    MsInitVars.MsInit_ArenaInfo[i+1].a_paddr - paddr,
			MsInitVars.MsInit_ArenaInfo[i].a_sel,
			flags);
		if (flags & A_FIXED) {
			dprintf(" FIXED");
		}
		if (flags & A_FREE) {
			dprintf(" FREE");
		}
		if (flags & A_END) {
			dprintf(" END");
		}
		if (flags & A_SWAPPABLE) {
			dprintf(" SWAPPABLE");
		}
		if (flags & A_DISCARDABLE) {
			dprintf(" DISCARDABLE");
		}
		if (flags & A_CODE) {
			dprintf(" CODE");
		}
		if (flags & A_DATA) {
			dprintf(" DATA");
		}
		if (flags & A_SYSINIT) {
			dprintf(" SYSINIT");
		}
		if (flags & A_MAP) {
			dprintf(" MAP");
		}
		dprintf("\n");
		if (flags & A_END) {
			break;
		}
	}
}

#endif


/*
 * editddhdr - Edit real mode segment values in all headers
 *		contained in this device driver module.
 */
void
editddhdr(p, dataseg, codeseg)
struct SysDev far *p;
unsigned short dataseg;
unsigned short codeseg;
{
	register int i;

	while (FP_OFF(p) != 0xffff) {
		DBPRT(1,("Driver='"));
#ifdef DEBUG
		if (p->SDevAtt & DEV_CHAR_DEV) {
			for (i = 0; i < sizeof(p->SDevName); i++) {
				DBPRT(1,("%c", p->SDevName[i]));
			}
		} else {
			DBPRT(1,("# devs=%d", p->SDevName[0]));
		}
#endif
		DBPRT(1,("' link=%lp,attr=%x,strat=%x,intr=%x",
			p->SDevNext, p->SDevAtt,
			p->SDevStrat, p->SDevInt));
		if ((p->SDevAtt & DEV_FCNLEV) == DEVLEV_1) {
			p->SDevRealDS = dataseg;
			p->SDevRealCS = codeseg;
			DBPRT(1,(",ds/cs=%x/%x",
				p->SDevRealDS, p->SDevRealCS));
		}
		DBPRT(1,("\n"));
		p = p->SDevNext;
	}
}


/*
 * getexeinfo - Initialize global data structures to describe a load image.
 */
void
getexeinfo(srcseg, dstseg, bios, endaddrp)
int srcseg;
int dstseg;
int bios;
long *endaddrp;
{
	register int i;
	register struct seginfo *sp;
	long delta, srcaddr, endaddr, dstaddr;
	char far *p;			/* pointer into loaded image */
	static char *exenames[2] = { "DOS", "BIOS" };

	p = sotofar(srcseg, 0);			/* point to EXE header */
	fcopy(p, (char far *) &exehdr, sizeof(exehdr));
	if (exehdr.e_magic != EMAGIC) {
		DBPRT(-1,("%s is not an EXE file\n", exenames[bios]));
		fatal();
	}
	p += (unsigned) exehdr.e_lfanew;	/* point to new EXE header */
	fcopy(p, (char far *) &newexe, sizeof(newexe));
	if (newexe.ne_magic != NEMAGIC) {
		DBPRT(-1,("%s is old EXE file\n", exenames[bios]));
		fatal();
	}
	DBPRT(1,("%s is new EXE file\n", exenames[bios]));
	if (newexe.ne_cseg > SEGMAX) {
		DBPRT(-1,("%s has too many segments\n", exenames[bios]));
		fatal();
	}
	fcopy(p + newexe.ne_segtab, (char far *) &segtbl[1],
				    newexe.ne_cseg*sizeof(struct new_seg));

	/*
	 *  Assume the segments get copied back to dstaddr,
	 *  and set the destination physical address accordingly.
	 */
	dstaddr = sotop(dstseg, 0);
	delta = 0;
DBPRT(1,("seg loadseg srcseg  seg/sel    reladdr     psize/vsize     delta\n"));
/*	 "d:    xxxx   xxxx  xxxx/xxxx  xxxxxxxx  xxxxxxxx/xxxxxxxx  xxxxxxxx"*/
	for (sp = &seginfo[1]; sp <= &seginfo[newexe.ne_cseg]; sp++) {
		i = sp - seginfo;
		if ((int) (sp->si_psize = segtbl[i].ns_cbseg) == 0 &&
		    segtbl[i].ns_sector) {
			sp->si_psize = 0x10000L;
		}
		if ((int) (sp->si_vsize = segtbl[i].ns_minalloc) == 0) {
			sp->si_vsize = 0x10000L;
		}
		sp->si_loadseg =
		    segtbl[i].ns_sector << (newexe.ne_align - PARASHIFT);
		if (sp->si_loadseg) {
			sp->si_loadseg += srcseg;
		}
		sp->si_srcaddr = (long) sp->si_loadseg << PARASHIFT;
#ifdef PAGING
		if (!bios && !(segtbl[i].ns_flags & NSTILE))
		    dstaddr = TILE(dstaddr);	/* Round to page boundary */
#else
		dstaddr += HeaderSize;
#endif
		if (bios || (segtbl[i].ns_flags & NSTILE)) {
			dstaddr = TILE(dstaddr);	    /* tile segment */
			sp->si_sel = ptoseg(dstaddr);
		} else {
			sp->si_sel = (SelBase += SELINCR);  /* don't tile */
		}
		sp->si_dstaddr = dstaddr;
		if (sp->si_loadseg) {
			if (dstaddr > sp->si_srcaddr) {
				if (delta < dstaddr - sp->si_srcaddr) {
					delta = dstaddr - sp->si_srcaddr;
				}
			}
			sp->si_srcaddr += delta;
		}
#ifdef PAGING
		dstaddr += sp->si_vsize;
#else
		dstaddr += HDRROUND(sp->si_vsize);
#endif
#ifdef DEBUG
		if (i < 10) {			/* fake up "%2d" */
			DBPRT(1, (" "));
		}
#endif
		DBPRT(1, ("%d:   %x   %x  %x/%x  %lx  %lx/%lx  %lx\n",
		    i,
		    sp->si_loadseg,
		    ptoseg(sp->si_srcaddr),
		    ptoseg(sp->si_dstaddr), sp->si_sel,
		    sp->si_loadseg? sp->si_dstaddr + sp->si_psize : 0L,
		    sp->si_psize, sp->si_vsize,
		    delta));
#ifdef PAGING
		if (!bios && !(segtbl[i].ns_flags & NSTILE))
		    dstaddr = TILE(dstaddr);	/* Round to page boundary */
#endif
	}

	endaddr = *endaddrp;
	*endaddrp = dstaddr;
	DBPRT(2,("endaddr=%lx -> %lx, delta=%lx\n", endaddr, dstaddr, delta));
	if (delta) {
	    if (bios) {
		DBPRT(-1,("BIOS has bss\n"));
		fatal();
	    }
	    for (sp = &seginfo[newexe.ne_cseg]; sp >= &seginfo[1]; sp--) {
		if (sp->si_loadseg) {
		    srcaddr = sotop(sp->si_loadseg, 0);
		    dstaddr = sp->si_srcaddr;
		    if (srcaddr != dstaddr) {
			DBPRT(2,
			    ("copyback seg %d src=%x dst=%x sz=%lx delta=%lx\n",
			    sp - seginfo,
			    ptoseg(srcaddr),
			    ptoseg(dstaddr),
			    endaddr - srcaddr,
			    srcaddr - dstaddr));
			copyback(srcaddr, dstaddr, endaddr - srcaddr);
		    }
		    endaddr = srcaddr;
		}
	    }
	}
}


/*
 * init - return the far real mode initial stack address extracted
 *	from the load image at LOADSEG.  The stack segment must match the
 *	autodata segment.  This routine is called prior to setting up ds,
 *	and so must not use any global variables until ds is set to DGROUP.
 *	In fact, this routine temporarily SETS UP DS TO BE LOADSEG, in order
 *	to point to the exe header with near pointers.
 *
 *	When done, ds is set to DGROUP, and the global HeaderSize is set
 *	from the heap size field in the new exe header.
 */
char far *
init()
{
	struct exe_hdr *ep = (struct exe_hdr *) 0;
	register struct new_exe *np;
	register struct new_seg *sp;
	int bsssize, headersize, nohighmem;
	char far *stack;

	setds(LOADSEG); 			/* set ds to LOADSEG */
	if (ep->e_magic != EMAGIC) {
		goto badexe;
	}
	    /* point to new exe header */
	np = (struct new_exe *) (unsigned) ep->e_lfanew;
	if (np->ne_magic != NEMAGIC ||
	    np->ne_autodata != FP_SEG(np->ne_sssp) ||
	    np->ne_autodata != np->ne_cseg) {
		goto badexe;
	}
	    /* point to autodata segment table entry */
	sp = (struct new_seg *)((char *)np + np->ne_segtab) + np->ne_autodata-1;

	FP_SEG(stack) = LOADSEG + (sp->ns_sector << (np->ne_align - PARASHIFT));
	FP_OFF(stack) = 0;
	if (sp->ns_flags & NSRELOC) {
		goto badexe;			/* DGROUP can't have relocs */
	}
	if (bsssize = sp->ns_minalloc - sp->ns_cbseg) {
		clearseg(stack + sp->ns_cbseg, bsssize);
	}
	if ((FP_OFF(stack) = FP_OFF(np->ne_sssp)) == 0) {
		goto badexe;
	}
	headersize = np->ne_heap;
	nohighmem = ep->e_bbits;
	setds(FP_SEG(stack));			/* set ds to DGROUP/autodata */
	/*
	 * Now global variables are addressable, but DBPRTs are
	 * not usable until the stack is set up so that ds == ss.
	 */
#ifndef PAGING
	HeaderSize = headersize;
#endif
	NoHighMem = nohighmem;
	return(stack);
badexe:
	fatal();
	/*NOTREACHED*/
}


/*
 * applyrel - Apply relocation records to one segment of a load image.
 */
applyrel(seg)
int seg;
{
	register int sel;
	register int off;
	unsigned chain;
	long segaddr;
	long reladdr;
	long psize;
	int far *sp;
	struct new_rlcinfo rinfo;
	struct new_rlc nreloc;

	if (newexe.ne_magic != NEMAGIC) {
		DBPRT(-1,("bad EXE type\n"));
		fatal();
	}
	DBPRT(6,("Applying relocation to seg %d\n", seg));
	if ((segtbl[seg].ns_flags & NSRELOC) == 0) {
		return;
	}
	if (seginfo[seg].si_loadseg == 0) {
		DBPRT(-1,("relocs on empty seg\n"));
		fatal();
	}
	segaddr = seginfo[seg].si_srcaddr;
	psize = seginfo[seg].si_psize;
	reladdr = segaddr + psize;
	rinfo = *(struct new_rlcinfo far *) ptofar(reladdr);
	reladdr += sizeof(rinfo);

	while (rinfo.nr_nreloc-- > 0) {
		nreloc = *(struct new_rlc far *) ptofar(reladdr);
		reladdr += sizeof(nreloc);
#ifdef DEBUG
		if (sysDebug & 4) {
			switch (nreloc.nr_stype & NRSTYP) {
				case NRSSEG:  dprintf("  segment");  break;
				case NRSPTR:  dprintf("  pointer");  break;
				case NRSOFF:  dprintf("  offset ");  break;
			}
		}
#endif
		if (nreloc.nr_flags & NRADD) {
			DBPRT(-1,("additive reloc"));
			fatal();
		}
		switch (nreloc.nr_flags & NRRTYP) {
			case NRRINT:  break;
			case NRRORD:  DBPRT(-1,("by ordinal")); fatal();
			case NRRNAM:  DBPRT(-1,("by name"));	fatal();
			case NROSFIX: DBPRT(-1,("OS fixup"));	fatal();
		}
		sel = nreloc.nr_union.nr_intref.nr_segno & 0xff;
		if (sel == 0xff) {
			DBPRT(-1,("movable"));
			fatal();
		}
		sel = seginfo[sel].si_sel;
		off = nreloc.nr_union.nr_intref.nr_entry;
		DBPRT(4,(" %x:%x  fixed", sel, off));
		chain = nreloc.nr_soff;
		while (chain != 0xffff) {
			DBPRT(4,("  @%x", chain));
			sp = (int far *) ptofar(segaddr + chain);
			chain = *sp;
			checkoffset(FP_OFF(sp), psize);
			switch (nreloc.nr_stype & NRSTYP) {
				case NRSSEG:
					*sp = sel;
					break;
				case NRSPTR:
					*sp++ = off;
					checkoffset(FP_OFF(sp), psize);
					*sp = sel;
					break;
				case NRSOFF:
					*sp = off;
					break;
				default:
					DBPRT(-1,("bad reloc type"));
					fatal();
			}
		}
		DBPRT(4,("\n"));
	}
}


/*
 * checkoffset - ensure that a fixup offset is within the initialized
 *	 portion of the segment.
 */
checkoffset(off, psize)
unsigned off;
long psize;
{
	if (((long) off) + 2 > psize) {
		DBPRT(-1,("fixup offset > psize"));
		fatal();
	}
}


/*
 * copyseg - Move one segment of a load image into final memory location.
 */
copyseg(seg)
int seg;
{
	register unsigned s;
	long srcaddr, dstaddr;
	long count;

	srcaddr = seginfo[seg].si_srcaddr;
	dstaddr = seginfo[seg].si_dstaddr;
	count = seginfo[seg].si_psize;
	if (srcaddr != dstaddr && count) {
	    while (count > 0) {
		s = (count > 0x8000L)? 0x8000 : (unsigned) count;
		DBPRT(2,("copyseg(seg=%d, src=%lp, dst=%lp, s=%x)\n",
			    seg, ptofar(srcaddr), ptofar(dstaddr), s));
		fcopy(ptofar(srcaddr), ptofar(dstaddr), s);
		srcaddr += s;
		dstaddr += s;
		count -= s;
	    }
#ifdef DEBUG
	} else {
	    DBPRT(2,("copyseg(seg=%d, src=dst=%lp, s=%lx)\n",
		    seg, ptofar(srcaddr), count));
#endif
	}
}


/*
 * clearbss - Clear memory not explicitly intialized by the load image.
 */
clearbss(seg)
int seg;
{
	unsigned size;
	long addr;

	addr = seginfo[seg].si_dstaddr + seginfo[seg].si_psize;
	size = (unsigned) (seginfo[seg].si_vsize - seginfo[seg].si_psize);
	if (size) {
		DBPRT(2,("clearseg(seg=%d, %lp, %x)\n",seg,ptofar(addr),size));
		clearseg(ptofar(addr), size);
	}
}


/*
 * copyback - Spread out a load image to avoid later copyseg conflicts.
 */
copyback(srcaddr, dstaddr, count)
long srcaddr;
long dstaddr;
long count;
{
	register unsigned s;

	srcaddr += count;
	dstaddr += count;
	while (count > 0) {
		s = (count > 0x8000L)? 0x8000 : (unsigned) count;
		srcaddr -= s;
		dstaddr -= s;
		count -= s;
		DBPRT(2,("copyback(src=%lp, dst=%lp, s=%x)\n",
			    ptofar(srcaddr), ptofar(dstaddr), s));
		bcopy(ptofar(srcaddr), ptofar(dstaddr), s);