/*
 * Replacement for free() that ignores NULL pointers.
 * Also skip free() when exiting for sure, this helps when we caught a deadly
 * signal that was caused by a crash in free().
 */
    void
vim_free(x)
    void *x;
{
    if (x != NULL && !really_exiting)
    {
#ifdef MEM_PROFILE
	mem_pre_free(&x);
#endif
	free(x);
    }
}

void * vim_memset(ptr, c, size)
    void    *ptr;
    int	    c;
    size_t  size;
{
    char *p = ptr;

    while (size-- > 0)
	*p++ = c;
    return ptr;
}

/*
 * Low level memory allocation function.
 * This is used often, KEEP IT FAST!
 */
    char_u *
lalloc(size, message)
    long_u	size;
    int		message;
{
    char_u	*p;		    /* pointer to new storage space */
    static int	releasing = FALSE;  /* don't do mf_release_all() recursive */
    int		try_again;
#if defined(HAVE_AVAIL_MEM) && !defined(SMALL_MEM)
    static long_u allocated = 0;    /* allocated since last avail check */
#endif

    /* Safety check for allocating zero bytes */
    if (size == 0)
    {
	/* Don't hide this message */
	emsg_silent = 0;
	EMSGN(_("E341: Internal error: lalloc(%ld, )"), size);
	return NULL;
    }

#ifdef MEM_PROFILE
    mem_pre_alloc_l(&size);
#endif

#if defined(MSDOS) && !defined(DJGPP)
    if (size >= 0xfff0)		/* in MSDOS we can't deal with >64K blocks */
	p = NULL;
    else
#endif

    /*
     * Loop when out of memory: Try to release some memfile blocks and
     * if some blocks are released call malloc again.
     */
    for (;;)
    {
	/*
	 * Handle three kind of systems:
	 * 1. No check for available memory: Just return.
	 * 2. Slow check for available memory: call mch_avail_mem() after
	 *    allocating KEEP_ROOM amount of memory.
	 * 3. Strict check for available memory: call mch_avail_mem()
	 */
	if ((p = (char_u *)malloc((size_t)size)) != NULL)
	{
#ifndef HAVE_AVAIL_MEM
	    /* 1. No check for available memory: Just return. */
	    goto theend;
#else
# ifndef SMALL_MEM
	    /* 2. Slow check for available memory: call mch_avail_mem() after
	     *    allocating (KEEP_ROOM / 2) amount of memory. */
	    allocated += size;
	    if (allocated < KEEP_ROOM / 2)
		goto theend;
	    allocated = 0;
# endif
	    /* 3. check for available memory: call mch_avail_mem() */
	    if (mch_avail_mem(TRUE) < KEEP_ROOM && !releasing)
	    {
		vim_free((char *)p);	/* System is low... no go! */
		p = NULL;
	    }
	    else
		goto theend;
#endif
	}
	/*
	 * Remember that mf_release_all() is being called to avoid an endless
	 * loop, because mf_release_all() may call alloc() recursively.
	 */
	if (releasing)
	    break;
	releasing = TRUE;

	clear_sb_text();	      /* free any scrollback text */
	try_again = mf_release_all(); /* release as many blocks as possible */
#ifdef FEAT_EVAL
	try_again |= garbage_collect(); /* cleanup recursive lists/dicts */
#endif

	releasing = FALSE;
	if (!try_again)
	    break;
    }

    if (message && p == NULL)
	do_outofmem_msg(size);

theend:
#ifdef MEM_PROFILE
    mem_post_alloc((void **)&p, (size_t)size);
#endif
    return p;
}

