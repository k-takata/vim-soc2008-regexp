/* vi:set ts=8 sts=4 sw=4:
 *
 * NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE
 *
 * This is NOT the original regular expression code as written by Henry
 * Spencer.  This code has been modified specifically for use with Vim, and
 * should not be used apart from compiling Vim.  If you want a good regular
 * expression library, get the original code.
 *
 * NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE
 */

#ifndef _REGEXP_H
#define _REGEXP_H

/*
 * The number of sub-matches is limited to 10.
 * The first one (index 0) is the whole match, referenced with "\0".
 * The second one (index 1) is the first sub-match, referenced with "\1".
 * This goes up to the tenth (index 9), referenced with "\9".
 */
#define NSUBEXP  10

/*
 * In the NFA engine: how many braces are allowed. 
 * TODO: Use dynamic memory allocation instead of static, like here
 */
#define NFA_MAX_BRACES 20

typedef struct regengine regengine_T;

typedef struct thread Thread;
/*
 * Structure returned by vim_regcomp() to pass on to vim_regexec().
 * This is the general structure. For the actual matcher, two specific
 * structures are used. See code below.
 */
typedef struct regprog
{
    regengine_T		*engine;
    unsigned		regflags;
} regprog_T;

/*
 * Structure used by the back track matcher.
 * These fields are only to be used in regexp.c!
 * See regexp.c for an explanation.
 */
typedef struct
{
    regengine_T		*engine;	/* These two members must sit here */
    unsigned		regflags;

    int			regstart;
    char_u		reganch;
    char_u		*regmust;
    int			regmlen;
    char_u		reghasz;
    char_u		program[1];	/* actually longer.. */
} bt_regprog_T;

/* Linked list. Each node in the list contains an int vector, counters for each node in the NFA */
struct clist_t
{
    int                 *counters;         /* Vector of counters */
    struct clist_t      *next;
};
typedef struct clist_t clist_T;

/*
 * Stucture representating a NFA state.
 * A NFA state may have no outgoing edge, when it is a NFA_MATCH state.
 */
typedef struct nfa_state nfa_state_T;
struct nfa_state
{
    int			c;              /* The most important field: the character to be matched */
    nfa_state_T		*out;
    nfa_state_T		*out1;
    int			id;
    int			lastlist;
    int			visits;
    Thread		*lastthread;
    clist_T             *clist;
};

/*
 * Structure used by the NFA matcher.
 */
typedef struct
{
    regengine_T		*engine;	/* These two members must sit here */
    unsigned		regflags;

    regprog_T		regprog;
    nfa_state_T		*start;
    int			nstate;
    nfa_state_T		state[0];	/* actually longer.. */
} nfa_regprog_T;

/*
 * Structure to be used for single-line matching.
 * Sub-match "no" starts at "startp[no]" and ends just before "endp[no]".
 * When there is no match, the pointer is NULL.
 */
typedef struct
{
    regprog_T		*regprog;
    char_u		*startp[NSUBEXP];
    char_u		*endp[NSUBEXP];
    int			rm_ic;
} regmatch_T;

/*
 * Structure to be used for multi-line matching.
 * Sub-match "no" starts in line "startpos[no].lnum" column "startpos[no].col"
 * and ends in line "endpos[no].lnum" just before column "endpos[no].col".
 * The line numbers are relative to the first line, thus startpos[0].lnum is
 * always 0.
 * When there is no match, the line number is -1.
 */
typedef struct
{
    regprog_T		*regprog;
    lpos_T		startpos[NSUBEXP];
    lpos_T		endpos[NSUBEXP];
    int			rmm_ic;
    colnr_T		rmm_maxcol;	/* when not zero: maximum column */
} regmmatch_T;

/*
 * Structure used to store external references: "\z\(\)" to "\z\1".
 * Use a reference count to avoid the need to copy this around.  When it goes
 * from 1 to zero the matches need to be freed.
 */
typedef struct
{
    short		refcnt;
    char_u		*matches[NSUBEXP];
} reg_extmatch_T;

struct regengine
{
    regprog_T		*(*regcomp)(char_u*, int);
    int         	(*regexec)(regmatch_T*, char_u*, colnr_T);
    int         	(*regexec_nl)(regmatch_T*, char_u*, colnr_T);
    long        	(*regexec_multi)(regmmatch_T*, win_T*, buf_T*, linenr_T, colnr_T, proftime_T*);
};

#endif	/* _REGEXP_H */
