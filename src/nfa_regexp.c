
/******************** Below are NFA regexp *********************/
/* File nfa_regexp.c is included automa[gt]ically in regexp.c, at the end. */


/* Upper limit allowed for {m,n} repetitions handled by NFA */
#define	    NFA_BRACES_MAXLIMIT	    10	    
/* For allocating space for the postfix representation */
#define	    NFA_POSTFIX_MULTIPLIER    (NFA_BRACES_MAXLIMIT+2)*2	    

#ifdef DEBUG
static void nfa_postfix_dump __ARGS((char_u *expr, int retval));
static void nfa_dump __ARGS((nfa_regprog_T *prog));
FILE	    *f;
#endif

/* Global variable, set during compilation of a regexp, when an error is encountered. 
 * If TRUE, we revert to the backtracking engine. If false, then the error 
 * should have been handled by the NFA engine. Syntax errors are handled by NFA engine. */
static int syntax_error = FALSE;
static int nfa_calc_size = FALSE;
static int nfa_paren_offset = 0;

static int *post_start;  /* holds the postfix form of r.e. */
static int *post_end;
static int *post_ptr;


static int nstate;	/* Number of states in the NFA. */
static int istate;	/* Index in the state vector, used in new_state */
static int nstate_max;	/* Upper bound of estimated number of states. */

static int nfa_just_found_braces = FALSE;  
static int nfa_gate_offset = 0;


/* Given a regexp, return how many \{} operators are present.
* Note that syntax check will be done at a later stage, during compilation. This will only give an upper bound.
*/
/*
static int
nfa_count_repetitions(expr)
		char_u	*expr;
{
char_u *p;
int count = 0;
	for (p = expr; *p; p++)
// 		if (*p == '{')
			count++;
	return count;
}
*/

/*
 * Initialize internal variables before NFA compilation.
 * Return OK on success, FAIL otherwise.
 */
    static int
nfa_regcomp_start(expr, re_flags)
    char_u	*expr;
    int		re_flags;	    /* see vim_regcomp() */
{
    int		postfix_size;

    nstate	= 0;
    istate	= 0;
    nfa_gate_offset = 0;
    nfa_paren_offset = 0;
    nstate_max	= (STRLEN(expr) + 1) * NFA_POSTFIX_MULTIPLIER; /* A reasonable estimation. */

    postfix_size = sizeof(*post_start) * nstate_max;	/* Size for postfix representation of expr */
    post_start = (int *)lalloc(postfix_size, TRUE);
    if (post_start == NULL)
        return FAIL;
    vim_memset(post_start, 0, postfix_size);
    post_ptr = post_start;
    post_end = post_start + postfix_size;

    regcomp_start(expr, re_flags);

    return OK;
}

/* helper fuctions used when doing re2post() parsing */
#define EMIT(c)	do {				\
		    if (post_ptr >= post_end)	\
			return FAIL;		\
		    *post_ptr++ = c;		\
		} while (0)

/*
 * Code to parse regular expression.
 *
 * We try to reuse parsing functions above to
 * minimize surprise and keep the syntax consistent.
 */

static int nfa_reg(int paren);

/*
 *  nfa_regatom - the lowest level
 *
 *  An atom can be one of a long list of items.  Many atoms match one character
 *  in the text.  It is often an ordinary character or a character class.
 *  Braces can be used to make a pattern into an atom.  The "\z(\)" construct
 *  is only for syntax highlighting.
 *
 *  atom    ::=     ordinary-atom
 *      or  \( pattern \)
 *      or  \%( pattern \)
 *      or  \z( pattern \)
 */

    static int
nfa_regatom()
{
    int		c;
    char_u	*p, *rp;
    int		extra = 0;

    c = getchr();
    /* NFA engine doesn't yet support mbyte composing chars => bug when search mbyte chars.
     * Fail and revert to old engine */
    if ((*mb_char2len)(c)>1)
        return FAIL;	    /* unsupported for now */
    switch (c)
    {
		case Magic('^'):
			EMIT(NFA_BOL);
			break;

		case Magic('$'):
			EMIT(NFA_EOL);
#if defined(FEAT_SYN_HL) || defined(PROTO)
			had_eol = TRUE;
#endif
			break;

		case Magic('<'):
			EMIT(NFA_BOW);
			break;

		case Magic('>'):
			EMIT(NFA_EOW);
			break;

		case Magic('_'):
			c = no_Magic(getchr());
			if (c == '^')	/* "\_^" is start-of-line */
			{
			EMIT(NFA_BOL);
			break;
			}
			if (c == '$')	/* "\_$" is end-of-line */
			{
			EMIT(NFA_EOL);
#if defined(FEAT_SYN_HL) || defined(PROTO)
			had_eol = TRUE;
#endif
			break;
			}

			return FAIL;	/* TODO(RE) Why fail here? */

			extra = ADD_NL;

			/* "\_[" is character range plus newline */
				if (c == '[')
			/* not supported yet */
			return FAIL;

		/* "\_x" is character class plus newline */
		/*FALLTHROUGH*/

		/*
		 * Character classes.
		 */
		case Magic('.'):
		case Magic('i'):
		case Magic('I'):
		case Magic('k'):
		case Magic('K'):
		case Magic('f'):
		case Magic('F'):
		case Magic('p'):
		case Magic('P'):
		case Magic('s'):
		case Magic('S'):
		case Magic('d'):
		case Magic('D'):
		case Magic('x'):
		case Magic('X'):
		case Magic('o'):
		case Magic('O'):
		case Magic('w'):
		case Magic('W'):
		case Magic('h'):
		case Magic('H'):
		case Magic('a'):
		case Magic('A'):
		case Magic('l'):
		case Magic('L'):
		case Magic('u'):
		case Magic('U'):
			p = vim_strchr(classchars, no_Magic(c));
			if (p == NULL)
			{
			return FAIL;	    /* runtime error */
			}
#ifdef FEAT_MBYTE
			/* When '.' is followed by a composing char ignore the dot, so that
			 * the composing char is matched here. */
			if (enc_utf8 && c == Magic('.') && utf_iscomposing(peekchr()))
			{
				c = getchr();
				goto nfa_do_multibyte;
			}
#endif
			/* only '.' is supported for now */
			if (c == Magic('.'))
			{
			EMIT(nfa_classcodes[p - classchars] + extra);
			break;
			}
			else
			return FAIL;	    /* unsupported */

		case Magic('n'):
			if (reg_string)
			/* In a string "\n" matches a newline character. */
			EMIT(NL);
			else
			{
			/* In buffer text "\n" matches the end of a line. */
			EMIT(NFA_NEWL);
			regflags |= RF_HASNL;
			}
			break;

		case Magic('('):
			if (nfa_reg(REG_PAREN) == FAIL)
			return FAIL;	    /* cascaded error */
			break;

		case NUL:
		case Magic('|'):
		case Magic('&'):
		case Magic(')'):
			EMSG("NFA regexp: Misplaced closing ')' ");
			syntax_error = TRUE;
			return FAIL;

		case Magic('='):
		case Magic('?'):
		case Magic('+'):
		case Magic('@'):
		case Magic('*'):
		case Magic('{'):
			EMSG("NFA regexp: Misplaced =?+@*{");
			syntax_error = TRUE;
			return FAIL;		/* these should follow an atom, not form an atom */

		case Magic('~'):		/* previous substitute pattern */
			/* Not supported yet */
			return FAIL;

		case Magic('1'):
		case Magic('2'):
		case Magic('3'):
		case Magic('4'):
		case Magic('5'):
		case Magic('6'):
		case Magic('7'):
		case Magic('8'):
		case Magic('9'):
			  /* not supported yet */
			  return FAIL;

		case Magic('z'):
		case Magic('%'):
			  /* not supported yet */
			  return FAIL;

		case Magic('['):
			p = regparse;
			rp = skip_anyof(regparse);		/* Skip over [] */
			if (*rp == ']')					/* there is a matching ']' */
			{
				if (*p == '^' || 			/* negated range */
					((get_char_class(&p) != CLASS_NONE)
					&& (get_equi_class(&p) != 0)
					&& (get_coll_element(&p) != 0)))
				{
#ifdef DEBUG
					EMSG("NFA regexp: Negated character ranges and collating char classes are not yet supported ");
#endif
					/* not supported yet */
					return FAIL;
				}

				EMIT(*p++);
				while (p < rp)
				{
					EMIT(*p);
					EMIT(NFA_OR);
					p++;
				}
			}
			else 
			if (reg_strict)
			{
				EMSG_M_RET_NULL(_("E769: Missing ] after %s["), reg_magic > MAGIC_OFF);
				syntax_error = TRUE;
				return FAIL;
			}	
			regparse = rp+1;		/* skip the trailing ] */

			return OK;

		/* FALLTHROUGH */
		default:
		{
#ifdef FEAT_MBYTE
			/* A multi-byte character is handled as a separate atom if it's
			 * before a multi and when it's a composing char. */
	nfa_do_multibyte:
			if (has_mbyte && (*mb_char2len)(c) > 1
					 && (enc_utf8 && utf_iscomposing(c)))
			{

			/* composing char not supported yet */
			return FAIL;
			}
#endif

#ifdef FEAT_MBYTE
			/* composing char not supported yet */
#endif
			EMIT(c);

			return OK;
		}
    }
    return OK;
}

/*
 * regpiece - something followed by possible [*+=]
 *
 * A piece is an atom, possibly followed by a multi, an indication of how many
 * times the atom can be matched.  Example: "a*" matches any sequence of "a"
 * characters: "", "a", "aa", etc.
 *
 * piece   ::=	    atom
 * 	or  atom  multi
 *
 */
    static int
nfa_regpiece()
{
    int		i;
    int		op;
    int		ret;
    long 	minval, maxval;
    int         greedy = TRUE;      /* Braces are prefixed with '-' ? */
    char_u	*old_regparse, *new_regparse;
	char	c2;
    int		*old_post_ptr, *my_post_start;
    int		old_regnpar;

    nfa_just_found_braces = FALSE;
    /* Save the current position in the regexp, so that we can use it if <atom>{m,n} is next. */
    old_regparse = regparse;
    /* Save current number of open paranthesis, so we can use it if <atom>{m,n} is next */
    old_regnpar = regnpar;
    my_post_start = post_ptr;	    /* store current pos in the postfix form, for \{m,n} involving 0s */
   
    ret = nfa_regatom();
    if (ret == FAIL)
	return FAIL;	    /* cascaded error */

    op = peekchr();
    if (re_multi_type(op) == NOT_MULTI)
	return OK;

    skipchr();
    switch (op)
    {
	case Magic('*'):
	    EMIT(NFA_STAR);
	    break;

	case Magic('+'):
	    EMIT(NFA_PLUS);
	    break;

	case Magic('@'):
	    /* Not supported yet */
	    return FAIL;

	case Magic('?'):
	case Magic('='):
	    EMIT(NFA_QUEST);
	    break;

	case Magic('{'):
	    /* a{2,5} will expand to 'aaa?a?a?'
	     * a{-1,3} will expand to 'aa??a??', where ?? is the nongreedy version of '?' 
	     * \v(ab){2,3} will expand to '(ab)(ab)(ab)?', where all the paranthesis have the same code
	     */

	    greedy = TRUE;
	    c2 = peekchr();
            if (c2 == '-')
            {
                skipchr();
                greedy = FALSE;
            }
	    if (!read_limits(&minval, &maxval))
	    {
		EMSG("NFA regexp: Error reading repetition limits");
		syntax_error = TRUE;
		return FAIL;
	    }
	    if (maxval > NFA_BRACES_MAXLIMIT)
	    {
		/* This would yield a huge automaton and use too much memory.
		 * Revert to old engine */
		return FAIL;
	    }

	    /* Special case: x{0} or x{-0} */
	    if (maxval == 0)
	    {
		post_ptr = my_post_start;	/* Ignore result of previous call to nfa_regatom() */
		EMIT(NFA_SKIP_CHAR);		/* NFA_SKIP_CHAR match has 0-length and works everywhere */
		return OK;
	    }

	    post_ptr = my_post_start;	/* Ignore previous call to nfa_regatom() */
	    new_regparse = regparse;	/* Save pos after the repeated atom and the \{} */
	    
	    new_regparse = regparse;
	    int quest = (greedy == TRUE? NFA_QUEST : NFA_QUEST_NONGREEDY);    /* to emit a \? */
	    for (i = 0; i < maxval; i++)
	    {
		regparse = old_regparse;	/* Goto beginning of the repeated atom */
		regnpar = old_regnpar;		/* Restore count of paranthesis */
		old_post_ptr = post_ptr;
		if (nfa_regatom() == FAIL)
		    return FAIL;
		if (i+1 > minval)	    /* after "minval" times, atoms are optional */
		    EMIT(quest);
		if (old_post_ptr != my_post_start)
		    EMIT(NFA_CONCAT);
	    }

	    regparse = new_regparse;	/* Go to just after the repeated atom and the \{} */
	    
	    break;


	default:
	    break;
    }	/* end switch */

    if (re_multi_type(peekchr()) != NOT_MULTI)
    {
	/* Can't have a multi follow a multi. */
	EMSG("NFA regexp: Can't have a multi follow a multi !");
	syntax_error = TRUE;
	return FAIL;
    }

    return OK;
}

/*
 * A concat is one or more pieces, concatenated.  It matches a match for the
 * first piece, followed by a match for the second piece, etc.  Example:
 * "f[0-9]b", first matches "f", then a digit and then "b".
 *
 * concat  ::=	    piece
 * 	or  piece piece
 * 	or  piece piece piece
 * 	etc.
 */
    static int
nfa_regconcat()
{
    int		cont = TRUE;
    int		first = TRUE;

    while (cont)
    {
	switch (peekchr())
	{
	    case NUL:
	    case Magic('|'):
	    case Magic('&'):
	    case Magic(')'):
		cont = FALSE;
		break;

	    case Magic('Z'):
#ifdef FEAT_MBYTE
		regflags |= RF_ICOMBINE;
#endif
		skipchr_keepstart();
		break;
	    case Magic('c'):
		regflags |= RF_ICASE;
		skipchr_keepstart();
		break;
	    case Magic('C'):
		regflags |= RF_NOICASE;
		skipchr_keepstart();
		break;
            case Magic('v'):
		reg_magic = MAGIC_ALL;
		skipchr_keepstart();
		curchr = -1;
		break;
            case Magic('m'):
                reg_magic = MAGIC_ON;
                skipchr_keepstart();
                curchr = -1;
                break;
            case Magic('M'):
                reg_magic = MAGIC_OFF;
                skipchr_keepstart();
                curchr = -1;
                break;
            case Magic('V'):
                reg_magic = MAGIC_NONE;
                skipchr_keepstart();
                curchr = -1;
                break;

	    default:
		if (nfa_regpiece() == FAIL)
		    return FAIL;
		if (first == TRUE)
		{   
		    if (nfa_just_found_braces)
		    {
			EMSG("NFA regexp:  Can't begina regexp with repetition braces ");
			syntax_error = TRUE;
			return FAIL;
		    }
		    first = FALSE;
		}
		else
		    EMIT(NFA_CONCAT);
		break;
	}
    }

    return OK;
}

/*
 * A branch is one or more concats, separated by "\&".  It matches the last
 * concat, but only if all the preceding concats also match at the same
 * position.  Examples:
 *      "foobeep\&..." matches "foo" in "foobeep".
 *      ".*Peter\&.*Bob" matches in a line containing both "Peter" and "Bob"
 *
 * branch ::=	    concat
 *      	or  concat \& concat
 *      	or  concat \& concat \& concat
 *      	etc.
 *
 */
    static int
nfa_regbranch()
{
    if (nfa_regconcat() == FAIL)
	return FAIL;
    if (peekchr() == Magic('&'))
	/* not supported yet */
	return FAIL;

    return OK;
}

/*
 *  A pattern is one or more branches, separated by "\|".  It matches anything
 *  that matches one of the branches.  Example: "foo\|beep" matches "foo" and
 *  matches "beep".  If more than one branch matches, the first one is used.
 *
 *  pattern ::=	    branch
 *     	or  branch \| branch
 *     	or  branch \| branch \| branch
 *     	etc.
 *
 */
    static int
nfa_reg(paren)
    int		paren;	/* REG_NOPAREN, REG_PAREN, REG_NPAREN or REG_ZPAREN */
{
    int		parno = 0;

#ifdef FEAT_SYN_HL
#endif
    if (paren == REG_PAREN)
    {
	if (regnpar >= NSUBEXP) /* Too many `(' */
	{
	    EMSG("NFA regexp: Too many '('");
	    syntax_error = TRUE;
	    return FAIL;
	}
	parno = regnpar++ - nfa_paren_offset;
    }

    if (nfa_regbranch() == FAIL)
	return FAIL;	    /* cascaded error */

    while (peekchr() == Magic('|'))
    {
	skipchr();
	if (nfa_regbranch() == FAIL)
	    return FAIL;    /* cascaded error */
	EMIT(NFA_OR);
    }

    /* Check for proper termination. */
    if (paren != REG_NOPAREN && getchr() != Magic(')'))
    {
	EMSG("NFA regexp: Group is not properly terminated ");
	syntax_error = TRUE;
	return FAIL;
    }
    else if (paren == REG_NOPAREN && peekchr() != NUL)
    {
	EMSG("NFA regexp: proper termination error ");
	syntax_error = TRUE;
	return FAIL;
    }
    /*
     * Here we set the flag allowing back references to this set of
     * parentheses.
     */
    if (paren == REG_PAREN)
    {
        had_endbrace[parno] = TRUE;     /* have seen the close paren */
	EMIT(NFA_MOPEN + parno);
    }

    return OK;
}

/* TODO: use union to save space? */
typedef struct
{
    char_u	*start[NSUBEXP];
    char_u	*end[NSUBEXP];
    lpos_T	startpos[NSUBEXP];
    lpos_T	endpos[NSUBEXP];
} regsub_T;

#ifdef DEBUG
static char code[50]; 
static FILE *debugf;
static void nfa_set_code(int c)
{
    STRCPY(code, "");
    switch (c)
    {
        case NFA_MATCH: STRCPY(code, "NFA_MATCH "); break;
        case NFA_SPLIT: STRCPY(code, "NFA_SPLIT "); break;
	case NFA_CONCAT: STRCPY(code, "NFA_CONCAT "); break;
	case NFA_MOPEN + 0: 
	case NFA_MOPEN + 1: 
	case NFA_MOPEN + 2: 
	case NFA_MOPEN + 3: 
	case NFA_MOPEN + 4: 
	case NFA_MOPEN + 5: 
	case NFA_MOPEN + 6: 
	case NFA_MOPEN + 7: 
	case NFA_MOPEN + 8: 
	case NFA_MOPEN + 9: 
	    STRCPY(code, "NFA_MOPEN(x)");
	    code[10] = c - NFA_MOPEN + '0';
	    break;
	case NFA_MCLOSE + 0: 
	case NFA_MCLOSE + 1: 
	case NFA_MCLOSE + 2: 
	case NFA_MCLOSE + 3: 
	case NFA_MCLOSE + 4: 
	case NFA_MCLOSE + 5: 
	case NFA_MCLOSE + 6: 
	case NFA_MCLOSE + 7: 
	case NFA_MCLOSE + 8: 
	case NFA_MCLOSE + 9: 
	    STRCPY(code, "NFA_MCLOSE(x)");
	    code[11] = c - NFA_MCLOSE + '0';
	    break;
	case NFA_EOL: STRCPY(code, "NFA_EOL "); break;
	case NFA_BOL: STRCPY(code, "NFA_BOL "); break;
	case NFA_ANY: STRCPY(code, "NFA_ANY "); break;
	case NFA_STAR: STRCPY(code, "NFA_STAR "); break;
	case NFA_PLUS: STRCPY(code, "NFA_PLUS "); break;
	case NFA_SKIP_CHAR: STRCPY(code, "NFA_SKIP_CHAR"); break;
	case NFA_OR: STRCPY(code, "NFA_OR"); break;
	case NFA_QUEST:	STRCPY(code, "NFA_QUEST"); break;
	case NFA_QUEST_NONGREEDY: STRCPY(code, "NFA_QUEST_NON_GREEDY"); break;
        default:    
            STRCPY(code, "CHAR(x)");
            code[5] = c;
    }
    
}

static void nfa_postfix_dump __ARGS((char_u *expr, int retval))
{
    FILE *f;
    f = fopen("LOG.log","a");
    if (f)
    {
	fprintf(f,"\n-------------------------\n");
        if (retval == FAIL)
            fprintf(f,">>> NFA engine failed ... \n");
        else if (retval == OK)
            fprintf(f,">>> NFA engine succeeded !\n");
	fprintf(f,"Regexp: \"%s\"\nPostfix notation (char): \"", expr);
	int *p;
	for (p=post_start; *p; p++)
	{
	    nfa_set_code(*p);
	    fprintf(f,"%s, ", code);
	}
	fprintf(f,"\"\nPostfix notation (int): ");
	for (p=post_start; *p; p++)
		fprintf(f,"%d ", *p);
	fprintf(f, "\n\n");
	fclose(f);
    }
}

static void nfa_print_state(FILE *debugf, nfa_state_T *state, int ident)
{
    if (state == NULL)
	return;

    int i;
    for (i=0;i<ident; i++)
	fprintf(debugf,"%c", ' ');

    nfa_set_code(state->c);
    fprintf(debugf, "%s (%d) (id=%d)\n", code, state->c, abs(state->id));
    if (state->id < 0)
	return;
    state->id *= -1;
    nfa_print_state(debugf, state->out, ident+4);
    nfa_print_state(debugf, state->out1, ident+4);

}

static void nfa_dump(nfa_regprog_T *prog)
{
    debugf=fopen("LOG.log","a");
    if (debugf)
    {
	nfa_print_state(debugf, prog->start, 0);
	fclose(debugf);
    }
}
#endif

/*
 * Parse r.e. @expr and convert it into postfix form.
 * Return the postfix string on success, NULL otherwise.
 */
    static int *
re2post(expr, re_flags)
    char_u	*expr;
    int		re_flags;
{
    if (nfa_reg(REG_NOPAREN) == FAIL)
	return NULL;
    EMIT(NFA_MOPEN);
    return post_start;
}

/* NB. Some of the code below is inspired by Russ's. */

/*
 * Represents an NFA state plus zero or one or two arrows exiting.
 * if c == MATCH, no arrows out; matching state.
 * If c == SPLIT, unlabeled arrows to out and out1 (if != NULL).
 * If c < 256, labeled arrow with character c to out.
 */

static nfa_state_T	*state_ptr; /* points to nfa_prog->state */

/* Allocate and initialize nfa_state_T */
    static nfa_state_T *
new_state(c, out, out1)
    int		c;
    nfa_state_T	*out;
    nfa_state_T	*out1;
{
    nfa_state_T *s;

    if (istate >= nstate)
	return NULL;

    s = &state_ptr[istate++];

    s->c    = c;
    s->out  = out;
    s->out1 = out1;

    s->id   = istate;
    s->lastlist = 0;
    s->lastthread = NULL;
    s->visits = 0;

    return s;
}

/*
 * A partially built NFA without the matching state filled in.
 * Frag.start points at the start state.
 * Frag.out is a list of places that need to be set to the
 * next state for this fragment.
 */
typedef struct Frag Frag;
typedef union Ptrlist Ptrlist;
struct Frag
{
    nfa_state_T   *start;
    Ptrlist	*out;
};

/* Initialize Frag struct. */
    static Frag
frag(start, out)
    nfa_state_T	*start;
    Ptrlist	*out;
{
    Frag n = { start, out };
    return n;
}

/*
 * Since the out pointers in the list are always
 * uninitialized, we use the pointers themselves
 * as storage for the Ptrlists.
 */
union Ptrlist
{
    Ptrlist	*next;
    nfa_state_T	*s;
};

/* Create singleton list containing just outp. */
    static Ptrlist*
list1(outp)
    nfa_state_T	**outp;
{
    Ptrlist *l;

    l = (Ptrlist*)outp;
    l->next = NULL;
    return l;
}

/* Patch the list of states at out to point to start. */
    static void
patch(l, s)
    Ptrlist	*l;
    nfa_state_T	*s;
{
    Ptrlist *next;

    for (; l; l = next)
    {
	next = l->next;
	l->s = s;
    }
}


/* Join the two lists l1 and l2, returning the combination. */
    static Ptrlist*
append(l1, l2)
    Ptrlist *l1;
    Ptrlist *l2;
{
    Ptrlist *oldl1;

    oldl1 = l1;
    while (l1->next)
	l1 = l1->next;
    l1->next = l2;
    return oldl1;
}

/*
 * Stack used for transforming postfix form into NFA.
 */
static Frag stack[1024];

/*
 * Convert a postfix form into its equivalent NFA.
 * Return the NFA start state on success, NULL otherwise.
 */
    static nfa_state_T *
post2nfa(postfix)
    int		*postfix;
{
    int		*p;
    Frag	*stackp, *stack_end, e1, e2, e;
    nfa_state_T	*s, *s1, *matchstate;

    if (postfix == NULL)
        return NULL;

#define PUSH(s)	{			    \
		    if (stackp >= stack_end)\
			return NULL;	    \
		     *stackp++ = s;	    \
		}

#define POP()	({			    \
		    if (stackp <= stack)    \
			return NULL;	    \
		    *--stackp;		    \
		 })

    stackp = stack;
    stack_end = stack + sizeof(stack);

    for (p = postfix; *p; ++p)
    {
	switch (*p)
	{
	/*
	 *	Catenation.
	 *	Pay attention: this operator does not exist
	 *	in the r.e. itself (it is implicit, really).
	 *	It is added when r.e. is translated to postfix
	 *	form in re2post().
	 *
	 *	No new state added here.
	 */
	case NFA_CONCAT:
	    if (nfa_calc_size == TRUE)
	    {
		nstate += 0;
		break;
	    }
	    e2 = POP();
	    e1 = POP();
	    patch(e1.out, e2.start);
	    PUSH(frag(e1.start, e2.out));
	    break;

	case NFA_OR:		/* Alternation */
	    if (nfa_calc_size == TRUE)
	    {
		nstate ++;
		break;
	    }
	    e2 = POP();
	    e1 = POP();
	    s = new_state(NFA_SPLIT, e1.start, e2.start);
	    if (s == NULL)
		return NULL;
	    PUSH(frag(s, append(e1.out, e2.out)));
	    break;

	case NFA_STAR:		/* Zero or more */
	    if (nfa_calc_size == TRUE)
	    {
		nstate ++;
		break;
	    }
	    e = POP();
	    s = new_state(NFA_SPLIT, e.start, NULL);
	    if (s == NULL)
	        return NULL;
	    patch(e.out, s);
	    PUSH(frag(s, list1(&s->out1)));
	    break;

	case NFA_QUEST:		/* One or zero, in this order => greedy match */
	    if (nfa_calc_size == TRUE)
	    {
		nstate ++;
		break;
	    }
	    e = POP();
	    s = new_state(NFA_SPLIT, e.start, NULL);
	    if (s == NULL)
	        return NULL;
	    PUSH(frag(s, append(e.out, list1(&s->out1))));
	    break;

	case NFA_QUEST_NONGREEDY:		/* Zero or one, in this order => non-greedy match */
	    if (nfa_calc_size == TRUE)
	    {
		nstate ++;
		break;
	    }
	    e = POP();
	    s = new_state(NFA_SPLIT, NULL, e.start);
	    if (s == NULL)
	        return NULL;
	    PUSH(frag(s, append(e.out, list1(&s->out1))));
	    break;

	case NFA_PLUS:		/* One or more */
	    if (nfa_calc_size == TRUE)
	    {
		nstate ++;
		break;
	    }
	    e = POP();
	    s = new_state(NFA_SPLIT, e.start, NULL);
	    if (s == NULL)
	        return NULL;
	    patch(e.out, s);
	    PUSH(frag(e.start, list1(&s->out1)));
	    break;
	
	case NFA_SKIP_CHAR:	    /* Symbol of 0-length, Used in a repetition with max/min count of 0 */
	    if (nfa_calc_size == TRUE)
	    {
		nstate ++;
		break;
	    }
	    s = new_state(NFA_SKIP_CHAR, NULL, NULL);
	    PUSH(frag(s, list1(&s->out)));
	    break;

	case NFA_MOPEN + 0:	/* Submatch */
	case NFA_MOPEN + 1:
	case NFA_MOPEN + 2:
	case NFA_MOPEN + 3:
	case NFA_MOPEN + 4:
	case NFA_MOPEN + 5:
	case NFA_MOPEN + 6:
	case NFA_MOPEN + 7:
	case NFA_MOPEN + 8:
	case NFA_MOPEN + 9:
	    if (nfa_calc_size == TRUE)
	    {
		nstate += 2;
		break;
	    }
	    e = POP();
	    s = new_state(*p, e.start, NULL);   /* `(' */
	    if (s == NULL)
		return NULL;

	    s1 = new_state(*p + 10, NULL, NULL);   /* `)' */
	    if (s1 == NULL)
		return NULL;
	    patch(e.out, s1);
	    PUSH(frag(s, list1(&s1->out)));
	    break;

	default:	/* Operands */
	    if (nfa_calc_size == TRUE)
	    {
		nstate ++;
		break;
	    }
	    s = new_state(*p, NULL, NULL);
    	    if (s == NULL)
    	        return NULL;
    	    PUSH(frag(s, list1(&s->out)));
    	    break;
	} /* switch(*p) */
    } /* for(p = postfix; *p; ++p) */

    if (nfa_calc_size == TRUE)
    {
	nstate ++;
	return NULL;	    /* Return value when counting size is ignored anyway */ 
    }

    e = POP();
    if (stackp != stack)
    {
	EMSG("NFA regexp: (While converting from postfix to NFA), too many states left on stack ");
	return NULL;
    }

    if (istate >= nstate)
	return NULL;

    matchstate = &state_ptr[istate++]; /* the match state */
    matchstate->c = NFA_MATCH;

    patch(e.out, matchstate);
    return e.start;

#undef POP
#undef PUSH
}

/* Thread contains runtime information of a NFA state */
struct thread
{
    nfa_state_T	*state;
    regsub_T	sub;
};

typedef struct
{
    Thread	*t;
    int		n;
} List;

static List list[2];

static int	listid;

    static void
addstate(l, state, m, off, lid, match)
    List		*l;	/* runtime state list */
    nfa_state_T		*state;	/* state to update */
    regsub_T		*m;	/* pointers to subexpressions */
    int			off;
    int			lid;
    int			*match;	/* found match? */
{
    regsub_T		save;
    int			subidx = 0;

    if (l == NULL || state == NULL) /* never happen */
	return;

    if (state->lastlist == lid)
    {
	if (++state->visits > 2)
	    return;
    }
    else        /* add the state to the list */
    {
	state->lastlist = lid;
	state->lastthread = &l->t[l->n++];
	state->lastthread->state = state;
	state->lastthread->sub = *m;
    }
#ifdef DEBUG
    nfa_set_code(state->c);
    fprintf(f, "> Adding new state (id = %d) to list %d. Character code %d\n", state->id, lid, state->c);
#endif
    switch (state->c)
    {
	case NFA_MATCH:
	    *match = TRUE;
	    break;

	case NFA_SPLIT:
	    addstate(l, state->out, m, off, lid, match);
	    addstate(l, state->out1, m, off, lid, match);
	    break;
	
	case NFA_SKIP_CHAR:
	    addstate(l, state->out, m, off, lid, match);
	    break;

	case NFA_MOPEN + 0:
	case NFA_MOPEN + 1:
	case NFA_MOPEN + 2:
	case NFA_MOPEN + 3:
	case NFA_MOPEN + 4:
	case NFA_MOPEN + 5:
	case NFA_MOPEN + 6:
	case NFA_MOPEN + 7:
	case NFA_MOPEN + 8:
	case NFA_MOPEN + 9:
	    subidx = state->c - NFA_MOPEN;

	    if (REG_MULTI)
	    {
		save.startpos[subidx] = m->startpos[subidx];
		save.endpos[subidx] = m->endpos[subidx];
	        m->startpos[subidx].lnum = reglnum;
	        m->startpos[subidx].col = reginput - regline + off;
	    }
	    else
	    {
		save.start[subidx] = m->start[subidx];
		save.end[subidx] = m->end[subidx];
	        m->start[subidx] = reginput + off;
	    }

	    addstate(l, state->out, m, off, lid, match);

	    if (REG_MULTI)
	    {
		m->startpos[subidx] = save.startpos[subidx];
		m->endpos[subidx] = save.endpos[subidx];
	    }
	    else
	    {
		m->start[subidx] = save.start[subidx];
		m->end[subidx] = save.end[subidx];
	    }
	    break;

	case NFA_MCLOSE + 0:
	case NFA_MCLOSE + 1:
	case NFA_MCLOSE + 2:
	case NFA_MCLOSE + 3:
	case NFA_MCLOSE + 4:
	case NFA_MCLOSE + 5:
	case NFA_MCLOSE + 6:
	case NFA_MCLOSE + 7:
	case NFA_MCLOSE + 8:
	case NFA_MCLOSE + 9:
	    subidx = state->c - NFA_MCLOSE;

	    if (REG_MULTI)
	    {
		save.startpos[subidx] = m->startpos[subidx];
		save.endpos[subidx] = m->endpos[subidx];
	        m->endpos[subidx].lnum = reglnum;
	        m->endpos[subidx].col = reginput - regline + off;
	    }
	    else
	    {
		save.start[subidx] = m->start[subidx];
		save.end[subidx] = m->end[subidx];
	        m->end[subidx] = reginput + off;
	    }

	    addstate(l, state->out, m, off, lid, match);

	    if (REG_MULTI)
	    {
		m->startpos[subidx] = save.startpos[subidx];
		m->endpos[subidx] = save.endpos[subidx];
	    }
	    else
	    {
		m->start[subidx] = save.start[subidx];
		m->end[subidx] = save.end[subidx];
	    }
	    break;
    }
}

/*
 * nfa_regmatch - main matching routine
 *
 * Run NFA to determine whether it matches reginput.
 *
 * Return TRUE if there is a match, FALSE otherwise.
 * Note: Caller must ensure that: start != NULL.
 */
    static int
nfa_regmatch(start, submatch)
    nfa_state_T		*start;
    regsub_T		*submatch;
{
    int		c, n, i = 0;
    int		match = FALSE;
    int		flag = 0;
    int		reginput_updated = FALSE;
    Thread	*t;
    List	*thislist, *nextlist;

    static 	regsub_T m;
    c = -1;

    if (REG_MULTI) /* TODO(RE) write a function */
    {
        /* Use 0xff to set lnum to -1 */
        vim_memset(submatch->startpos, 0xff, sizeof(lpos_T) * NSUBEXP);
        vim_memset(submatch->endpos, 0xff, sizeof(lpos_T) * NSUBEXP);
        vim_memset(m.startpos, 0xff, sizeof(lpos_T) * NSUBEXP);
        vim_memset(m.endpos, 0xff, sizeof(lpos_T) * NSUBEXP);
    }
    else
    {
        vim_memset(submatch->start, 0, sizeof(char_u *) * NSUBEXP);
        vim_memset(submatch->end, 0, sizeof(char_u *) * NSUBEXP);
        vim_memset(m.start, 0, sizeof(char_u *) * NSUBEXP);
        vim_memset(m.end, 0, sizeof(char_u *) * NSUBEXP);
    }

    listid = 1;

#ifdef DEBUG
	f = fopen("regmatch.log","a");
	fprintf(f, "\n\n\n\n\n\n=======================================================\n");
	fprintf(f, "=======================================================\n\n\n\n\n\n\n");
	nfa_print_state(f, start, 0);
#endif

    thislist = &list[0];
    thislist->n = 0;
    addstate(thislist, start, &m, 0, listid, &match);

    /* run for each character */
    do {
again:
#ifdef FEAT_MBYTE
	if (has_mbyte)
	{
	    c = (*mb_ptr2char)(reginput);
	    n = (*mb_ptr2len)(reginput);
	}
	else
#endif
	{
	    c = *reginput;
	    n = 1;
	}
	if (c == NUL)
	    n = 0;

	/* swap lists */
	thislist = &list[flag];
	nextlist = &list[flag ^= 1];
	nextlist->n = 0;	    /* `clear' nextlist */
	++listid;
#ifdef DEBUG
    fprintf(f, "------------------------------------------\n");
    fprintf(f, ">>> Advanced one character ... Current char is %c (code %d) \n", c, (int)c);
    fprintf(f, ">>> Thislist has %d states available: ", thislist->n);
    for (i = 0; i< thislist->n; i++)
	fprintf(f, "%d  ", abs(thislist->t[i].state->id));
    fprintf(f, "\n");
#endif

	/* compute nextlist */
	for (i = 0; i < thislist->n; ++i)
	{
	    t = &thislist->t[i];
#ifdef DEBUG
    nfa_set_code(t->state->c);
    fprintf(f, "(%d) %s, code %d ... \n", abs(t->state->id), code, (int)t->state->c);
#endif
	    switch (t->state->c)
	    {
	    case NFA_MATCH:
		match = TRUE;
		*submatch = t->sub;
		goto nextchar;

	    case NFA_BOL:
		if (reginput == regline)
		    addstate(thislist, t->state->out, &t->sub, 0, listid, &match);
		break;

	    case NFA_EOL:
		if (c == NUL)
		    addstate(thislist, t->state->out, &t->sub, 0, listid, &match);
		break;

	    case NFA_BOW:
	    {
		int bol = TRUE;
		if (c == NUL)
		    bol = FALSE;
#ifdef FEAT_MBYTE
		else if (has_mbyte)
		{
		    int this_class;

		    /* Get class of current and previous char (if it exists). */
		    this_class = mb_get_class(reginput);
		    if (this_class <= 1)
			bol = FALSE;
		    else if (reg_prev_class() == this_class)
			bol = FALSE;
		}
#endif
		else
		{
		    if (!vim_iswordc(c)
			|| (reginput > regline && vim_iswordc(reginput[-1])))
			bol = FALSE;
		}
		if (bol)
		    addstate(thislist, t->state->out, &t->sub, 0, listid, &match);
		break;
	    }

	    case NFA_EOW:
	    {
		int eol = TRUE;
		if (reginput == regline)
		    eol = FALSE;
#ifdef FEAT_MBYTE
		else if (has_mbyte)
		{
		    int this_class, prev_class;

		    /* Get class of current and previous char (if it exists). */
		    this_class = mb_get_class(reginput);
		    prev_class = reg_prev_class();
		    if (this_class == prev_class
			    || prev_class == 0 || prev_class == 1)
		    	eol = FALSE;
		}
#endif
		else
		{
		    if (!vim_iswordc(reginput[-1])
			    || (reginput[0] != NUL && vim_iswordc(c)))
			eol = FALSE;
		}
		if (eol)
		    addstate(thislist, t->state->out, &t->sub, 0, listid, &match);
		break;
	    }

	    case NFA_ANY:
		addstate(nextlist, t->state->out, &t->sub, n, listid+1, &match);
		break;

	    case NFA_NEWL:
		if (!reg_line_lbr && REG_MULTI
				&& c == NUL && reglnum <= reg_maxline)
		{
		    reg_nextline();
		    addstate(nextlist, t->state->out, &t->sub, n, listid+1, &match);
		    reginput_updated = TRUE;
		}
		break;

	    default:	/* regular character */
		if (t->state->c == c)
		    addstate(nextlist, t->state->out, &t->sub, n, listid+1, &match);
		else if (ireg_ic && MB_TOLOWER(t->state->c) == MB_TOLOWER(c))
		    addstate(nextlist, t->state->out, &t->sub, n, listid+1, &match);
		break;
	    }

	}   /* for (thislist = thislist; thislist->state; thislist++) */

	if (match == FALSE)
	{
	    addstate(nextlist, start, &m, n, listid+1, &match);
	}

        if (reginput_updated)
        {
	   reginput_updated = FALSE;
	   goto again;
        }

nextchar:
#ifdef FEAT_MBYTE
	/* TODO(RE) Check for following composing character. */
#endif
	    reginput += n;
    } while (c);

#ifdef DEBUG
    fclose(f);
#endif

    return match;
}

/*
 * nfa_regtry - try match of "prog" with at regline["col"].
 * Returns 0 for failure, number of lines contained in the match otherwise.
 */
    static long
nfa_regtry(start, col)
    nfa_state_T	*start;
    colnr_T	col;
{
    int		i;
    regsub_T	sub;

    reginput = regline + col;
    need_clear_subexpr = TRUE;

    if (nfa_regmatch(start, &sub) == 0)
	return 0;

    cleanup_subexpr();
    if (REG_MULTI)
    {
	for (i = 0; i < NSUBEXP; i++)
	{
            reg_startpos[i] = sub.startpos[i];
            reg_endpos[i] = sub.endpos[i];
	}

        if (reg_startpos[0].lnum < 0)
        {
            reg_startpos[0].lnum = 0;
            reg_startpos[0].col = col;
        }
        if (reg_endpos[0].lnum < 0)
        {
            reg_endpos[0].lnum = reglnum;
            reg_endpos[0].col = (int)(reginput - regline);
        }
        else
            /* Use line number of "\ze". */
            reglnum = reg_endpos[0].lnum;
    }
    else
    {
	for (i = 0; i < NSUBEXP; i++)
	{
            reg_startp[i] = sub.start[i];
            reg_endp[i] = sub.end[i];
	}

        if (reg_startp[0] == NULL)
            reg_startp[0] = regline + col;
        if (reg_endp[0] == NULL)
            reg_endp[0] = reginput;
    }

    return 1 + reglnum;
}

/*
 * Match a regexp against a string ("line" points to the string) or multiple
 * lines ("line" is NULL, use reg_getline()).
 *
 * Returns 0 for failure, number of lines contained in the match otherwise.
 */
    static long
nfa_regexec_both(line, col)
    char_u	*line;
    colnr_T	col;		/* column to start looking for match */
{
    nfa_regprog_T   *prog;
    long	retval = 0L;
    int		size;
    int		i;

    if (REG_MULTI)
    {
	prog = (nfa_regprog_T*)reg_mmatch->regprog;
	line = reg_getline((linenr_T)0);    /* relative to the cursor */
	reg_startpos = reg_mmatch->startpos;
	reg_endpos = reg_mmatch->endpos;
    }
    else
    {
	prog = (nfa_regprog_T*)reg_match->regprog;
	reg_startp = reg_match->startp;
	reg_endp = reg_match->endp;
    }

    /* Be paranoid... */
    if (prog == NULL || line == NULL)
    {
	EMSG(_(e_null));
	goto theend;
    }

    /* If the start column is past the maximum column: no need to try. */
    if (ireg_maxcol > 0 && col >= ireg_maxcol)
	goto theend;

    /* If pattern contains "\c" or "\C": overrule value of ireg_ic */
    if (prog->regflags & RF_ICASE)
	ireg_ic = TRUE;
    else if (prog->regflags & RF_NOICASE)
	ireg_ic = FALSE;

#ifdef FEAT_MBYTE
    /* If pattern contains "\Z" overrule value of ireg_icombine */
    if (prog->regflags & RF_ICOMBINE)
	ireg_icombine = TRUE;
#endif

    regline = line;
    reglnum = 0;    /* relative to line */

    nstate = prog->nstate;

    size = (nstate + 1) * sizeof(Thread);

    list[0].t = (Thread *)lalloc(size, TRUE);
    list[1].t = (Thread *)lalloc(size, TRUE);
    if (list[0].t == NULL || list[1].t == NULL)
	goto theend;

    vim_memset(list[0].t, 0, size);
    vim_memset(list[1].t, 0, size);

    /* TODO need speedup */
    for (i = 0; i < nstate; ++i)
        {
	    prog->state[i].id = i;
            prog->state[i].lastlist = 0;
            prog->state[i].visits = 0;
            prog->state[i].lastthread = NULL;
        }

    retval = nfa_regtry(prog->start, col);

theend:
    vim_free(list[0].t);
    vim_free(list[1].t);
    list[0].t = NULL;
    list[1].t = NULL;

    return retval;
}

/*
 * compile a regular expression into internal code for the NFA matcher.
 * Returns the program in allocated space.  Returns NULL for an error.
 */
    static regprog_T *
nfa_regcomp(expr, re_flags)
    char_u	*expr;
    int		re_flags;
{
    nfa_regprog_T	*prog;
    int			prog_size;
    int			*postfix;

    if (expr == NULL)
	return NULL;

    init_class_tab();

    if (nfa_regcomp_start(expr, re_flags) == FAIL)
	return NULL;

    /* Space for compiled regexp */
    prog_size = sizeof(nfa_regprog_T) + sizeof(nfa_state_T) * nstate_max;
    prog = (nfa_regprog_T *)lalloc(prog_size, TRUE);	    
    if (prog == NULL)
	goto fail;	
    vim_memset(prog, 0, prog_size);

    /* Build postfix form of the regexp. Needed to build the NFA (and count its size) */
    postfix = re2post(expr, re_flags);
    if (postfix == NULL)
	goto fail;	    /* Cascaded (syntax?) error */

    /* 
     * TODO(RE) Two passes: 
     * 1. first to decide size, and count repetition operators \{}
     * 2. second to emit code 
     */
#ifdef DEBUG
    FILE *f = fopen("regmatch.log", "a");
    if (f)
    {
	fprintf(f, "\n***************\n\n\n\nCompiling regexp \"%s\" ... hold on !\n\n\n", expr);
	fclose(f);
    }
#endif

    /* PASS 1 
     * Parse expression, count number of NFA states in "nstate". Do not build the NFA. 
     */
    nfa_calc_size = TRUE;
    post2nfa(postfix);
    nfa_calc_size = FALSE;

    state_ptr = prog->state;

    /* PASS 2
     * Build the NFA 
     */
    prog->start = post2nfa(postfix);
    if (prog->start == NULL)
	goto fail;

    prog->regflags = regflags;
    prog->engine = &nfa_regengine;
    prog->nstate = nstate;
#ifdef DEBUG
    nfa_postfix_dump(expr, OK);
    nfa_dump(prog);
#endif
	
out:
    vim_free(post_start);
    post_start = post_ptr = post_end = NULL;
    state_ptr = NULL;
    return (regprog_T *)prog;

fail:
    vim_free(prog);
    prog = NULL;
#ifdef DEBUG
    nfa_postfix_dump(expr, FAIL);
#endif
    goto out;
}

/*
 * Match a regexp against a string.
 * "rmp->regprog" is a compiled regexp as returned by nfa_regcomp().
 * Uses curbuf for line count and 'iskeyword'.
 *
 * Return TRUE if there is a match, FALSE if not.
 */
    static int
nfa_regexec(rmp, line, col)
    regmatch_T	*rmp;
    char_u	*line;	/* string to match against */
    colnr_T	col;	/* column to start looking for match */
{
    reg_match = rmp;
    reg_mmatch = NULL;
    reg_maxline = 0;
    reg_line_lbr = FALSE;
    reg_win = NULL;
    ireg_ic = rmp->rm_ic;
#ifdef FEAT_MBYTE
    ireg_icombine = FALSE;
#endif
    ireg_maxcol = 0;
    return (nfa_regexec_both(line, col) != 0);
}

#if defined(FEAT_MODIFY_FNAME) || defined(FEAT_EVAL) \
	|| defined(FIND_REPLACE_DIALOG) || defined(PROTO)
/*
 * Like nfa_regexec(), but consider a "\n" in "line" to be a line break.
 */
    static int
nfa_regexec_nl(rmp, line, col)
    regmatch_T	*rmp;
    char_u	*line;	/* string to match against */
    colnr_T	col;	/* column to start looking for match */
{
    reg_match = rmp;
    reg_mmatch = NULL;
    reg_maxline = 0;
    reg_line_lbr = TRUE;
    reg_win = NULL;
    ireg_ic = rmp->rm_ic;
#ifdef FEAT_MBYTE
    ireg_icombine = FALSE;
#endif
    ireg_maxcol = 0;
    return (nfa_regexec_both(line, col) != 0);
}
#endif

/*
 * Match a regexp against multiple lines.
 * "rmp->regprog" is a compiled regexp as returned by vim_regcomp().
 * Uses curbuf for line count and 'iskeyword'.
 *
 * Return zero if there is no match.  Return number of lines contained in the
 * match otherwise.
 *
 * Note: the body is the same as bt_regexec() except for nfa_regexec_both()
 *
 * ! Also NOTE : match may actually be in another line. e.g.:
 * when r.e. is \nc, cursor is at 'a' and the text buffer looks like
 *
 * +-------------------------+
 * |a                        |
 * |b                        |
 * |c                        |
 * |                         |
 * +-------------------------+
 *
 * then nfa_regexec_multi() returns 3. while the original
 * vim_regexec_multi() returns 0 and a second call at line 2 will return 2.
 *
 * FIXME if this behivior is not compatible.
 */
    static long
nfa_regexec_multi(rmp, win, buf, lnum, col, tm)
    regmmatch_T	*rmp;
    win_T	*win;		/* window in which to search or NULL */
    buf_T	*buf;		/* buffer in which to search */
    linenr_T	lnum;		/* nr of line to start looking for match */
    colnr_T	col;		/* column to start looking for match */
    proftime_T	*tm;		/* timeout limit or NULL */
{
    long	r;
    buf_T	*save_curbuf = curbuf;

    reg_match = NULL;
    reg_mmatch = rmp;
    reg_buf = buf;
    reg_win = win;
    reg_firstlnum = lnum;
    reg_maxline = reg_buf->b_ml.ml_line_count - lnum;
    reg_line_lbr = FALSE;
    ireg_ic = rmp->rmm_ic;
#ifdef FEAT_MBYTE
    ireg_icombine = FALSE;
#endif
    ireg_maxcol = rmp->rmm_maxcol;

    /* Need to switch to buffer "buf" to make vim_iswordc() work. */
    curbuf = buf;
    r = nfa_regexec_both(NULL, col);
    curbuf = save_curbuf;

    return r;
}

