/* vi:set ts=8 sts=4 sw=4:  */
/******************** Below are NFA regexp *********************/
/* File nfa_regexp.c is included automa[gt]ically in regexp.c, at the end. */


#define ENABLE_CHAR_RANGE	/* Comment this out to disable the NFA implementation of  [ ] */

#ifdef DEBUG
//#define ENABLE_LOG_FILE		/* Comment this out to disable log files. They can get pretty big */
#endif

/* Upper limit allowed for {m,n} repetitions handled by NFA */
#define	    NFA_BRACES_MAXLIMIT		    10	    
/* For allocating space for the postfix representation */
#define	    NFA_POSTFIX_MULTIPLIER	    (NFA_BRACES_MAXLIMIT+2)*2	    
/* Size of stack, used when converting the postfix regexp into NFA */
#define	    NFA_STACK_SIZE		    1024

#ifdef ENABLE_LOG_FILE
static void nfa_postfix_dump __ARGS((char_u *expr, int retval));
static void nfa_dump __ARGS((nfa_regprog_T *prog));
FILE	    *f;
#endif

/* Global variable, set during compilation of a regexp, when an error is encountered. 
 * If TRUE, we revert to the backtracking engine. If false, then the error 
 * should have been handled by the NFA engine. Syntax errors are handled by NFA engine. */
static int syntax_error = FALSE;

static int *post_start;  /* holds the postfix form of r.e. */
static int *post_end;
static int *post_ptr;


static int nstate;	/* Number of states in the NFA. */
static int istate;	/* Index in the state vector, used in new_state */
static int nstate_max;	/* Upper bound of estimated number of states. */

static int nfa_just_found_braces = FALSE;  


/* helper fuctions used when doing re2post() parsing */
#define EMIT(c)	do {				\
		    if (post_ptr >= post_end)	\
			return FAIL;		\
		    *post_ptr++ = c;		\
		} while (0)

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

/* 
 * Search between "start" and "end" and try to recognize a 
 * character class in expanded form. For example [0-9]. 
 * On success, return the id the character class to be emitted. 
 * On failure, return 0 (=FAIL)
 * Start points to the first char of the range, while end should point
 * to the closing brace.
 */
static int
nfa_recognize_char_class(start, end, extra_newl)
    char_u  *start;
    char_u  *end;
    int	    extra_newl;
{
int	i;
/* Each of these variables takes up a char in "config[]",
 * in the order they are here. */
int	not = FALSE, af = FALSE, AF = FALSE, az = FALSE, 
	AZ = FALSE, o7 = FALSE, o9 = FALSE, underscore = FALSE,
	newl = FALSE;
char_u	*p;
#define NCONFIGS 16
int	classid[NCONFIGS] = 
		    {	NFA_DIGIT, NFA_NDIGIT, NFA_HEX, NFA_NHEX, 
			NFA_OCTAL, NFA_NOCTAL, NFA_WORD, NFA_NWORD, 
			NFA_HEAD, NFA_NHEAD, NFA_ALPHA, NFA_NALPHA, 
			NFA_LOWER, NFA_NLOWER, NFA_UPPER, NFA_NUPPER
		    };
char_u	myconfig[9];
char_u	config[NCONFIGS][9] = {
		     "000000100",	/* digit */
		     "100000100",	/* non digit */
		     "011000100",	/* hex-digit */
		     "111000100",	/* non hex-digit */
		     "000001000",	/* octal-digit */
		     "100001000",	/* [^0-7] */
		     "000110110",	/* [0-9A-Za-z_]	*/
		     "100110110",	/* [^0-9A-Za-z_] */
		     "000110010",	/* head of word */
		     "100110010",	/* not head of word */
		     "000110000",	/* alphabetic char a-z */
		     "100110000",	/* non alphabetic char */
		     "000100000",	/* lowercase letter */
		     "100100000",	/* non lowercase */
		     "000010000",	/* uppercase */
		     "100010000"	/* non uppercase */
			};

    if (extra_newl == TRUE)
	newl = TRUE;

    if (*end != ']')
	return FAIL;
    p = start;
    if (*p == '^')
    {
	not = TRUE;
	p ++;
    }

    while (p < end)
    {
	if (p+2 < end && *(p+1) == '-')
	{
	    switch (*p)
	    {
		case '0':   
		    if (*(p+2) == '9')
		    {
			o9 = TRUE;
			break;
		    }
		    else
		    if (*(p+2) == '7')
		    {
			o7 = TRUE;
			break;
		    }
		case 'a':
		    if (*(p+2) == 'z')
		    {
			az = TRUE;
			break;
		    }
		    else
		    if (*(p+2) == 'f')
		    {
			af = TRUE;
			break;
		    }
		case 'A':
		    if (*(p+2) == 'Z')
		    {
			AZ = TRUE;
			break;
		    }
		    else
		    if (*(p+2) == 'F')
		    {
			AF = TRUE;
			break;
		    }
		/* fallthrough */
		default:
		    return FAIL;
	    }
	    p += 3;
	}
	else
	if (p+1 < end && *p == '\\' && *(p+1) == 'n')
	{
	    newl = TRUE;
	    p += 2;
	}
	else
	if (*p == '_')
	{
	    underscore = TRUE;
	    p ++;
	}
	else
	if (*p == '\n')
	{
	    newl = TRUE;
	    p ++;
	}
	else
	    return FAIL;
    }	/* while (p < end) */
    if (p != end)
	return FAIL;

    /* build the config that represents the ranges we gathered */
    STRCPY(myconfig, "000000000");
    if (not == TRUE)
	myconfig[0] = '1';
    if (af == TRUE)
	myconfig[1] = '1';
    if (AF == TRUE)
	myconfig[2] = '1';
    if (az == TRUE)
	myconfig[3] = '1';
    if (AZ == TRUE)
	myconfig[4] = '1';
    if (o7 == TRUE)
	myconfig[5] = '1';
    if (o9 == TRUE)
	myconfig[6] = '1';
    if (underscore == TRUE)
	myconfig[7] = '1';
    if (newl == TRUE)
	myconfig[8] = '1';

    /* try to recognize character classes */
    for (i = 0; i < NCONFIGS; i++)
	if (STRNCMP(myconfig, config[i],8) == 0)
	    return classid[i];

    /* TODO(RE) Add support for character classes + Newline */
    
    /* fallthrough => no success so far */
    return FAIL;

#undef NCONFIGS
}


/*
 * Produce the bytes for equivalence class "c".
 * Currently only handles latin1, latin9 and utf-8.
 * Emits bytes in postfix notation: 'a,b,NFA_OR,c,NFA_OR' is equivalent to 'a OR b OR c'
 * NOTE! When changing this function, also update reg_equi_class()
 */
    static int
nfa_emit_equi_class(c, neg)
    int	    c;
    int	    neg;
{
int	first = TRUE;
int	glue = neg == TRUE ? NFA_CONCAT : NFA_OR; 
#define EMIT_GLU()		\
	if (neg == TRUE) {	\
	    EMIT(NFA_NOT);	\
	    EMIT(NFA_CONCAT);	\
	}			\
	if (first == FALSE)	\
	    EMIT(glue);		\
	else			\
	    first = FALSE;	\
	    

#ifdef FEAT_MBYTE
    if (enc_utf8 || STRCMP(p_enc, "latin1") == 0
					 || STRCMP(p_enc, "iso-8859-15") == 0)
#endif
    {
	switch (c)
	{
	    case 'A': case '\300': case '\301': case '\302':
	    case '\303': case '\304': case '\305':
		    EMIT('A');	    EMIT_GLU();
		    EMIT('\300');   EMIT_GLU();
		    EMIT('\301');   EMIT_GLU();
		    EMIT('\302');   EMIT_GLU();
		    EMIT('\303');   EMIT_GLU();
		    EMIT('\304');   EMIT_GLU();
		    EMIT('\305');   EMIT_GLU();
		    return OK;

	    case 'C': case '\307':
		    EMIT('C');	    EMIT_GLU();
		    EMIT('\307');   EMIT_GLU();
		    return OK;

	    case 'E': case '\310': case '\311': case '\312': case '\313':
		    EMIT('E');	    EMIT_GLU();
		    EMIT('\310');   EMIT_GLU();
		    EMIT('\311');   EMIT_GLU();
		    EMIT('\312');   EMIT_GLU();
		    EMIT('\313');   EMIT_GLU();
		    return OK;

	    case 'I': case '\314': case '\315': case '\316': case '\317':
		    EMIT('I');	    EMIT_GLU();
		    EMIT('\315');   EMIT_GLU();
		    EMIT('\316');   EMIT_GLU();
		    EMIT('\317');   EMIT_GLU();
		    return OK;

	    case 'N': case '\321':
		    EMIT('N');	    EMIT_GLU();
		    EMIT('\321');   EMIT_GLU();
		    return OK;

	    case 'O': case '\322': case '\323': case '\324': case '\325':
	    case '\326':
		    EMIT('O');	    EMIT_GLU();
		    EMIT('\322');   EMIT_GLU();
		    EMIT('\323');   EMIT_GLU();
		    EMIT('\324');   EMIT_GLU();
		    EMIT('\325');   EMIT_GLU();
		    EMIT('\326');   EMIT_GLU();
		    return OK;

	    case 'U': case '\331': case '\332': case '\333': case '\334':
		    EMIT('U');	    EMIT_GLU();
		    EMIT('\331');   EMIT_GLU();
		    EMIT('\332');   EMIT_GLU();
		    EMIT('\333');   EMIT_GLU();
		    EMIT('\334');   EMIT_GLU();
		    return OK;

	    case 'Y': case '\335':
		    EMIT('Y');	    EMIT_GLU();
		    EMIT('\335');   EMIT_GLU();
		    return OK; 

	    case 'a': case '\340': case '\341': case '\342':
	    case '\343': case '\344': case '\345':
		    EMIT('a');	    EMIT_GLU();
		    EMIT('\340');   EMIT_GLU();
		    EMIT('\341');   EMIT_GLU();
		    EMIT('\342');   EMIT_GLU();
		    EMIT('\343');   EMIT_GLU();
		    EMIT('\344');   EMIT_GLU();
		    EMIT('\345');   EMIT_GLU();
		    return OK;

	    case 'c': case '\347':
		    EMIT('c');	    EMIT_GLU();
		    EMIT('\347');   EMIT_GLU();
		    return OK;

	    case 'e': case '\350': case '\351': case '\352': case '\353':
		    EMIT('e');	    EMIT_GLU();
		    EMIT('\350');   EMIT_GLU();
		    EMIT('\351');   EMIT_GLU();
		    EMIT('\352');   EMIT_GLU();
		    EMIT('\353');   EMIT_GLU();
		    return OK;

	    case 'i': case '\354': case '\355': case '\356': case '\357':
		    EMIT('i');	    EMIT_GLU();
		    EMIT('\354');   EMIT_GLU();
		    EMIT('\355');   EMIT_GLU();
		    EMIT('\356');   EMIT_GLU();
		    EMIT('\357');   EMIT_GLU();
		    return OK;

	    case 'n': case '\361':
		    EMIT('n');	    EMIT_GLU();
		    EMIT('\361');   EMIT_GLU();
		    return OK;

	    case 'o': case '\362': case '\363': case '\364': case '\365':
	    case '\366':
		    EMIT('o');	    EMIT_GLU();
		    EMIT('\362');   EMIT_GLU();
		    EMIT('\363');   EMIT_GLU();
		    EMIT('\364');   EMIT_GLU();
		    EMIT('\365');   EMIT_GLU();
		    EMIT('\366');   EMIT_GLU();
		    return OK;

	    case 'u': case '\371': case '\372': case '\373': case '\374':
		    EMIT('u');	    EMIT_GLU();
		    EMIT('\371');   EMIT_GLU();
		    EMIT('\372');   EMIT_GLU();
		    EMIT('\373');   EMIT_GLU();
		    EMIT('\374');   EMIT_GLU();
		    return OK;

	    case 'y': case '\375': case '\377':
		    EMIT('y');	    EMIT_GLU();
		    EMIT('\375');   EMIT_GLU();
		    EMIT('\377');   EMIT_GLU();
		    return OK;

	    default:
		    return FAIL;
	}
    }

    EMIT(c);
    return OK;
#undef EMIT_GLU
}

/*
 * Code to parse regular expression.
 *
 * We try to reuse parsing functions in regexp.c to
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
    int		c, charclass, equiclass, collclass, got_coll_char; 
    char_u	*p, *endp;
    int		extra = 0, first, emit_range, negated, result;
    int		startc = -1, endc = -1, oldstartc = -1;
    int		cpo_lit;	/* 'cpoptions' contains 'l' flag */
    int		cpo_bsl;	/* 'cpoptions' contains '\' flag */
    int		glue;

    cpo_lit = vim_strchr(p_cpo, CPO_LITERAL) != NULL;
    cpo_bsl = vim_strchr(p_cpo, CPO_BACKSL) != NULL;

    c = getchr();
    /* NFA engine doesn't yet support mbyte composing chars => bug when search mbyte chars.
     * Fail and revert to old engine */
#ifdef FEAT_MBYTE
    if ((*mb_char2len)(c)>1)
        return FAIL;	    /* unsupported for now */
#endif
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
		    
		  /* TODO(RE) \_x (character classes + newline) not yet supported)	*/
		  return FAIL;

		  extra = ADD_NL;

		  /* TODO(RE) "\_[" is character range plus newline */
		  if (c == '[')
		    goto charranges;		  

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
#ifdef DEBUG
//	EMSG3("E999: (NFA) Class char %c, found at index %d in nfa_classcodes", c, p-classchars);
#endif
			EMIT(nfa_classcodes[p - classchars]);
			if (extra == ADD_NL)
			{
			    EMIT(NFA_NEWL);
			    EMIT(NFA_OR);
			}
			break;

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
			syntax_error = TRUE;
			EMSG_RET_FAIL("E999: (NFA regexp) Misplaced ) or & or | ");

		case Magic('='):
		case Magic('?'):
		case Magic('+'):
		case Magic('@'):
		case Magic('*'):
		case Magic('{'):
			/* these should follow an atom, not form an atom */
			syntax_error = TRUE;
			EMSG_RET_FAIL("E999: (NFA regexp) Misplaced =?+@*{");

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

charranges:
		case Magic('['):
#ifndef ENABLE_CHAR_RANGE
		    return FAIL;
#endif
		    /* 
		     * Glue is emitted between several atoms from the []. 
		     * It is either NFA_OR, or NFA_CONCAT. 
		     *
		     * [abc] expands to 'a b NFA_OR c NFA_OR' (in postfix notation)
		     * [^abc] expands to 'a NFA_NOT b NFA_NOT NFA_CONCAT c NFA_NOT NFA_CONCAT
		     *		NFA_CONCAT NFA_END_NEG_RANGE NFA_CONCAT' (in postfix notation)	
		     *
		     */


/* Emit negation atoms, if needed 
 * The CONCAT below merges the NOT with the previous node */
#define TRY_NEG()		    \
	    if (negated == TRUE)    \
	    {			    \
		EMIT(NFA_NOT);	    \
		EMIT(NFA_CONCAT);   \
	    }

/* Emit glue between important nodes : CONCAT or OR */
#define EMIT_GLUE()		    \
	    if (first == FALSE)	    \
		EMIT(glue);	    \
	    else		    \
		first = FALSE;	    

		    p = regparse;
		    endp = skip_anyof(p);
		    if (*endp == ']')
		    {
			/* 
			 * Try to reverse engineer character classes. For example,
			 * recognize that [0-9] stands for  \d and [A-Za-z_] with \h,
			 * and perform the necessary substitutions in the NFA 
			 */
			result = nfa_recognize_char_class(regparse,endp, extra == ADD_NL);
			if (result != FAIL)
			{
			    EMIT(result);
			    regparse = endp + 1;
			    return OK;
			}
			/* 
			 * Failed to recognize a character class. Use the simple version
			 * that turns [abc] into 'a' OR 'b' OR 'c'
			 */
			startc = endc = oldstartc = -1;
			first = TRUE;		/* Emitting first atom in this sequence? */
			negated = FALSE;
			glue = NFA_OR;
			if (*regparse == '^') 			/* negated range */
			{
			    negated = TRUE;
			    glue = NFA_CONCAT;
			    regparse ++;
			}
			if (*regparse == '-')
			{
			    startc = '-';
			    EMIT(startc);
			    TRY_NEG();
			    EMIT_GLUE();
			    regparse ++;
			}
			/* Emit the OR branches for each character in the [] */
			emit_range = FALSE;
			while (regparse < endp)
			{
			    oldstartc = startc;
			    startc = -1;
			    got_coll_char = FALSE;
			    if (*regparse == '[')
			    {
				/* Check for [: :], [= =], [. .] */
				equiclass = collclass = 0;
				charclass = get_char_class(&regparse);
				if (charclass == CLASS_NONE) 
				{
				    equiclass = get_equi_class(&regparse);
				    if (equiclass == 0)
					collclass = get_coll_element(&regparse);
				}

				/* Character class like [:alpha:]  */
				if (charclass != CLASS_NONE)
				{
				    switch (charclass)
				    {
					case CLASS_ALNUM:
					    EMIT(NFA_CLASS_ALNUM);
					    break;
					case CLASS_ALPHA:
					    EMIT(NFA_CLASS_ALPHA);
					    break;
					case CLASS_BLANK:
					    EMIT(NFA_CLASS_BLANK);
					    break;
					case CLASS_CNTRL:
					    EMIT(NFA_CLASS_CNTRL);
					    break;
					case CLASS_DIGIT:
					    EMIT(NFA_CLASS_DIGIT);
					    break;
					case CLASS_GRAPH:
					    EMIT(NFA_CLASS_GRAPH);
					    break;
					case CLASS_LOWER:
					    EMIT(NFA_CLASS_LOWER);
					    break;
					case CLASS_PRINT:
					    EMIT(NFA_CLASS_PRINT);
					    break;
					case CLASS_PUNCT:
					    EMIT(NFA_CLASS_PUNCT);
					    break;
					case CLASS_SPACE:
					    EMIT(NFA_CLASS_SPACE);
					    break;
					case CLASS_UPPER:
					    EMIT(NFA_CLASS_UPPER);
					    break;
					case CLASS_XDIGIT:
					    EMIT(NFA_CLASS_XDIGIT);
					    break;
					case CLASS_TAB:
					    EMIT(NFA_CLASS_TAB);
					    break;
					case CLASS_RETURN:
					    EMIT(NFA_CLASS_RETURN);
					    break;
					case CLASS_BACKSPACE:
					    EMIT(NFA_CLASS_BACKSPACE);
					    break;
					case CLASS_ESCAPE:
					    EMIT(NFA_CLASS_ESCAPE);
					    break;
				    }
				    TRY_NEG();
				    EMIT_GLUE();
				    continue;
				}
				/* Try equivalence class [=a=] and the like */
				if (equiclass != 0)
				{
				    result = nfa_emit_equi_class(equiclass, negated);
				    if (result == FAIL)
				    {
					/* should never happen */
					EMSG_RET_FAIL("E999: Error building NFA with equivalence class!");
				    }
				    EMIT_GLUE();
				    continue;
				}
				/* Try collating class like [. .]  */
				if (collclass != 0)
				{
				    startc = collclass;	    /* allow [.a.]-x as a range */
				    /* Will emit the proper atom at the end of the while loop */
				}
			    }
			    /* Try a range like 'a-x' or '\t-z' */
			    if (*regparse == '-')
			    {
				emit_range = TRUE;
				startc = oldstartc;
				regparse++;
				continue;	    /* reading the end of the range */
			    }

			    /* Now handle simple and escaped characters.
			     * Only "\]", "\^", "\]" and "\\" are special in Vi.  Vim
			     * accepts "\t", "\e", etc., but only when the 'l' flag in
			     * 'cpoptions' is not included.
			     * Posix doesn't recognize backslash at all.
			     */
			    if  (*regparse == '\\' && !cpo_bsl && regparse+1 <= endp && 
				    (vim_strchr(REGEXP_INRANGE, regparse[1]) != NULL ||
					(!cpo_lit && vim_strchr(REGEXP_ABBR, regparse[1]) != NULL)
				    )
				)
			    {
				regparse++;

				if (*regparse == 'u' || *regparse == 'U')
				    return FAIL;	/* multibyte chars not supported yet */

				if (*regparse == 'n' || *regparse == 'n')
				    startc = reg_string ? NL : NFA_NEWL; 
				else
				if (*regparse == 'd'
				    || *regparse == 'o'
				    || *regparse == 'x')
				{
				    startc = coll_get_char();
				    got_coll_char = TRUE;
				    regparse--;
				}
				else
				{
				    startc = backslash_trans(*regparse);	    /* \r,\t,\e,\b */
				}
			    }

			    /* Normal printable char */
			    if (startc == -1)
				startc = *regparse;
			    if (startc >= 256)
				EMSG_RET_FAIL("E999: (NFA regexp) Multibyte characters are not yet supported");
			    
			    /* Previous char was '-', so this char is end of range. Emit the range */
			    if (emit_range)
			    {
				endc = startc; startc = oldstartc;
				if (startc > endc || startc + 256 < endc)
				    EMSG_RET_FAIL(_(e_invrange));
				/* Emit the range. "startc" was already emmitted, so skip it. */
				for (c = startc+1; c <= endc; c++)
				{
				    EMIT(c);
				    TRY_NEG();
				    EMIT_GLUE();
				}
				emit_range = FALSE;
			    }
			    else
			    /* This char (startc) is not part of a range. Just emit it. */
			    {	
				/* 
				 * Normally, simply emit startc. But if we get char code=0 
				 * from a collating char, then replace it with 0x0a.
				 * 
				 * This is needed to completely mimic the behaviour of 
				 * the backtracking engine.
				 * */
				if (got_coll_char == TRUE && startc == 0)
				    EMIT(0x0a);
				else
				    EMIT(startc);
				TRY_NEG();
				EMIT_GLUE();
			    }

			    regparse++;
			}	    /* while (p < endp) */

			if (*(regparse-1) == '-')		/* if last, '-' is just a char */
			{
			    EMIT('-');
			    TRY_NEG();
			    EMIT_GLUE();
			}

			if (extra == ADD_NL)		/* \_[] also matches \n */
			{
			    EMIT(reg_string ? NL : NFA_NEWL);
			    TRY_NEG();
			    EMIT_GLUE();
			}

			regparse = endp+1;		/* skip the trailing ] */
			if (negated == TRUE)
			{
			    EMIT(NFA_END_NEG_RANGE);	/* Mark end of negated char range */
			    EMIT(NFA_CONCAT);
			}
			return OK;
		    }		/* if exists closing ] */
		    else 
		    if (reg_strict)
		    {
			syntax_error = TRUE;
			EMSG_RET_FAIL(_("E769: Missing ] after ["));
		    }

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
			/* composing char not supported yet */
#endif
			c = no_Magic(c);
			EMIT(c);

			return OK;
		}
    }

#undef TRY_NEG
#undef EMIT_GLUE

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
    int		c2;
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
	/* 
	 * Trick: Normally, (a*)\+ would match the whole input "aaa".
	 * The first and only submatch would be "aaa". But the backtracking 
	 * engine interprets the plus as "try matching one more time", and 
	 * a* matches a second time at the end of the input, the empty string.
	 * The submatch will the empty string. 
	 *
	 * In order to be consistent with the old engine, we disable NFA_PLUS,
	 * and replace <atom>+ with <atom><atom>*
	 */
/*	    EMIT(NFA_PLUS);	    */
	    regnpar = old_regnpar;
	    regparse = old_regparse;
	    curchr = -1;
	    if (nfa_regatom() == FAIL)
		return FAIL;	
	    EMIT(NFA_STAR);
	    EMIT(NFA_CONCAT);
	    skipchr();		/* skip the \+	*/
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
	    if (c2 == '-' || c2 == Magic('-'))
	    {
		skipchr();
		greedy = FALSE;
	    }
	    if (!read_limits(&minval, &maxval))
	    {
		syntax_error = TRUE;
		EMSG_RET_FAIL("E999: (NFA regexp) Error reading repetition limits");
	    }
	    /*  <atom>{0,inf}, <atom>{0,} and <atom>{}  are equivalent to <atom>*  */
	    if (minval == 0 && maxval == MAX_LIMIT && greedy)	
	    {
		EMIT(NFA_STAR);
		break;
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
		curchr = -1;
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
	    curchr = -1;
	    
	    break;


	default:
	    break;
    }	/* end switch */

    if (re_multi_type(peekchr()) != NOT_MULTI)
    {
	/* Can't have a multi follow a multi. */
	syntax_error = TRUE;
	EMSG_RET_FAIL("E999: (NFA regexp) Can't have a multi follow a multi !");
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
		if (first == FALSE)
		    EMIT(NFA_CONCAT);
		else
		    first = FALSE;
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
	    syntax_error = TRUE;
	    EMSG_RET_FAIL("E999: (NFA regexp) Too many '('");
	}
	parno = regnpar++;
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
	syntax_error = TRUE;
	EMSG_RET_FAIL("E999: (NFA regexp) Group is not properly terminated ");
    }
    else if (paren == REG_NOPAREN && peekchr() != NUL)
    {
	syntax_error = TRUE;
	if (peekchr() == Magic(')'))
	    EMSG_M_RET_FAIL(_("E55: Unmatched %s)"), reg_magic == MAGIC_ALL);	
	else
	    EMSG_RET_FAIL("E999: (NFA regexp) proper termination error ");
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

#ifdef ENABLE_LOG_FILE
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
	case NFA_NEWL:	STRCPY(code, "NFA_NEWL "); break;
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
	case NFA_STAR: STRCPY(code, "NFA_STAR "); break;
	case NFA_PLUS: STRCPY(code, "NFA_PLUS "); break;
	case NFA_NOT:	STRCPY(code, "NFA_NOT "); break;
	case NFA_SKIP_CHAR: STRCPY(code, "NFA_SKIP_CHAR"); break;
	case NFA_OR: STRCPY(code, "NFA_OR"); break;
	case NFA_QUEST:	STRCPY(code, "NFA_QUEST"); break;
	case NFA_QUEST_NONGREEDY: STRCPY(code, "NFA_QUEST_NON_GREEDY"); break;
	case NFA_END_NEG_RANGE:	STRCPY(code, "NFA_END_NEG_RANGE"); break;
	case NFA_CLASS_ALNUM:	STRCPY(code, "NFA_CLASS_ALNUM"); break;
	case NFA_CLASS_ALPHA:	STRCPY(code, "NFA_CLASS_ALPHA"); break;
	case NFA_CLASS_BLANK:	STRCPY(code, "NFA_CLASS_BLANK"); break;
	case NFA_CLASS_CNTRL:	STRCPY(code, "NFA_CLASS_CNTRL"); break;
	case NFA_CLASS_DIGIT:	STRCPY(code, "NFA_CLASS_DIGIT"); break;
	case NFA_CLASS_GRAPH:	STRCPY(code, "NFA_CLASS_GRAPH"); break;
	case NFA_CLASS_LOWER:	STRCPY(code, "NFA_CLASS_LOWER"); break;
	case NFA_CLASS_PRINT:	STRCPY(code, "NFA_CLASS_PRINT"); break;
	case NFA_CLASS_PUNCT:	STRCPY(code, "NFA_CLASS_PUNCT"); break;
	case NFA_CLASS_SPACE:	STRCPY(code, "NFA_CLASS_SPACE"); break;
	case NFA_CLASS_UPPER:	STRCPY(code, "NFA_CLASS_UPPER"); break;
	case NFA_CLASS_XDIGIT:	STRCPY(code, "NFA_CLASS_XDIGIT"); break;
	case NFA_CLASS_TAB:	STRCPY(code, "NFA_CLASS_TAB"); break;
	case NFA_CLASS_RETURN:	STRCPY(code, "NFA_CLASS_RETURN"); break;
	case NFA_CLASS_BACKSPACE:   STRCPY(code, "NFA_CLASS_BACKSPACE"); break;
	case NFA_CLASS_ESCAPE:	STRCPY(code, "NFA_CLASS_ESCAPE"); break;
        
	case NFA_ANY:	STRCPY(code, "NFA_ANY"); break;
	case NFA_IDENT:	STRCPY(code, "NFA_IDENT"); break;
	case NFA_SIDENT:	STRCPY(code, "NFA_SIDENT"); break;
	case NFA_KWORD:	STRCPY(code, "NFA_KWORD"); break;
	case NFA_SKWORD:	STRCPY(code, "NFA_SKWORD"); break;
	case NFA_FNAME:	STRCPY(code, "NFA_FNAME"); break;
	case NFA_SFNAME:	STRCPY(code, "NFA_SFNAME"); break;
	case NFA_PRINT:	STRCPY(code, "NFA_PRINT"); break;
	case NFA_SPRINT:	STRCPY(code, "NFA_SPRINT"); break;
	case NFA_WHITE:	STRCPY(code, "NFA_WHITE"); break;
	case NFA_NWHITE:	STRCPY(code, "NFA_NWHITE"); break;
	case NFA_DIGIT:	STRCPY(code, "NFA_DIGIT"); break;
	case NFA_NDIGIT:	STRCPY(code, "NFA_NDIGIT"); break;
	case NFA_HEX:	STRCPY(code, "NFA_HEX"); break;
	case NFA_NHEX:	STRCPY(code, "NFA_NHEX"); break;
	case NFA_OCTAL:	STRCPY(code, "NFA_OCTAL"); break;
	case NFA_NOCTAL:	STRCPY(code, "NFA_NOCTAL"); break;
	case NFA_WORD:	STRCPY(code, "NFA_WORD"); break;
	case NFA_NWORD:	STRCPY(code, "NFA_NWORD"); break;
	case NFA_HEAD:	STRCPY(code, "NFA_HEAD"); break;
	case NFA_NHEAD:	STRCPY(code, "NFA_NHEAD"); break;
	case NFA_ALPHA:	STRCPY(code, "NFA_ALPHA"); break;
	case NFA_NALPHA:	STRCPY(code, "NFA_NALPHA"); break;
	case NFA_LOWER:	STRCPY(code, "NFA_LOWER"); break;
	case NFA_NLOWER:	STRCPY(code, "NFA_NLOWER"); break;
	case NFA_UPPER:	STRCPY(code, "NFA_UPPER"); break;
	case NFA_NUPPER:	STRCPY(code, "NFA_NUPPER"); break;
	
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
    fprintf(debugf, "(%2d)", abs(state->id));
    for (i=0;i<ident; i++)
	fprintf(debugf,"%c", ' ');

    nfa_set_code(state->c);
    fprintf(debugf, "%s (%d) (id=%d)\n", code, state->c, abs(state->id));
    if (state->id < 0)
	return;
    
    state->id = abs(state->id) * -1;
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
    s->negated = FALSE;

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

    l = (Ptrlist *)outp;
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
    static Ptrlist *
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
static Frag *stack;

/*
 * Convert a postfix form into its equivalent NFA.
 * Return the NFA start state on success, NULL otherwise.
 */
    static nfa_state_T *
post2nfa(postfix, end, nfa_calc_size)
    int		*postfix;
    int		*end;
    int		nfa_calc_size;
{
    int		*p, st_error = FALSE;
    Frag	*stackp, *stack_end, e1, e2, e;
//    Frag	empty;
    nfa_state_T	*s, *s1, *matchstate;

    if (postfix == NULL)
        return NULL;

/* 
 * The next definitions of PUSH and POP do not work as expected ...
 *
#define PUSH(s)						    \
		if (stackp >= stack_end)		    \
		    EMSG("E999: NFA Stack error: Could not Push!");\
		else					    \
		    *stackp++ = (s)
	        
#define POP()								\
	*--stackp;							\
	if (stackp <= stack)						\
	    return NULL;						

*/

#define PUSH(s) {                           \
                    if (stackp >= stack_end)\
                        return NULL;        \
                     *stackp++ = s;         \
                }
#define POP()   ({                          \
                    if (stackp <= stack)    \
                        return NULL;        \
                    *--stackp;              \
                 })

    if (nfa_calc_size == FALSE)
    {
	/* Allocate space for the stack. Max states on the stack : nstate */
	stack = (Frag *) lalloc((nstate+1)*sizeof(Frag), TRUE);
	stackp = stack;
	stack_end = stack + NFA_STACK_SIZE;
//	empty = frag(NULL,NULL);
    }

    for (p = postfix; p < end && !st_error; ++p)
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

	case NFA_NOT:		/* Negation of a character */
	    if (nfa_calc_size == TRUE)
	    {
		nstate += 1;
		break;
	    }
	    /* Create state s for NFA_NOT */
	    s = new_state(NFA_NOT, NULL, NULL);
    	    if (s == NULL)
    	        return NULL;
    	    PUSH(frag(s, list1(&s->out)));
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

	case NFA_QUEST:		/* one or zero atoms=> greedy match */
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

	case NFA_QUEST_NONGREEDY:	/* zero or one atoms => non-greedy match */
	    if (nfa_calc_size == TRUE)
	    {
		nstate ++;
		break;
	    }
	    e = POP();
	    s = new_state(NFA_SPLIT, NULL, e.start);
	    if (s == NULL)
	        return NULL;
	    PUSH(frag(s, append(e.out, list1(&s->out))));
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
	
	/* Symbol of 0-length, Used in a repetition 
	 * with max/min count of 0 */
	case NFA_SKIP_CHAR:
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
	    if (*(p+1) && (*(p+1) == NFA_NOT))
		s->negated = TRUE;
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
	EMSG_RET_NULL("E999: (NFA regexp) (While converting from postfix to NFA), too many states left on stack ");

    if (istate >= nstate)
	EMSG_RET_NULL("E999: (NFA regexp) Not enough space to store the whole NFA ");

    if (st_error == TRUE)
	EMSG_RET_NULL("E999: (NFA regexp) Error popping and pushing to/from the stack ");

    vim_free(stack);

    matchstate = &state_ptr[istate++]; /* the match state */
    matchstate->c = NFA_MATCH;

    patch(e.out, matchstate);
    return e.start;

#undef POP
#undef PUSH
}

/* thread_T contains runtime information of a NFA state */
struct thread
{
    nfa_state_T	*state;
    regsub_T	sub;
};

typedef struct
{
    thread_T	*t;
    int		n;
} List;

static List list[3];

static int	listid;
List	*thislist, *nextlist, *neglist;

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

    /* TODO(RE) Optimize next if ... what ids should not be remembered? */

    /* Only remember states with printable chars or 
     * beginning of groups, and codes used in nfa_regmatch() */
    if (state->c > 0 
	|| (state->c - NFA_MOPEN >=0 && state->c - NFA_MOPEN <=9)
	|| (state->c == NFA_MATCH)  || (state->c == NFA_BOW) || (state->c == NFA_EOW)
	|| (state->c == NFA_ANY)    || (state->c == NFA_BOL) || (state->c == NFA_EOL)
	|| (state->c == NFA_NEWL)   || (state->c == NFA_END_NEG_RANGE)
	|| (state->c >= NFA_CLASS_ALNUM && state->c <= NFA_CLASS_ESCAPE)	/* [:alpha:] */
	|| (state->c >= '\300' && state->c <= '\377')				/* equivalence classes */
	|| (state->c >= NFA_ANY && state->c <= NFA_NUPPER)			/* \a, \d etc */
	|| (state->c >= NFA_FIRST_NL && state->c <= NFA_LAST_NL)		/* \n + \a, \d etc */
	)
    {	
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
    }
#ifdef ENABLE_LOG_FILE
    nfa_set_code(state->c);
    fprintf(f, "> Adding state %d to %slist. Character %s, code %d\n", 
	abs(state->id), (l == thislist? "this" : 
			(l == neglist ? "neg" : "next" ) ), code, state->c);
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

	case NFA_END_NEG_RANGE:
	    /* End a [^xyz....] construction: accept any char and advance */
//	    addstate(l, state->out, m, off, lid, match);
	    break;

	case NFA_NOT:
	    EMSG("E999: (NFA regexp internal error) Should not process NOT node !");
#ifdef ENABLE_LOG_FILE
	fprintf(f, "\n\n>>> E999: Added state NFA_NOT to a list ... Something went \
wrong ! Why wasn't it processed already? \n\n");
#endif

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

/* Check character class "class" against current character c. */
static int check_char_class(class, c)
    int		class;
    int		c;
{
    switch (class)
    {
	case NFA_CLASS_ALNUM:
	    if (isalnum(c))
		return OK;
	    break;
	case NFA_CLASS_ALPHA:
	    if (isalpha(c))
		return OK;
	    break;
	case NFA_CLASS_BLANK:
	    if (c == ' ' || c == '\t')
		return OK;
	    break;
	case NFA_CLASS_CNTRL:
	    if (iscntrl(c))
		return OK;
	    break;
	case NFA_CLASS_DIGIT:
	    if (VIM_ISDIGIT(c))
		return OK;
	    break;
	case NFA_CLASS_GRAPH:
	    if (isgraph(c))
		return OK;
	    break;
	case NFA_CLASS_LOWER:
	    if (MB_ISLOWER(c))
		return OK;
	    break;
	case NFA_CLASS_PRINT:
	    if (vim_isprintc(c))
		return OK;
	    break;
	case NFA_CLASS_PUNCT:
	    if (ispunct(c))
		return OK;
	    break;
	case NFA_CLASS_SPACE:
	    if ((c >=9 && c <= 13) || (c == ' '))
		return OK;
	    break;
	case NFA_CLASS_UPPER:
	    if (MB_ISUPPER(c))
		return OK;
	    break;
	case NFA_CLASS_XDIGIT:
	    if (vim_isxdigit(c))
		return OK;
	    break;
	case NFA_CLASS_TAB:
	    if (c == '\t')
		return OK;
	    break;
	case NFA_CLASS_RETURN:
	    if (c == '\r')
		return OK;
	    break;
	case NFA_CLASS_BACKSPACE:
	    if (c == '\b')
		return OK;
	    break;
	case NFA_CLASS_ESCAPE:
	    if (c == '\033')
		return OK;
	    break;
	
	default:
	    /* should not be here :P */
	    EMSG_RET_FAIL("E999: (NFA regexp) Invalid character class ");
    }
    return FAIL;
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
    int		c, n, i = 0, result;
    int		match = FALSE, negate = FALSE;
    int		flag = 0;
    int		j = 0;
    int		reginput_updated = FALSE;
    thread_T	*t;
    char_u	*cc;

    static 	regsub_T m;
    c = -1;

    if (REG_MULTI)
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

#ifdef ENABLE_LOG_FILE
    f = fopen("log_nfarun.log","a");
    fprintf(f, "\n\n\n\n\n\n=======================================================\n");
    fprintf(f, "=======================================================\n\n\n\n\n\n\n");
    nfa_print_state(f, start, 0);
#endif

    thislist = &list[0];
    thislist->n = 0;
    neglist = &list[2];
    neglist->n = 0;
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
	cc = (char_u *)&c;

	/* swap lists */
	thislist = &list[flag];
	nextlist = &list[flag ^= 1];
	nextlist->n = 0;	    /* `clear' nextlist */
	++listid;
#ifdef ENABLE_LOG_FILE
    fprintf(f, "------------------------------------------\n");
    fprintf(f, ">>> Advanced one character ... Current char is %c (code %d) \n", c, (int)c);
    fprintf(f, ">>> Thislist has %d states available: ", thislist->n);
    for (i = 0; i< thislist->n; i++)
	fprintf(f, "%d  ", abs(thislist->t[i].state->id));
    fprintf(f, "\n");
#endif

	/* compute nextlist */
	for (i = 0; i < thislist->n || neglist->n > 0; ++i)
	{
	    if (neglist->n > 0)
	    {
		t = &neglist->t[0];
		neglist->n --;
		i--;
	    }
	    else
		t = &thislist->t[i];
	    /* Negation ? */
	    negate = FALSE;
	    if (t->state->out && t->state->out->c == NFA_NOT)
		negate = TRUE;

#ifdef ENABLE_LOG_FILE
    nfa_set_code(t->state->c);
    fprintf(f, "(%d) %s, code %d ... \n", abs(t->state->id), code, (int)t->state->c);
#endif

#define	ADD_POS_NEG_STATE()				    \
		/* Check if current char matches */	    \
		if (negate == FALSE && result == OK)	    \
		    addstate(nextlist, t->state->out, &t->sub, n, listid+1, &match);	\
		else					    \
		/* Check if current char doesn't match */   \
		if (negate == TRUE && result != OK)	    \
		    addstate(neglist, t->state->out->out, &t->sub, n, listid+1, &match);

	    switch (t->state->c)
	    {
	    case NFA_MATCH:
		match = TRUE;
		*submatch = t->sub;
#ifdef ENABLE_LOG_FILE
    for (j = 0; j < 4; j++)
	if (REG_MULTI)
	    fprintf(f, "\n *** group %d, start: c=%d, l=%d, end: c=%d, l=%d", j, t->sub.startpos[j].col, (int)t->sub.startpos[j].lnum, t->sub.endpos[j].col, (int)t->sub.endpos[j].lnum);
	else	
	    fprintf(f, "\n *** group %d, start: \"%s\", end: \"%s\"", j, (char *)t->sub.start[j], (char *)t->sub.end[j]);
    fprintf(f, "\n");
#endif
		goto nextchar;		/* found the left-most longest match */
		break;

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

	    case NFA_NEWL:
		if (!reg_line_lbr && REG_MULTI
				&& c == NUL && reglnum <= reg_maxline)
		{
		    reg_nextline();
		    addstate(nextlist, t->state->out, &t->sub, n, listid+1, &match);
		    reginput_updated = TRUE;
		}
		break;

	    case NFA_CLASS_ALNUM:
	    case NFA_CLASS_ALPHA:
	    case NFA_CLASS_BLANK:
	    case NFA_CLASS_CNTRL:
	    case NFA_CLASS_DIGIT:
	    case NFA_CLASS_GRAPH:
	    case NFA_CLASS_LOWER:
	    case NFA_CLASS_PRINT:
	    case NFA_CLASS_PUNCT:
	    case NFA_CLASS_SPACE:
	    case NFA_CLASS_UPPER:
	    case NFA_CLASS_XDIGIT:
	    case NFA_CLASS_TAB:
	    case NFA_CLASS_RETURN:
	    case NFA_CLASS_BACKSPACE:
	    case NFA_CLASS_ESCAPE:
		result = check_char_class(t->state->c, c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_END_NEG_RANGE:
		/* This follows a series of negated nodes, like: 
		 * CHAR(x), NFA_NOT, CHAR(y), NFA_NOT etc */
		if (c > 0)
		  addstate(nextlist, t->state->out, &t->sub, n, listid+1, &match);
		break;

	    case NFA_ANY:	    
		/* Any printable char, not just any char. '\0' (end of input) must not match */
		if (c > 0)
		  addstate(nextlist, t->state->out, &t->sub, n, listid+1, &match);
		break;

/* Character classes like \a for alpha, \d for digit etc */
	    case NFA_IDENT:	/*  \i	*/
		result = vim_isIDc(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_SIDENT:	/*  \I	*/
		result = !VIM_ISDIGIT(c) && vim_isIDc(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_KWORD:	/*  \k	*/
		result = vim_iswordp(cc);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_SKWORD:	/*  \K	*/
		result = !VIM_ISDIGIT(c) && vim_iswordp(cc);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_FNAME:	/*  \f	*/
		result = vim_isfilec(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_SFNAME:	/*  \F	*/
		result = !VIM_ISDIGIT(c) && vim_isfilec(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_PRINT:	/*  \p	*/
		result = ptr2cells(cc) == 1;
		ADD_POS_NEG_STATE();
		break;

	    case NFA_SPRINT:	/*  \P	*/
		result = !VIM_ISDIGIT(c) && ptr2cells(cc) == 1;
		ADD_POS_NEG_STATE();
		break;

	    case NFA_WHITE:	/*  \s	*/
		result = vim_iswhite(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_NWHITE:	/*  \S	*/
		result = c != NUL && !vim_iswhite(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_DIGIT:	/*  \d	*/
		result = ri_digit(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_NDIGIT:	/*  \D	*/
		result = c != NUL && !ri_digit(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_HEX:	/*  \x	*/
		result = ri_hex(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_NHEX:	/*  \X	*/
		result = c != NUL && !ri_hex(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_OCTAL:	/*  \o	*/
		result = ri_octal(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_NOCTAL:	/*  \O	*/
		result = c != NUL && !ri_octal(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_WORD:	/*  \w	*/
		result = ri_word(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_NWORD:	/*  \W	*/
		result = c != NUL && !ri_word(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_HEAD:	/*  \h	*/
		result = ri_head(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_NHEAD:	/*  \H	*/
		result = c != NUL && !ri_head(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_ALPHA:	/*  \a	*/
		result = ri_alpha(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_NALPHA:	/*  \A	*/
		result = c != NUL && !ri_alpha(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_LOWER:	/*  \l	*/
		result = ri_lower(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_NLOWER:	/*  \L	*/
		result = c != NUL && !ri_lower(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_UPPER:	/*  \u	*/
		result = ri_upper(c);
		ADD_POS_NEG_STATE();
		break;

	    case NFA_NUPPER:	/* \U	*/
		result = c != NUL && !ri_upper(c);
		ADD_POS_NEG_STATE();
		break;

	    default:	/* regular character */
		result =    (
			(t->state->c == c) || 
			(ireg_ic == TRUE && MB_TOLOWER(t->state->c) == MB_TOLOWER(c)) ||
			(un_Magic(t->state->c) == c)
			    );
		ADD_POS_NEG_STATE();
		break;
	    }

	}   /* for (thislist = thislist; thislist->state; thislist++) */

	/* The first found match is the leftmost one, but there may be a 
	 * longer one. Keep running the NFA, but don't start from the 
	 * beginning */
	if (match == FALSE)
	{
	    addstate(nextlist, start, &m, n, listid+1, &match);
	}

        if (reginput_updated)
        {
	   reginput_updated = FALSE;
	   goto again;
        }

#ifdef ENABLE_LOG_FILE
    fprintf(f, ">>> Thislist had %d states available: ", thislist->n);
    for (i = 0; i< thislist->n; i++)
	fprintf(f, "%d  ", abs(thislist->t[i].state->id));
    fprintf(f, "\n");
#endif

nextchar:
#ifdef FEAT_MBYTE
	/* TODO(RE) Check for following composing character. */
#endif
	    reginput += n;
    } while (c || reginput_updated);

#ifdef ENABLE_LOG_FILE
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
	prog = (nfa_regprog_T *)reg_mmatch->regprog;
	line = reg_getline((linenr_T)0);    /* relative to the cursor */
	reg_startpos = reg_mmatch->startpos;
	reg_endpos = reg_mmatch->endpos;
    }
    else
    {
	prog = (nfa_regprog_T *)reg_match->regprog;
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

    size = (nstate + 1) * sizeof(thread_T);

    list[0].t = (thread_T *)lalloc(size, TRUE);
    list[1].t = (thread_T *)lalloc(size, TRUE);
    list[2].t = (thread_T *)lalloc(size, TRUE);
    if (list[0].t == NULL || list[1].t == NULL || list[2].t == NULL)
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
    vim_free(list[2].t);
    list[0].t = NULL;
    list[1].t = NULL;
    list[2].t = NULL;

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
     * In order to build the NFA, we parse the input regexp twice:
     * 1. first pass to count size (so we can allocate space)
     * 2. second to emit code 
     */
#ifdef ENABLE_LOG_FILE
    FILE *f = fopen("log_nfarun.log", "a");
    if (f)
    {
	fprintf(f, "\n***************\n\n\n\nCompiling regexp \"%s\" ... hold on !\n\n\n", expr);
	fclose(f);
    }
#endif

    /* PASS 1 
     * Count number of NFA states in "nstate". Do not build the NFA. 
     */
    post2nfa(postfix, post_ptr, TRUE);
    state_ptr = prog->state;

    /* PASS 2
     * Build the NFA 
     */
    prog->start = post2nfa(postfix, post_ptr, FALSE);
    if (prog->start == NULL)
	goto fail;

    prog->regflags = regflags;
    prog->engine = &nfa_regengine;
    prog->nstate = nstate;
#ifdef ENABLE_LOG_FILE
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
#ifdef ENABLE_LOG_FILE
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

