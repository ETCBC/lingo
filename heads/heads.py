'''
This module contains the code used to export edge relations
for phrase(atom) heads in the ETCBC's BHSA Hebrew data.

N.B. the functions here are older and are in the process of
being simplified and expanded to cover all phrase types.
'''

def get_heads(phrase, diagnose=False):
    '''
    Returns substantive head nouns, if there are any, from a phrase node.
    "substantive" does not include prounouns.
    
    Based on a supplied phrase get phrase atom and subphrase features 
    and compare them against a group of sets.
    Define those sets first. Then make the comparison.
    
    *Note*
    Currently this function has been tested thoroughly only with phrases
    that function as a subject or object within the clause. Theoretically
    it should work with nearly any phrase type. But that has yet to be tested.
    Also, the algorithm currently excludes pronouns.
    '''
    
    good_sp = {'subs', 'nmpr', 'adjv'}
    good_pdp = {'subs', 'nmpr'}
        
    heads = [] # nouns go here
    phrase_words = L.d(phrase, 'word')
        
    for word in phrase_words:
        
        # get phrases's phrase atoms, subphrases, and subphrase relations
        phrase_atom = L.u(word, 'phrase_atom')[0]
        subphrases = L.u(word, 'subphrase') 
        sp_relas = set(F.rela.v(sp) for sp in subphrases)
        
        test_good = [F.pdp.v(word) in good_pdp, # is noun
                     F.sp.v(word) in good_sp, # is noun
                     good_phrs_type(phrase_atom, subphrases, diagnose), # is NP or PP with את
                     independent(phrase_atom, subphrases, heads, diagnose) 
                    ] # is valid subphrase rela.
        
        # compare word/phrase features
        if all(test_good):
        
            # handle quantifiers
            quants = {'KL/', 'M<V/'}
            if F.lex.v(word) in quants or F.ls.v(word) == 'card':
                genitive_head = get_quantified(word, good_pdp, good_sp) # returns word node or None
                if genitive_head:
                    heads.append(genitive_head) # valid quantified noun found
                else:
                    continue # no noun found, skip it
            else:
                heads.append(word) # word is a head
    
        else:
            if diagnose: 
                print(T.text([word]), word)
                print('test_good', tuple(zip(test_good, ('pdp', 'sp', 'phr_typ', 'indep.'))))
                print('subphrases', subphrases)
                print('phrase_atom', phrase_atom)
                print()
            continue
            
    return heads

def good_phrs_type(phrase_atom, subphrases, diagnose=False):
    '''
    Return boolean on whether a phrase atom is an acceptable type.
    Acceptable is either a noun phrase (NP) or
    a prepositional phrase (PP) that is governed only by את.
    '''

    if F.typ.v(phrase_atom) == 'NP': # noun phrase
        return True
    
    # for logic on this selection criteria, see [?]
    prep_sp = sorted(sp for sp in subphrases # sorted sp with prepositions
                         if 'prep' in set(F.pdp.v(w) for w in L.d(sp, 'word')))
    phrase_type = prep_sp or (phrase_atom,)
    preps = [w for w in L.d(phrase_type[0], 'word') 
                    if F.pdp.v(w) == 'prep']
    
    if F.typ.v(phrase_atom) == 'PP' and preps: # check for את
        prep = preps[0]
        if F.lex.v(prep) == '>T':
            return True
        else:
            if diagnose:
                print('>T not found...')
            return False
    else:
        if diagnose:
            print('neither NP or PP...')
            print('phrase_type: ', phrase_type)
        return False

def get_quantified(abs_wnode, good_pdp, good_sp, diagnose=False):
    '''
    Extract the genitive noun in a construct chain with a quantifier.
    The function simply returns the first substantive in the chain.
    '''
    
    rectum = E.mother.t(abs_wnode) # get rectum subphrase
    abs_phrase = L.d(L.u(abs_wnode, 'phrase')[0], 'word') # for phrase boundary
    
    if not rectum:
        if diagnose:
            print('no rectum found at word', abs_wnode)
        return None  # abs not in norm. construct (e.g. w/ verbs)
    
    # get words and nouns in the rectum subphrase
    r_words = L.d(rectum[0], 'word')
    r_nouns = [w for w in r_words 
                   if F.sp.v(w) in good_sp
                   and F.pdp.v(w) in good_pdp
                   and w in abs_phrase]
    if r_nouns:
        return r_nouns[0] # return the first noun
    else:
        if diagnose:
            print('no noun found for word', abs_wnode)
        return None # no noun found, return nothing
    
def independent(phrase_atom, subphrases, heads_list, diagnose=False):
    
    '''
    Checks phrase and subphrase relations for dependency relations.
    Requires a list of previously analyzed head nouns.
    This list is required to double check parallel (coordinate) relations. 
    '''
    # exclude words in phrase_atoms with these relation features
    omit_pa_rela = {'Appo', # apposition
                    'Spec'} # specification
    
    # exclude words in subphrases with these relation features
    omit_sp_rela = {'rec', # nomen rectum
                    'adj', # adjunct 
                    'atr', # attributive
                    'mod', # modifier
                    'dem'} # demontrative
    
    parallels = {'par', 'Para'} # parallel i.e. coordination specification
    omit_relas = omit_pa_rela | omit_sp_rela
    phrase_units = list(subphrases) + [phrase_atom] # phrase atom & subphrase 
    relas = set(F.rela.v(obj) for obj in phrase_units) # phrase atom & subphrase relas
    
    if not relas & omit_relas and not parallels & relas: # good relas
        return True
    
    elif not relas & omit_relas and parallels & relas: # check parallel relations
        
        # assemble acceptable phrase mothers from the already accepted head nouns
        head_mothers = set(L.u(w, 'phrase_atom')[0] for w in heads_list)
        head_mothers |= set(sp for w in heads_list
                               for sp in L.u(w, 'subphrase'))
        
        for pu in phrase_units:
            if F.rela.v(pu) in parallels:
                mother = E.mother.f(pu)[0]
                if mother in head_mothers:
                    return True
                else:
                    if diagnose:
                        print('False independence: ')
                        print('phrase units', phrase_units)
                        print('mothers', head_mothers)
                        print('head mothers', head_mothers)
                        print()
                        
                    return False
                
    else: # noun is not independent
        if diagnose:
            print('not an acceptable rela...')
        return False  