'''
This module contains the code used to produce the heads.tf and 
prep_obj.tf features. This code is a work-in-progress. Retrieving
phrase heads requires a lot of choices about syntax and semantics. It 
also requires an underlying data model that is reliable and fine-grained
enough to handle complex phrase structures. The ETCBC data model is not
fully sufficient to handle these cases. Thus, this module itself does not
produce perfect results. Any suggestions for improvement are greatly
appreciated.

-Cody Kingham, ETCBC, 16.04.18
'''

def get_heads(phrase, api):
    '''
    Extracts and returns the heads of a supplied
    phrase or phrase atom based on that phrase's type
    and the relations reflected within the phrase.
    
    --input--
    phrase(atom) node number
    
    --output--
    tuple of head word node(s) 
    '''
    
    F, E, T, L = api.F, api.E, api.T, api.L # TF data methods
    
    # mapping from phrase type to good part of speech values for heads
    head_pdps = {'VP': {'verb'},                   # verb 
                 'NP': {'subs', 'adjv', 'nmpr'},   # noun 
                 'PrNP': {'nmpr', 'subs'},         # proper-noun 
                 'AdvP': {'advb', 'nmpr', 'subs'}, # adverbial 
                 'PP': {'prep'},                   # prepositional 
                 'CP': {'conj', 'prep'},           # conjunctive
                 'PPrP': {'prps'},                 # personal pronoun
                 'DPrP': {'prde'},                 # demonstrative pronoun
                 'IPrP': {'prin'},                 # interrogative pronoun
                 'InjP': {'intj'},                 # interjectional
                 'NegP': {'nega'},                 # negative
                 'InrP': {'inrg'},                 # interrogative
                 'AdjP': {'adjv'}                  # adjective
                } 
    
    # get phrase-head's part of speech value and list of candidate matches
    phrase_type = F.typ.v(phrase)
    head_candidates = [w for w in L.d(phrase, 'word')
                          if F.pdp.v(w) in head_pdps[phrase_type]]
        
    # VP with verbs require no further processing, return the head verb
    if phrase_type == 'VP':        
        return tuple(head_candidates)
        
    # go head-hunting!
    heads = []
    
    for word in head_candidates:
        
        # gather the word's subphrase (+ phrase_atom if otype is phrase) relations
        word_phrases = list(L.u(word, 'subphrase'))
        word_phrases += list(L.u(word, 'phrase_atom')) if (F.otype.v(phrase) == 'phrase') else list()
        word_relas = set(F.rela.v(phr) for phr in word_phrases) or {'NA'}

        # check (sub)phrase relations for independency
        if word_relas - {'NA', 'par', 'Para'}: 
            continue
            
        # check parallel relations for independency
        elif all([word_relas & {'par', 'Para'},
                  mother_is_head(word_phrases, heads, api),
                  check_preposition(word, phrase, api),
                  #check_parallel_quants(word, api)
                 ]):
            
            this_head = find_quantified(word, api) or find_attributed(word, api) or word
            heads.append(this_head)
            
        # save all others as heads, check for quantifiers first
        elif all([word_relas == {'NA'},
                  check_preposition(word, phrase, api)
                 ]):
            
            this_head = find_quantified(word, api) or find_attributed(word, api) or word
            heads.append(this_head)
            
    return tuple(sorted(set(heads)))
            
def mother_is_head(word_phrases, previous_heads, api):
    
    '''
    Test and validate parallel relationships for independency.
    Must gather the mother for each relation and check whether 
    the mother contains a head word. 
    
    --input--
    * list of phrase nodes for a given word (includes subphrases)
    * list of previously approved heads
    
    --output--
    boolean
    '''
    
    F, E, T, L = api.F, api.E, api.T, api.L # TF data methods
    
    # get word's enclosing phrases that are parallel
    parallel_phrases = [ph for ph in word_phrases if F.rela.v(ph) in {'par', 'Para'}]
    # get the mother for the parallel phrases
    parallel_mothers = [E.mother.f(ph)[0] for ph in parallel_phrases] 
    # get mothers' words, by mother
    parallel_mom_words = [set(L.d(mom, 'word')) for mom in parallel_mothers]
    # test for head in each mother
    test_mothers = [bool(phrs_words & set(previous_heads)) for phrs_words in parallel_mom_words] 
        
    return all(test_mothers)
    

def find_quantified(word, api):
    
    '''        
    Check whether a head candidate is a quantifier (e.g. כל).
    If it is, find the quantified noun if there is one.
    Quantifiers may be connected with the modified noun
    by a subphrase relation of "rec" for nomen regens. 
    In this case, the quantifier word node is the
    mother itself. In other cases, the noun is related to the
    number via the "atr" (attributive) subphrase relation. In this
    case, the edge relation is connected from the substantive
    to the number's subphrase.
    
    --input--
    word node
    
    --output--
    new word node or None
    '''
    
    F, E, T, L = api.F, api.E, api.T, api.L # TF data methods
    
    custom_quants = {'KL/', 'M<V/', 'JTR/', # quantifier lexemes, others?
                     'M<FR/', 'XYJ/'} 
    good_pdps = {'subs', # substantive
                 'nmpr', # proper noun
                 'prde', # demonstrative
                 'prps', # pronoun
                 'verb'} # "verb" for participles, see the inquiries below.
    
    if F.lex.v(word) not in custom_quants and F.ls.v(word) not in {'card', 'ordn'}:        
        return None
    
    # first check rec relations for valid quantified noun:
    rectum = next((sp for sp in E.mother.t(word) if F.rela.v(sp) == 'rec'), 0) # extract the rectum
    noun = next((w for w in L.d(rectum, 'word') if F.pdp.v(w) in good_pdps), 0) # filter words for noun
    num_check = F.ls.v(L.u(noun, 'lex')[0]) if noun else ''
    if noun and num_check not in {'card', 'ordn'}:
        return noun
    
    # check the adjunct relation if no rec found:
    subphrases = sorted(L.u(word, 'subphrase'))    
    # move progressively from smallest to largest subphrase, stop when non-cardinal noun is found  
    for sp in subphrases:        
        candidates = sorted(daughter for daughter in E.mother.t(sp) if F.rela.v(daughter) == 'adj')
        for candi in candidates:
            noun = next((w for w in L.d(candi, 'word') if F.pdp.v(w) in good_pdps), 0)
            num_check = F.ls.v(L.u(noun, 'lex')[0]) if noun else ''
            if noun and num_check not in {'card', 'ordn'}:                
                return noun
    
    # all else are non-quantifiers
    return None

def check_parallel_quants(word, api):
    '''
    *TEMPORARY FIX*
    Check whether the head candidate is a quantifier in parallel
    with another quantifier. These are cases such as Gen 5:3:
        שְׁלֹשִׁ֤ים וּמְאַת֙ שָׁנָ֔ה
    cases like ^these result in a number being selected as a 
    head alongside the quantified noun. So, in this case the
    pulled heads are שלש and שנה whereas it should simply return שנה.
    
    This fix makes a simple check to see whether 1. whether the word
    is a quantifier, and 2. if it is a quantifier, if it is paralleled
    by another quantifier. The function only checks subphrase relations.
    '''
    

def find_attributed(word, api):
    
    '''        
    Check whether the head candidate is an adjective.
    If it is, retrieve its attributed noun via the
    regens (rec) relationship.
    
    This function is similar to the quantified function.
    
    --input--
    word node
    
    --output--
    new word node or None
    '''
    
    F, E, T, L = api.F, api.E, api.T, api.L # TF data methods
    
    if F.pdp.v(word) != 'adjv':
        return None
    
    # check rec relations for valid attributed noun:
    rectum = next((sp for sp in E.mother.t(word) if F.rela.v(sp) == 'rec'), 0) # extract the rectum
    noun = next((w for w in L.d(rectum, 'word') if F.pdp.v(w) == 'subs'), 0) # filter words for noun
    if noun:
        return noun
    
    # sanity check: adjectives should not 
    # pass through this algorithm without a noun assignment
    if F.typ.v(L.u(word, 'phrase')) == 'NP':
        raise Exception(f'adjective head assignment on NP {L.u(word, "phrase")} at word {word}')
    else:
        return None
    
def check_preposition(word, phrase, tf_api):
    
    '''
    Objects of prepositions do not have
    any relational information in the database.
    Therefore, cases of prepositions which are 
    immediately preceded by another preposition
    must be excluded since they are not true heads.
    
    --input--
    word node and phrase nodes
    
    --output--
    boolean on whether head
    '''
    
    F, L = tf_api.F, tf_api.L
    
    if F.pdp.v(word) != 'prep':
        return True
    
    # look for preceding preposition
    ph_boundaries = L.d(phrase, 'word')
    if F.pdp.v(word-1) == 'prep' and word-1 in ph_boundaries:
        return False
    
    else:
        return True