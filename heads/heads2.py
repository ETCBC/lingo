'''
This module contains a class, getHeads,
which runs a series of templates against
an instance of the BHSA database. The templates
select heads from all phrase types. In cases where
the BHSA encodes a quantifier as a head when it 
has a quantified object, the templates select the
quantified object instead. This module also
selects prepositional object heads using the same 
logic.
'''

class getHeads:

    '''
    Three features are produced and delivered by this module:
    head = phrase head to phrase mapping according to phrase type and word independency tests;
           also performed for phrase_atoms by removing phrase_atom independency tests. To be 
           exported as an edge feature from a head to a phrase.
    obj_prep = object of preposition mapping to the preposition according to word independency tests;
               to be exported as an edge feature from an object to its preposition.
    nhead = nominal head (subs, adjv, etc.) within any phrase, including prepositional phrases, with independency tests
            to be exported as an edge feature from an nhead to a phrase.

    Each template is stored in a search dictionary which contains
    the template and head/obj indices to the result.
    
    The resulting feature dictionaries are stored as getHeads.head, getHeads.obj_prep, and getHeads.nhead.
    '''
    
    def __init__(self, TF_api):
        
        # shorthand TF methods
        S, T, F, L, E = TF_api.S, TF_api.T, TF_api.F, TF_api.L, TF_api.E
        
        # preprocessing data
        print('preparing preprocessing sets...')
        