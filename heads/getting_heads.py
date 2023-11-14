# -*- coding: utf-8 -*-
# ---
#   jupyt_pdp_safe@-ext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.13.1
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

# %% [markdown]
# # Getting Heads 😶
# ## By Cody Kingham, in collaboration with Christiaan Erwich
#
# ## Problem Description
# The ETCBC's BHSA core data does not contain the standard syntax tree format. This also means that syntactic and functional relationships between individual words are not mapped in a transparent or easily accessible way. In some cases, fine-grained relationships are ignored altogether. For example, for a given noun phrase (NP), there is no explicit way of obtaining its head noun (i.e. the noun itself without any modifying elements). This causes numerous problems for research in the realm of semantics. For instance, it is currently very difficult to calculate the complete person, gender, and number (PGN) of a given subject phrase. That is because PGN is stored at the word level only. But this is a very inadequate representation. Phrases in the ETCBC often contain coordinate relationships within the phrase. So even if one selects the first "noun" in the phrase and checks for its PGN value, they may overlook the presence of another noun which makes the phrase plural. Ideally, the phrase itself would have a PGN feature. But before this kind of data is created, it is necessary to separate the head words of a phrase from their modifying elements such as adjectives, determiners, or nouns in construct (genitive) relations.
#
# A head word can be defined as the word for which a phrase type is named after. Examples of phrase types are "NP" (noun phrase) or "VP" (verb phrase). In this notebook, we experiment with and build the functions stored in `heads.py` in order to export a set of Text-Fabric edge features. The edge features represent a mapping from a phrase node to its head element.
#
# This goal requires us to think carefully about the way inter-word, semantic relations are reflected in the ETCBC's data. The ETCBC *does* contain some rudimentary semantic embeddings through the so-called [subphrase](https://etcbc.github.io/bhsa/features/hebrew/c/otype). These can be utilized to isolate head words from secondary elements. A subphrase should *not* be thought of as a smaller, embedded phrase, like the ETCBC's phrase-atom (though it sometimes must indadequately fill that role). Rather, the subphrase is a way to encode relationships between words below the level of a phrase(atom), hence "sub." A subphrase can be a single word, or it can be a collection of words. A word can be in multiple subphrases, but can not be in more than 3 (due to the limitations of the data creation program, [parsephrases](http://www.etcbc.nl/datacreation/#ps3.p)).
#
# ## Method
# The types of phrases represented in the ETCBC include `NP` (noun phrase), `VP` (verb phrase), `PrNP` (proper noun phrase), `PP` (prepositional phrase), `AdvP` (adverbial phrase), and [eight others](https://etcbc.github.io/bhsa/features/hebrew/c/typ). For some of these types, isolating the head word is a simple affair. By coordinating a word's phrase-dependent part of speech with its enclosing phrase's type, one can identify the head word. For a `VP`, that would mean simply finding the word within the phrase that has a `pdp` (phrase dependent part of speech) value of `verb`. Or for a prepositional phrase, find the word with a `pdp` of `prep`.
#
# The `NP` and `PrNP`, on the other hand, present special challenges. These phrases often contain multiple words with a modifying relation to the head noun. An example of this is the construct relation (e.g. "Son of Jacob"). The problem becomes particularly thorny when relations like the construct are chained together so that one is faced with the choice between multiple potential head nouns.
#
# To navigate the problem, we must use the feature [rela (relationship)](https://etcbc.github.io/bhsa/features/hebrew/c/rela) stored on `subphrase`s in addition to the `pdp` and phrase `type` features. In order to isolate the head word of a `NP`, we look for a word within the phrase that has a `pdp` value of `subs` (i.e. noun). We then obtain a list of all the `subphrase`s which contain that word using the [L.u Text-Fabric method](https://annotation.github.io/text-fabric/tf/core/locality.html). We then use the list of subphrase node numbers to create a list of all subphrase relations containing the word. If the list contains *any* dependent relations, then the word is automatically excluded from being a head word and we can move on to the next candidate. One final check is required for candidate words at the level of the `phrase`: the same procedure described above for `subphrase`s must be performed for `phrase_atom` relations. This means excluding words within a `phrase_atom` with a dependent relation to another `phrase_atom` within the `phrase`. If the head of a *`phrase_atom`* is being calculated, this step is not necessary.
#
# There are only two possible `subphrase` or `phrase_atom` relations for a valid head word: `NA` or `par`/`Para` (the verb is an exception, which in a handful of cases does have a construct relation). `NA` means that no relation is reflected. The word is independent. The `par` (`subphrase`) and `Para` (`phrase_atom`) stands for parallel relations, i.e. coordinates. While coordinates are not formally the head, they are often an important part of how the grammatical and semantic relations are built. Thus we provide coordinates alongside the head noun. These words require one further test, that is, it must be verified that their mother (using the [edge feature](https://annotation.github.io/text-fabric/tf/core/edgefeature.html) "[mother](https://etcbc.github.io/bhsa/features/hebrew/c/mother.html)") is itself a head word. To do this step thus requires us to keep track of those words within the phrase which have been validated. We can do so with a simple list.
#
# ## Results
# The function `get_heads` produces head word nodes on supplied phrase(atom) nodes. The results have been manually inspected for consistency.
#
# For phrase types other than the noun phrase, the results are very accurate. Some phrase types, like the conjunction phrase, do have unexpected forms. For instance, the phrase בעבור is coded as a conjunction phrase in the BHSA; in it, there is actually no word with a part of speech of "conjunction". These kinds of cases are easily accounted for by making exceptions in the set of acceptable parts of speech.
#
# For noun phrases, the situation is different. In the majority of cases, the results are good. But there are a handful of cases that simply cannot be addressed using the current ETCBC data model without a solution that exceeds the bounds of this current project. The reason is that the current model does not transparently encode hierarchy between phrases and embeded phrases. For instance, both phrase atoms and subphrases have *some* overlapping features. But what is the relationship of a phrase atom to a subphrase? Or, what is the relation of one subphrase to another? These are only coded implicitly in the data. In reality, there are "subphrases" embedded within the ETCBC's subphrases which are not even registered in the BHSA data. While phrase atoms receive type codes, subphrases do not. Yet, subphrases are "phrases" too, which should also have type codes. Another problem is that the precise level of embedding for the subphrases are not provided. Subphrases are presented as equal constituents, even though some subphrases are contained within others. These kinds of problems make a simple method, such as applied here, inadequate. But more importantly, they highlight the shortcomings of the ETCBC data model.
#
# The members of the ETCBC are aware of the inadequacy of that data model to represent complex phrases, and a change is in the pipeline to address it. However, it remains to be seen how long those changes might take. For now, the functions produced and modified in this NB will sufice to provide a temporary solution for those who require head words from BHSA phrases.

# %% [markdown]
# ## Code Development
#
# Below we experiment with the code and develop the functions that will extract the head nouns. This involves a good deal of manual inspections of the results before exporting the Text-Fabric features.
#
# The code is written immediately below. Associated questions that arise while writing or evaluating the code are contained in the subsequent section.

# %%
import collections, os, sys, random
from pprint import pprint
from tf.fabric import Fabric
from tf.extra.bhsa import Bhsa

# load Text-Fabric and data
data_loc = ["~/github/etcbc/bhsa/tf/c"]
TF = Fabric(locations=data_loc, silent=True)
api = TF.load(
    """
              book chapter verse
              typ pdp rela mother 
              function lex sp ls
              """,
    silent=True,
)

F, E, T, L = api.F, api.E, api.T, api.L  # TF data methods
B = Bhsa(api, name="getting_heads", version="c")  # BHSA visualizer


# %%
def get_heads(phrase):
    """
    Extracts and returns the heads of a supplied
    phrase or phrase atom based on that phrase's type
    and the relations reflected within the phrase.

    --input--
    phrase(atom) node number

    --output--
    tuple of head word node(s)
    """

    # mapping from phrase type to good part of speech values for heads
    head_pdps = {
        "VP": {"verb"},  # verb
        "NP": {"subs", "adjv", "nmpr"},  # noun
        "PrNP": {"nmpr", "subs"},  # proper-noun
        "AdvP": {"advb", "nmpr", "subs"},  # adverbial
        "PP": {"prep"},  # prepositional
        "CP": {"conj", "prep"},  # conjunctive
        "PPrP": {"prps"},  # personal pronoun
        "DPrP": {"prde"},  # demonstrative pronoun
        "IPrP": {"prin"},  # interrogative pronoun
        "InjP": {"intj"},  # interjectional
        "NegP": {"nega"},  # negative
        "InrP": {"inrg"},  # interrogative
        "AdjP": {"adjv"},  # adjective
    }

    # get phrase-head's part of speech value and list of candidate matches
    phrase_type = F.typ.v(phrase)
    head_candidates = [
        w for w in L.d(phrase, "word") if F.pdp.v(w) in head_pdps[phrase_type]
    ]

    # VP with verbs require no further processing, return the head verb
    if phrase_type == "VP":
        return tuple(head_candidates)

    # go head-hunting!
    heads = []

    for word in head_candidates:

        # gather the word's subphrase (+ phrase_atom if otype is phrase) relations
        word_phrases = list(L.u(word, "subphrase"))
        word_phrases += (
            list(L.u(word, "phrase_atom"))
            if (F.otype.v(phrase) == "phrase")
            else list()
        )
        word_relas = set(F.rela.v(phr) for phr in word_phrases) or {"NA"}

        # check (sub)phrase relations for independency
        if word_relas - {"NA", "par", "Para"}:
            continue

        # check parallel relations for independency
        elif word_relas & {"par", "Para"} and mother_is_head(word_phrases, heads):
            this_head = find_quantified(word) or find_attributed(word) or word
            heads.append(this_head)

        # save all others as heads, check for quantifiers first
        elif word_relas == {"NA"}:
            this_head = find_quantified(word) or find_attributed(word) or word
            heads.append(this_head)

    return tuple(sorted(set(heads)))


def mother_is_head(word_phrases, previous_heads):

    """
    Test and validate parallel relationships for independency.
    Must gather the mother for each relation and check whether
    the mother contains a head word.

    --input--
    * list of phrase nodes for a given word (includes subphrases)
    * list of previously approved heads

    --output--
    boolean
    """

    # get word's enclosing phrases that are parallel
    parallel_phrases = [ph for ph in word_phrases if F.rela.v(ph) in {"par", "Para"}]
    # get the mother for the parallel phrases
    parallel_mothers = [E.mother.f(ph)[0] for ph in parallel_phrases]
    # get mothers' words, by mother
    parallel_mom_words = [set(L.d(mom, "word")) for mom in parallel_mothers]
    # test for head in each mother
    test_mothers = [
        bool(phrs_words & set(previous_heads)) for phrs_words in parallel_mom_words
    ]

    return all(test_mothers)


def find_quantified(word):

    """
    Check whether a head candidate is a quantifier (e.g. כל).
    If it is, find the quantified noun if there is one.
    Quantifiers are connected with the modified noun
    either by a subphrase relation of "rec" for nomen
    regens. In this case, the quantifier word node is the
    mother itself. In other cases, the noun is related to the
    number via the `atr` (attributive) subphrase relation. In this
    case, the edge relation is connected from the substantive
    to the number's subphrase.

    --input--
    word node

    --output--
    new word node or None
    """

    custom_quants = {
        "KL/",
        "M<V/",
        "JTR/",  # quantifier lexemes, others?
        "M<FR/",
        "XYJ/",
    }
    good_pdps = {
        "subs",  # substantive
        "nmpr",  # proper noun
        "prde",  # demonstrative
        "prps",  # pronoun
        "verb",
    }  # "verb" for participles, see the inquiries below.

    if F.lex.v(word) not in custom_quants and F.ls.v(word) not in {"card", "ordn"}:
        return None

    # first check rec relations for valid quantified noun:
    rectum = next(
        (sp for sp in E.mother.t(word) if F.rela.v(sp) == "rec"), 0
    )  # extract the rectum
    noun = next(
        (w for w in L.d(rectum, "word") if F.pdp.v(w) in good_pdps), 0
    )  # filter words for noun
    num_check = F.ls.v(L.u(noun, "lex")[0]) if noun else ""
    if noun and num_check not in {"card", "ordn"}:
        return noun

    # check the adjunct relation if no rec found:
    subphrases = sorted(L.u(word, "subphrase"))
    # move progressively from smallest to largest subphrase, stop when non-cardinal noun is found
    for sp in subphrases:
        candidates = sorted(
            daughter for daughter in E.mother.t(sp) if F.rela.v(daughter) == "adj"
        )
        for candi in candidates:
            noun = next((w for w in L.d(candi, "word") if F.pdp.v(w) in good_pdps), 0)
            num_check = F.ls.v(L.u(noun, "lex")[0]) if noun else ""
            if noun and num_check not in {"card", "ordn"}:
                return noun

    # all else are non-quantifiers
    return None


def find_attributed(word):

    """
    Check whether the head candidate is an adjective.
    If it is, retrieve its attributed noun via the
    regens (rec) relationship.

    This function is similar to the quantified function.

    --input--
    word node

    --output--
    new word node or None
    """

    if F.pdp.v(word) != "adjv":
        return None

    # check rec relations for valid attributed noun:
    rectum = next(
        (sp for sp in E.mother.t(word) if F.rela.v(sp) == "rec"), 0
    )  # extract the rectum
    noun = next(
        (w for w in L.d(rectum, "word") if F.pdp.v(w) == "subs"), 0
    )  # filter words for noun
    if noun:
        return noun

    # sanity check: adjectives should not
    # pass through this algorithm without a noun assignment
    if F.typ.v(L.u(word, "phrase")) == "NP":
        raise Exception(
            f'adjective head assignment on NP {L.u(word, "phrase")} at word {word}'
        )
    else:
        return None

# %% [markdown]
# ## Tests
#
# Testing the get heads function and measuring the results.
#
# ### Test Functions


# %%
def show_subphrases(phrase_node):
    """
    Inspect subphrases and their relations to each other.
    """
    print("subphrase", "\ttext", "\trelation", "\tmother")
    for sp in L.d(phrase_node, "subphrase"):
        print(T.text(L.d(sp, "word")), sp, F.rela.v(sp), E.mother.f(sp))


# %% [markdown]
# ### Simple Test
# Apply the function to all the phrases in the HB. Record those cases which fail to receive a head assignment.

# %%
headless = []
total = 0

for phrase in F.otype.s("phrase"):

    total += 1

    heads = get_heads(phrase)

    if not heads:
        headless.append((phrase,))

len(headless)

# %% [markdown]
# ### Quality Tests
#
# What do the results look like? Start with noun-phrases, but retrieve interesting examples with more than a few words.

# %%
examples = []

for phrase in F.typ.s("NP"):

    len_words = len(L.d(phrase, "word"))

    if len_words > 5:

        heads = get_heads(phrase)

        examples.append(heads)

len(examples)

# %%
random.shuffle(examples)  # get samples at random

# %%
B.show(examples[:20], condenseType="phrase")  # uncomment me

# %% [markdown]
# ## Data Discovery
#
# The queries which follow were written at different times during the code construction for the heads algorithm.
#
# In this section, important questions were asked whose answers are needed to ensure the code is written correctly. The BHSA data is queried to answer them. These are questions like, "Do we need to check for relational independency for only noun phrases?" (no); and "does every phrase type have a word with a corresponding pdp?" (no).
#
# ### Make definitions available for exploration:

# %%
# mapping from phrase type to its head part of speech
type_to_pdp = {
    "VP": "verb",  # verb
    "NP": "subs",  # noun
    "PrNP": "nmpr",  # proper-noun
    "AdvP": "advb",  # adverbial
    "PP": "prep",  # prepositional
    "CP": "conj",  # conjunctive
    "PPrP": "prps",  # personal pronoun
    "DPrP": "prde",  # demonstrative pronoun
    "IPrP": "prin",  # interrogative pronoun
    "InjP": "intj",  # interjectional
    "NegP": "nega",  # negative
    "InrP": "inrg",  # interrogative
    "AdjP": "adjv",
}  # adjective


# %% [markdown]
# ### Test for non-NP phrases with valid pdp but invalid head
#
# These tests demonstrate that subphrase relation checks are also needed for phrase types besides noun phrases. The only valid subphrase/phrase_atom relations for any potential head word is either `NA` or `par`/`Para`. While a few phrase types do not need additional relational checks, e.g. personal pronoun phrases, we can go ahead and consistently handle all phrases in the same way.
#
# The only exception to the above rule is the `VP`, for which there are 14 cases of the `VP`'s head word (verb) that is also in a subphrase with a regens (`rec`) relation.
#
# The operational question of these tests was:
# > Are there cases in which a non-NP phrase(atom) contains a word with the corresponding pdp value, but which is probably not a head?
#
# To answer the question, we first survey all cases where the phrase type's head candidate is in a subphrase with a relation that is not normally "independent." Based on the survey, we manually check the most pertinent phrase types and results. The tests reveal that, indeed, relation checks are needed for many phrase types.


# %%
def test_pdp_safe(phrase_object="phrase_atom"):

    """
    Make a survey of phrase types and their matching `pdp` words,
    count what kinds of subphrase relations these words
    occur in. The survey can then be used to investigate
    whether phrase types besides noun phrases require relationship
    checks for independency.
    """

    pdp_relas_survey = collections.defaultdict(lambda: collections.Counter())
    headless = 0

    for phrase in F.otype.s(phrase_object):

        typ = F.typ.v(phrase)  # phrase type

        head_pdp = type_to_pdp[typ]

        maybe_heads = [w for w in L.d(phrase, "word") if F.pdp.v(w) == head_pdp]

        # this check shows that many
        # phrases don't have a word
        # with a corresponding pdp!
        if not maybe_heads:
            headless += 1

        # survey the candidate heads' relations
        for word in maybe_heads:

            head_name = typ + "|" + head_pdp
            subphrases = L.u(word, "subphrase")
            sp_relas = (
                set(F.rela.v(sp) for sp in subphrases) if subphrases else {"NA"}
            )  # <- handle cases without any subphrases (i.e. verbs)

            pdp_relas_survey[head_name].update(sp_relas)

    print(f"{phrase_object}s without matching pdp: {headless}\n")
    print("subphrase relation survey: ")
    for name, rela_counts in pdp_relas_survey.items():

        print(name)

        for r, count in rela_counts.items():
            print("\t", r, "-", count)


# %%
# for phrase_atoms
test_pdp_safe()

# %%
# and for phrases
test_pdp_safe(phrase_object="phrase")


# %% [markdown]
# ^ These surveys tell us that for several of these phrase types, e.g. `InjP`, we can automatically take the word with the pdp value that corresponds with its phrase type as the head.
#
# There are also quite a few cases where the phrase type does not have a word with a matching pdp value: 837 for phrase atoms and 670 for phrases. In the subsequent section we will run tests to find out why this is the case.
#
# Back to the question of this section: There are 14 examples of VP with verbs that have a `rec` (nomen regens) relation. Are these heads or not? We check now...


# %%
def find_and_show(search_pattern):
    results = sorted(B.search(search_pattern))
    print(len(results), "results")
    B.show(results, end=20, condenseType="phrase", withNodes=True)


# %%
# run notebook locally to see HTML-formatted results for the below searches


rec_verbs = """

phrase_atom typ=VP
    subphrase rela=rec
        word pdp=verb
"""

find_and_show(rec_verbs)

# %% [markdown]
# In all 14 results, the verb serves as the true head word of the `VP`.
#
# *Note: The verb will prove to be an exception, as all other words in a `rec` relation are not head words*
#
# The `PP` also has some strange relations. We see what's going on with the same kind of inspection. First we look at the `rec` (regens) relations.

# %%
rec_preps = """

phrase_atom typ=PP
    subphrase rela=rec
        word pdp=prep
"""

find_and_show(rec_preps)

# %% [markdown]
# The PP is different. In cases where the `phrase_atom` = `rec`, the preposition is *not* the head. Thus, the algorithm will need to check for these cases.
#
# Now for the `adj` subphrase relation in `PP`:

# %%
adj_preps = """

phrase_atom typ=PP
    subphrase rela=adj
        word pdp=prep
"""

find_and_show(adj_preps)

# %% [markdown]
# The results above show that the `adj` subphrase relation is also a non-head. These cases have to be excluded.
#
# Now we move on to test the **adverb** relations reflected in the survey...

# %%
adv_adj = """

phrase_atom typ=AdvP
    subphrase rela=adj
        word pdp=advb

"""

find_and_show(adv_adj)

# %% [markdown]
# The `adj` relationships in the adverbial phrase is also not a true head. Now for the `mod` (modifier) relation.

# %%
adv_mod = """

phrase_atom typ=AdvP
    subphrase rela=mod
        word pdp=advb

"""

find_and_show(adv_mod)

# %% [markdown]
# In this case, it appears that `mod` is also an invalid relation for adverb phrases. And example is גם הלם ('also here') where גם is the adverb in `mod` relation, but the head is really הלם "here" (also an adverb). In several cases, the modifier modifies a verb. In these cases the "head," often a participle or infinitive, acts as the adverb, even though it is not explicitly marked as such.
#
# Now we move on to the last examination, that of the `AdjP` (adjective phrase). There are three relations of interest:
# > atr - 6 <br>
# > adj - 3 <br>
# > rec - 1 <br>

# %%
adj_atr = """

phrase_atom typ=AdjP
    subphrase rela=atr
        word pdp=adjv

"""

find_and_show(adj_atr)

# %%
adj_adj = """

phrase_atom typ=AdjP
    subphrase rela=adj
        word pdp=adjv

"""

find_and_show(adj_adj)

# %%
adj_rec = """

phrase_atom typ=AdjP
    subphrase rela=rec
        word pdp=adjv

"""

find_and_show(adj_rec)

# %% [markdown]
# The results for the three searches above show indeed that the relations of `atr`, `adj`, and `rec` are not head words.

# %% [markdown]
# ### Tests for phrase types without a word that has a valid pdp value
#
# The initial survey above revealed that 837 phrase atoms and 670 phrases lack a word with a corresponding pdp value. Here we investigate to see why that is the case. Is there a way to compensate for this problem? Are these truly phrases that lack heads?
#
# We run another survey and count the phrase types against the non-matching pdp values found within them. At this point, we must also exclude words that have dependent relations (as defined above, subphrase values of NA or parallel).

# %%
count_no_pdp = collections.defaultdict(lambda: collections.Counter())
record_no_pdp = collections.defaultdict(lambda: collections.defaultdict(list))

for phrase in F.otype.s("phrase_atom"):

    typ = F.typ.v(phrase)

    # see if there is not corresponding pdp value
    corres_pdp = type_to_pdp[typ]
    corresponding_pdps = [w for w in L.d(phrase, "word") if F.pdp.v(w) == corres_pdp]

    if not corresponding_pdps:

        # put potential heads here
        maybe_heads = []

        # calculate subphrase relations
        for word in L.d(phrase, "word"):

            # get subphrase relations
            word_subphrs = L.u(word, "subphrase")
            sp_relas = set(F.rela.v(sp) for sp in word_subphrs) or {"NA"}

            # check subphrase relations for independence
            if sp_relas == {"NA"}:
                maybe_heads.append(word)

            # test parallel relation for independence
            elif sp_relas == {"NA", "par"} or sp_relas == {"par"}:

                # check for good, head mothers
                good_mothers = set(
                    sp for w in maybe_heads for sp in L.u(w, "subphrase")
                )
                this_daughter = [sp for sp in word_subphrs if F.rela.v(sp) == "par"][0]
                this_mother = E.mother.f(this_daughter)

                if this_mother in good_mothers:
                    maybe_heads.append(word)

        # sanity check
        # maybe_heads should have SOMETHING
        if not maybe_heads:
            raise Exception(f"phrase {phrase} looks HEADLESS!")

        # count pdp types
        head_pdps = [F.pdp.v(w) for w in maybe_heads]
        count_no_pdp[typ].update(head_pdps)

        # save for examination
        for word in maybe_heads:
            record_no_pdp[typ][F.pdp.v(word)].append((phrase, word))

for name, counts in count_no_pdp.items():

    print(name)

    for pdp, count in counts.items():
        print("\t", pdp, "-", count)

# %% [markdown]
# These results are a bit puzzling. The numbers here are words within the phrase atoms that have NO subphrase relations. That means, for example, words such as הַ "the" do not appear to have any subphrase relation to their modified nouns. That again illustrates the shortcoming of the ETCBC data in this respect. There should be a relation from the article to the determined noun.
#
# From this point forward, I will begin working through all four phrase types and the cases reflected in the survey.
#
# Beginning with the `AdvP` type and the article. Upon some initial inspection, I've found that in many of the `AdvP` with the article, there is also a substantive (`subs`) that was found by the search. Are there any cases where there is no `nmpr` or `subs` found alongside the article? We can use the dict `record_no_pdp` which has recorded all cases reflected in the survey. Below I look to see if all 190 cases of an article in these `AdvP` phrases also has a corresponding noun.

# %%
no_noun = []

for phrase in record_no_pdp["AdvP"]["art"]:

    pdps = set(F.pdp.v(w) for w in L.d(phrase[0], "word"))

    if not {"nmpr", "subs"} & pdps:
        no_noun.append((phrase,))

print(len(no_noun), "without nouns found...")

# %% [markdown]
# There it is. So all cases of these articles can be discarded. In these cases, the noun serves as the head of the adverbial phrase. An example of this is when the noun marks the location of the action (hence adverb).
#
# Next, we check the conjunctions found in the adverbial phrases. Are any of those heads?

# %%
# B.show(record_no_pdp['AdvP']['conj']) # uncomment me!

# %% [markdown]
# All conjunctions in these `AdvP` phrases function to mark coordinate elements (only ו in these results). They can also be discarded as not possible heads.
#
# Now we investigate the `PrNP` results with `subs` and `art`...

# %%
# B.show(record_no_pdp['PrNP']['subs']) # uncomment me!

# %%
# B.show(record_no_pdp['PrNP']['art']) # uncomment me!

# %% [markdown]
# The `art` relations reflected in the second search are not heads, but are all related to a substantive. All of the results in `subs` are heads. Thus, the only acceptable pdp for `PrNP` besides a proper noun is `subs`.
#
# Now we dig into `CP` results. 85 of them have no `pdp` of conjunction, but have a preposition instead. Let's see what's going on...

# %%
# B.show(record_no_pdp['CP']['prep'][:20]) # uncomment me!

# %% [markdown]
# These are very interesting results. These conjunction phrases are made up of constructions like ב+עבור and ב+טרם. Together these words function as a conjunction, but alone they are prepositions and particles. Is it even possible in this case to say that there is a "head"?
#
# It could be said that these combinations of words mean more than the sum of their parts; they are good examples of constructions, i.e. combinations of words whose meaning cannot be inferred simply from their individual words. Constructions illustrate the vague boundary between syntax and lexicon (cf. e.g. Goldberg, 1995, *Constructions*).
#
# While these words are indeed marked as conjunction phrases, it is better in this case to analyze them as prepositional phrases (which they also are...this is another shortcoming of our data, or perhaps a mistake??). Thus, the head is the preposition, not the prepositional object.
#
# We should expect that the remaining `subs` and `advb` groups are in fact the objects of those prepositions (and hence excluded). Let's test that assumption by looking for a preposition behind these words...

# %%
no_prep = []

for (phrase, word) in record_no_pdp["CP"]["subs"] + record_no_pdp["CP"]["advb"]:

    possible_prep = word - 1

    if F.sp.v(possible_prep) != "prep":
        no_prep.append((phrase, word))

print(f"subs|advb with no preceding prepositions: {len(no_prep)}")

# %% [markdown]
# Here we see. We can confirm that none of the substantives or adverbs will be the head of a conjunction phrase. A preposition is the only other kind of head for the `CP` besides a conjunction itself.
#
# Finally, we're left with a last noun phrase (`NP`) for which no matching noun was found. The search found instead both `adjv` (adjective) and a `intj` (interjection). Let's see it.

# %%
# B.show(record_no_pdp['NP']['intj']) # uncomment me

# %% [markdown]
# In this case, the word אוי "woe" functions like a noun. This thus appears to be another mislabeled `pdp` value, since it should read `subs`. This, like the previous example, will not receive a head value due to the mistake.

# %% [markdown]
# ### Retrieving Quantified Words
#
# When the heads algorithm looks for a noun without any subphrase relations in the phrase, it will often return a quantifier noun such as a number, e.g. שבעה "seven", or such as another descriptor like כל. But these words function semantically in a more descriptive role than a head role. Thus, we want our algorithm to isolate quantified nouns from their quantifiers. To do that means we must first know how the ETCBC encodes the relationship between a quantifier and the quantified noun.
#
# In a previous algorithm used for quantified extraction, we looked for a nomen regens relation on the quantifier and located the noun within the related subphrase. This approach works well for the quantifier כל. But for cardinal numbers, the relation `adj` (adjunct) is often used as well (as seen in the surveys below).
#
# To illustrate with the search below, the quantifier שבעה "seven" has no nomen regens relation:

# %%
# B.show([(2217,)]) # uncomment me!

# %% [markdown]
# Rather than reflecting a regen/rectum relation, the second word שנים "years," the quantified noun, has a subphrase relation of `adj` "adjunct":

# %%
print(T.text(L.d(652883, "word")))
print()

for sp in L.u(2218, "subphrase"):  # subphrases belonging to "years"
    print(sp, "(subphrase)")
    print("\t", T.text(L.d(sp, "word")))
    print("\t", "rela:", F.rela.v(sp))
    print()

# %% [markdown]
# Let's see what other kinds of subphrase relations are reflected by quantifieds.
#
# Below we make a survey of all mother-daughter relations between a quantifier subphrase and its daughters. The goal is to isolate those relationships which contain the quantified noun. We work through examples to get an idea of the meaning of the features. And we write a few TF search queries further below to confirm hypotheses about these relationships.

# %%
quant_relas = collections.defaultdict(lambda: collections.Counter())
quant_ex = collections.defaultdict(lambda: collections.defaultdict(list))
quants = [word for lex in F.ls.s("card") for word in L.d(lex, "word")]

for word in quants:

    subphrases = L.u(word, "subphrase")
    sp_daughters = [E.mother.t(sp) for sp in subphrases if E.mother.t(sp)]
    sp_daughters += [E.mother.t(word)] if E.mother.t(word) else list()
    sp_relas = [F.rela.v(sp[0]) for sp in sp_daughters]
    quant_relas[F.lex.v(word)].update(sp_relas)

    for rela in sp_relas:
        quant_ex[F.lex.v(word)][rela].append((L.u(word, "phrase")[0], word))

for name, counts in quant_relas.items():

    print(name)

    for pdp, count in counts.items():
        print("\t", pdp, "-", count)

# %% [markdown]
# Based on the inspection below, it can be seen that quantified nouns are connected to their quantifier via a subphrase relation of either `adj` (adjunctive) or `rec` (regens), as mentioned at the beginning of this inquiry.

# %%
# B.show(quant_ex['CB</']['rec']) # uncomment me!

# %% [markdown]
# The query below shows that the relation `par` most frequently refers to a parallel number, e.g. שבעים ושבעה "seventy and seven" where "and seven" is in a parallel relationship.

# %%
# B.show(quant_ex['CB</']['par']) # uncomment me!

# %% [markdown]
# The `atr` relation appears when an adjective is used to describe a quantifier:

# %%
# B.show(quant_ex['>XD/']['atr']) # uncomment me!

# %% [markdown]
# The `Spec` (phrase_atom rela) are cases where a phrase atom is used to add adjectival information about the quantifier.

# %%
# B.show(quant_ex['>XD/']['Spec']) # uncomment me!

# %% [markdown]
# The `mod` relation are cases where the quantifier is modified with particles like גם or רק

# %%
# B.show(quant_ex['CNJM/']['mod']) # uncomment me!

# %% [markdown]
# The `dem` relation is when a demonstrative like אלה modifies the quantifier.

# %%
# B.show(quant_ex['CB</']['dem']) # uncomment me!

# %% [markdown]
# Based on the analysis up to this point, there are two kinds of relations which lead back to the quantified noun: the `rec` (regens) and `adj` (adjunct) relations. What about in cases where both of these relations are present? Is there ever a case where it is ambiguous which relation contains the quantified noun?
#
# We use a TF search pattern to build a query for these cases. We look for cases that have three subphrases. The first has a word (`w1`) which is also contained in a lex object (second to bottom block) that has a `ls` (lexical set) value of `card` (cardinal number). Then we look for two other subphrases that have a relation either to the first subphrase (in the case of the `adj` rela) or the quantifier word contained in the first subphrase (in the case of a regens relation). Within `sp2` and `sp3`, we also select the first word so we can highlight it in the `B.show` below.

# %%
quant_rec_adj = """

sp1:subphrase
    w1:word

sp2:subphrase rela=rec
    =: word

sp3:subphrase rela=adj
    =: word

lex ls=card
   w2:word
   
w1 = w2
w1 <mother- sp2
sp1 <mother- sp3

sp2 <: sp3
"""

quant_rec_adj = B.search(quant_rec_adj)

len(quant_rec_adj)

# %%
# B.show(sorted(quant_rec_adj)) # uncomment me!

# %% [markdown]
# There are 245 cases with both relations. Based on inspection, it seems that the word in the `rec` relation is usually another quantifier. Are there cases where it is not?
#
# We apply a filter with a list comprehension to the results below to filter out cases where there is a cardinal number in `sp2`.

# %%
non_card = [r for r in quant_rec_adj if F.ls.v(L.u(r[3], "lex")[0]) != "card"]

len(non_card)

# %%
# B.show(non_card) # uncomment me!

# %% [markdown]
# The example below illustrates a complexity here. We iterate through every subphrase in one of the phrases from the result above. We print the subphrase number, the text, the relation, and the number of the subphrase mother...

# %%
show_subphrases(686936)

# %% [markdown]
# In the example of שְׁתֵּֽי־צִפֳּרִ֥ים "two [of] birds" there are is a `rec` relation between two and birds. Then there is an adjunct relation, `adj`, further describing the whole subphrase "two birds": חַיֹּ֖ות טְהֹרֹ֑ות "pure beasts". In this case, it is the `rec` relation which is the valid head, while "pure beasts" is a secondary description. This example illustrates that there should be a priority for the `rec` relationship.

# %%
show_subphrases(682231)

# %% [markdown]
# This other example shows the same inner-structure, as do the other 3 that we've manually inspected. This confirms indeed that priority should be given when a noun is found in the `rec` relation. Afterwards, the `adj` relation is checked.
#
#
# Finally, we want to test that there always is a quantified noun in the `adj` relation. Are there other cases, based on the findings above, where the `adj` relation does not actually contain the quantified nouns? We create a looser query than the one above to cover all cases of the `adj` relation. Then we filter the results...

# %%
no_quants_adj = """

sp1:subphrase
    w1:word

sp2:subphrase rela=adj
    =: word

lex ls=card
   w2:word
   
w1 = w2
sp1 <mother- sp2
"""

no_quants_adj = sorted(B.search(no_quants_adj))
no_quants_adj = [r for r in no_quants_adj if F.pdp.v(r[3]) not in {"subs", "nmpr"}]

len(no_quants_adj)

# %%
# B.show(no_quants_adj[:10]) # uncomment me

# %% [markdown]
# Many of the cases are due to the presence of an article or a determiner.
#
# In one case the noun that is present is a demonstrative pronoun (`prde`) אלה for which there is no further specification. For now we exclude determiners and demonstratives and consider their role afterwards.

# %%
no_quants_adj = [r for r in no_quants_adj if F.pdp.v(r[3]) not in {"art", "prde"}]

len(no_quants_adj)

# %%
# B.show(sorted(no_quants_adj)) # uncomment me

# %% [markdown]
# An interesting result occurrs in Micah 5:4 where the word in the adjunct position has a pdp of "adjv" (adjective) when it should be subs. This is a mistake in the data. In this case, the participle should be nominalized as a `subs`:

# %%
show_subphrases(829120)

# %%
# B.show([(829120,)])

# %% [markdown]
# This case will be excluded from our algorithm due to the mistake.
#
# In other cases, the first word in the `adj` related subphrase is used as an adjective in a construct relation:

# %%
show_subphrases(738097)

# %% [markdown]
# If we print the `pdp` values of the words within the related subphrase חלקי־אבנים, we will find the "subs":

# %%
[F.pdp.v(w) for w in L.d(1342696, "word")]

# %% [markdown]
# There are only a couple of these cases in the results. Thus, it will be safe for the algorithm to take the first `subs` pdp word that it comes across.
#
# Besides these cases, there are cases where there is no `subs` but a participle occurs as a `subs` with a pdp of `verb` due to the presence of satellite objects around the verb:

# %%
# B.show([(898716,)]) # uncomment me

# %% [markdown]
# For these cases, the algorithm should look for cases where there is no other pdp candidate and there is a verb that has a `vt` of participle.
#
# There are also several cases where the quantified noun is a personal pronoun, as exemplified below:

# %%
# B.show([(867246,)]) # uncomment me

# %% [markdown]
# These should be added to the set of acceptable quantified heads.
#
# Below we remove the cases accounted for thus far.

# %%
no_quants_adj = [
    r for r in no_quants_adj if F.pdp.v(r[3]) not in {"adjv", "verb", "prps"}
]

len(no_quants_adj)

# %%
# B.show(no_quants_adj) # uncomment me

# %% [markdown]
# What remains is 5 instances of quantified prepositional phrases. These are cases where the number is truly acting in a nominal capacity. In these cases, the algorithm should not return any quantified nouns since the quantifier itself semantically functions as the noun.

# %%
no_quants_adj = [r for r in no_quants_adj if F.pdp.v(r[3]) not in {"prep"}]

len(no_quants_adj)

# %% [markdown]
# That is all the cases in which there is not a traditional "subs" within the adjunct of a quantifier. The final set of acceptable `pdp` tags for a quantified noun in an `adj` related subphrase are as follows:

# %%
acceptable_adj_quantifieds = {
    "subs",  # noun
    "nmpr",  # proper noun
    "prde",  # demonsrative
    "prps",  # pronoun
    "verb",
}  # for participles

# %% [markdown]
# **The queries above raise the equivalent question for `rec` related, quantified subphrases:** Are there other kinds of acceptable quantified nouns in the `rec` relationship besides `subs` and `nmpr`? We make a query and test whether we also need a similar set to the one above.

# %%
rec_quants = """

sp1:subphrase
    w1:word

sp2:subphrase rela=rec
    =: word

lex ls=card
   w2:word
   
w1 = w2
w1 <mother- sp2
"""

rec_quants = sorted(B.search(rec_quants))

# apply filters:
rec_quants = [
    r
    for r in rec_quants
    if F.pdp.v(r[3]) not in {"subs", "nmpr"} and F.ls.v(L.u(r[3], "lex")[0]) != "card"
]

len(rec_quants)

# %%
# B.show(rec_quants) # uncomment me

# %% [markdown]
# Many of these appear to be cases where the article is in the first position followed by a `subs`. We exclude those below...

# %%
rec_quants = [r for r in rec_quants if F.pdp.v(r[3]) != "art"]

len(rec_quants)

# %%
# B.show(rec_quants) # uncomment me

# %% [markdown]
# There are a couple cases where the demonstrative occurs in the quantified position, exemplified below:

# %%
# B.show([(676421,)]) # uncomment me

# %% [markdown]
# The rest of the cases seem to be problematic examples of prepositions. They are problematic since they should be coded with a relation of `adj` rather than `rec`. In any case, the sets of acceptable solutions should not include the preposition, the same as with the `adj`.
#
# Based on this analysis, the `rec` quantified subphrases can utilize the same check-set as the `adj` quantifieds.
#
# #### Conclusion
# This analysis has found that quantifieds should be processed in the order of `rec` subphrase relations first. If an acceptable part of speech tag is not found within the `rec` subphrase, then the subsequent `adj` subphrase (adjunct) should be checked. In a handfull of cases, there will not be a quantified noun since the quantifier itself functions as a nominal element.
#
# For both the `rec` and `adj` related subphrases, the same `pdp` check set can be used to isolate viable heads.
#
# #### Appendix: Which Subphrase?
#
# In cases where the quantified noun is related by the subphrase rela of `adj`, to which subphrase of the quantifier will it relate? It is assumed that it would relate to the largest one...

# %% [markdown]
# A good solution would be to progressively move up from the smallest subphrase to the largest subphrase and check for relations on each one until it is found. That is what we follow in the algorithm.

# %% [markdown]
# ### Adjective -> Subs Missed Results
#
# In the first test, several substantives are missed due to the presence of an adjectival element. Let's look at those cases and see what's going on. I have copied the phrase numbers of a few relevant examples.

# %%
adj_examples = [(771933,), (799523,)]

# B.show(adj_examples) # uncomment me

# %%
show_subphrases(adj_examples[0][0])

# %%
for word in L.d(adj_examples[0][0], "word"):
    print(T.text([word]), F.pdp.v(word))

# %% [markdown]
# In this case, the substantive is not detected by the algorithm since it is in a dependent subphrase, a construct relation, with its modifying adjective. How to extract these nouns?
#
# This is very similar to the quantifier case, where the word in the rectum is actually the head (e.g. שתי שנה "two years" where "two" is registered as the head, but the substantive "years" is the semantic head). This kind of relationship is differentiated from non-heads by the fact that the adjective itself is independent. Thus, in cases where the adjective is independent and has a daughter rectum subphrase, the algorithm should retrive the attributed noun.
#
# **proposed solution**: Add `adjv` to the set of acceptable `pdp` for the `NP`. Any adjectives will be processed for dependency: most will fail that test. But for the dozens of cases where the adjective does not fail, the algorithm will apply a separate check for a `rec` related subphrase which contains the true head.
#
# ### Participle -> Head Missed Results
#
# Other phrases that end up headless are noun phrases that have a participle which serves as a the nominal element, but since it has satellites is coded as a "verb":

# %%
verb_examples = [(709010,), (711593,), (756104,)]

# B.show(verb_examples) # uncomment me

# %%
for phrase in verb_examples:
    show_subphrases(phrase[0])
    print()

# %% [markdown]
# There are mixed cases here due to the shortcomings of the current data model. In these cases, the participle is marked as a "verb" since it also has objects or descriptors. In the first example above, the noun גרה functions as the *object* of the verb. The head is מעלה. But the same logic does not hold for the second or third case. In the second case, ֹ פצוע־דכא gives an *attribute* or quality of שפכה. In the third case, מצק "poured", describes an attribute of נחשת "bronze." Thus the opposite of example 1 is true, that is, the head noun is the attributed noun in the construct relation.
#
# Since the specific role of the noun or the verb is not specified at this lower phrase level, is there even a way to differentiate these cases?
#
#

# %%
for phrase in verb_examples:
    print(phrase[0])
    for word in L.d(phrase[0], "word"):
        print(T.text([word]), F.pdp.v(word))
    print()

# %% [markdown]
# It actually appears that the database treats all 3 the same: as adjectives at the phrase-dependent part of speech level. Thus, these cases will receive the same treatments as the adjective cases above.

# %% [markdown]
# ### KL/ relation problems
#
# I found an instance in Number 3:15 where the subphrase relationship that connects כל with its quantified noun is "atr." That is probably wrong. Are there other cases with the same problem?

# %%
kl_prob = """

sp1:subphrase
    w1:word lex=KL/ st=a

sp2:subphrase rela=atr

sp2 -mother> sp1
sp2 >> sp1
w1 :: sp1
"""

kl_prob = sorted(B.search(kl_prob))

len(kl_prob)

# %%
# B.show(kl_prob) # uncomment me

# %% [markdown]
# It seems that the adjectives are not nominalized in this construction as pdp of `subs`. Most of the findings are adjectives in construct with כל. But there are several cases of the participle also.
#
# Is this encoding correct?
#
# If the `rela` code were properly `rec` as most are, then this would simply be a matter of adding an additional acceptable `pdp` to the list within the get_quantified function.

# %%
# kl_prob = [r for r in kl_prob if not {'adjv'} and set(F.pdp.v(w) for w in L.d(r[2]))]

len(kl_prob)

# %%
kl_prob = set(r[0] for r in kl_prob)

len(kl_prob)

# %% [markdown]
# ### Subphrase by Subphrase Approach?
#
# Experimenting with switching from a word-by-word approach to a subphrase-by-subphrase. The first iteration of the get_heads function iterated word by word to identify valid heads with independent subphrase relations. A more efficient, and methodologically sound approach would be to work from the subphrase down to the word. Here I experiment with such a method.

# %%
test_phrases = [
    ph
    for ph in F.typ.s("NP")
    if len(L.d(ph, "word")) == 5 and F.otype.v(ph) == "phrase"
]

# %%
test = test_phrases[20]

test

# %%
show_subphrases(test)

# %%
head_cands = [sp for sp in L.d(test, "subphrase") if F.rela.v(sp) == "NA"]

head_cands

# %% [markdown]
# Note above that the heads are those within NA relations that consist of single words. How consistent is this? Are there any cases where the head does not receive its own individual subphrase with a NA relation? Or are there cases of NA relations of non-head elements? Below we run a couple of tests, and then we build a primitive head finder based on this hypothesis in order to manually inspect what happens.

# %%
for word in F.otype.s("word"):

    subphrases = L.u(word, "subphrase")

    if not subphrases:
        continue

    sp_relas = set(F.rela.v(sp) for sp in subphrases)

    if not {"NA", "par"} & sp_relas:
        print("example found: ")
        print(word, subphrases, sp_relas)
        break

print("search complete with 0 results")

# %%
T.text(L.d(1300568, "word"))

# %%
no_na = """

sp1:subphrase
    w1:word
sp2:subphrase

sp2 -mother> w1
"""

no_na = sorted(S.search(no_na))

len(no_na)

# %%
no_na_filtered = []

for r in no_na:

    reg = r[1]

    reg_subphrases = L.u(reg, "subphrase")
    reg_sp_relas = set(F.rela.v(sp) for sp in reg_subphrases)

    if "NA" not in reg_sp_relas:
        no_na_filtered.append(r)

print(f"words with construct relation and no NA subphrase: {len(no_na_filtered)}")

# %% [markdown]
# The search above shows that in any case that a word is in a construct relation with a subphrase, a NA (no relation) subphrase exists.
#
# Let's broaden the inquiry a bit. What are the specific situations in which there is NO non-related subphrase at all. What kinds of relations are present? What kinds of phrases are they?

# %%
na_survey = collections.Counter()

for phrase in F.otype.s("phrase"):

    subphrase_relas = tuple(
        sorted(set(F.rela.v(sp) for sp in L.d(phrase, "subphrase")))
    )

    if not subphrase_relas:
        na_survey["NO subphrases"] += 1

    elif "NA" in subphrase_relas:
        na_survey["has NA"] += 1

    else:
        na_survey[subphrase_relas] += 1

pprint(na_survey)

# %% [markdown]
# This count shows that there are only two situations in the data: either
#
# 1) a phrase has no subphrases present, or
#
# 2) it has a subphrase with a relation of "NA". There are NO cases of phrases that lack an NA subphrase but have other relations. That is good for our hypothesis...
#
# In the experiment below, two important assumptions are made about the head:
#
# **First**, it is assumed that **the head is the first valid `pdp` word in the phrase**, with the exception of quantifieds and attributed nouns which are handled differently.
#
# **Second**, it is assumed that the **first NA-relation subphrase contains the head**. We test that assumption by manually inspecting the output.


# %%
def primitive_head_hunter(phrase):

    """
    Looks at noun phrases for heads.
    """

    good_pdp = {"subs", "nmpr"}

    subphrase_candidates = [
        sp
        for sp in L.d(phrase, "subphrase")
        if F.rela.v(sp) == "NA" and F.rela.v(L.u(sp, "phrase_atom")[0]) == "NA"
    ]

    # handle simple phrases
    if not subphrase_candidates:
        head_candidates = [w for w in L.d(phrase, "word") if F.pdp.v(w) in good_pdp]
        try:
            return (head_candidates[0],)
        except:
            print(f"exception at {phrase}")

    # attempt simple head assignment
    first_na_subphrase = subphrase_candidates[0]
    try:
        the_head = next(
            w for w in L.d(first_na_subphrase, "word") if F.pdp.v(w) in good_pdp
        )
        return (the_head,)
    except:
        if F.pdp.v(L.d(first_na_subphrase, "word")[0]) == "adjv":
            pass
        else:
            raise Exception(phrase)


# %%
test_results = [primitive_head_hunter(ph) for ph in test_phrases]

random.shuffle(test_results)


# %%
# B.show(test_results) # uncomment me

# %% [markdown]
# As it turns out, the assumption about NA phrase type is workable. But the complications of this approach (explained below) make it an unlikely solution for now.

# %% [markdown]
# #### Conclusion
#
# I've done some initial testing with the subphrase by subphrase approach. It is a promising method, but requires a more complicated implementation with nested searches through each level of the phrase hierarchy. A simple subphrase by subphrase approach is not sufficient—one needs to go phrase by phrase, phrase_atom by phrase_atom, subphrase by subphrase, and even beyond. It is a recursive problem that cannot be navigated with the present, limited data model. There is more to say about the present state of the data model which I will save for the final report.
#
# At present, the word-by-word approach provides an elegant (though limited) solution that is able to navigate the quirks of the present data model and provide an acceptable level of accuracy, with some exceptions for more complicated phrase constructions.

# %% [markdown]
# ## Handling Parallels
#
# What is the best way to handle parallel head elements? In general, a phrase has only one real "head". That is, often the first head element determines the grammatical gender or number of the verb (thanks to Constantijn Sikkel for this conversation). Yet, the nouns which are coordinate to the head are often of interest for both grammatical and semantic studies.
#
# There are two approaches to collecting coordinate heads. One is to check for every word with a relation of "parallel" whether its mother is already established as a head. Another approach is to recursively search for nouns that are coordinate with the head word. Up until this inquiry, I have opted for option 1 due to the complexity of checking necessary relationships for a head candidate. But a phrase in Deuteromoy 12:17 is then missed by this current approach, since there is there a chain of head nouns in construct with the quantifier מעשר "tenth". These cases are missed.
#
# It is possible to edit the algorithm to accomodate these cases. But the example raises the broader question of whether option 1 is truly sufficient and methodologically sound. In this section, I test whether option 2 is a better alternative. First, we are unsure about how to separate a head word from a larger, paralleled subphrase. In option 1, individual words are tested, each for dependent relationships. But option 2 will go the opposite direction: beginning at the subphrase level and working down to the word. Does this affect our ability to separate the head noun of the phrase?

# %%
def OLD_get_heads(phrase):
    """
    Extracts and returns the heads of a supplied
    phrase or phrase atom based on that phrase's type
    and the relations reflected within the phrase.

    --input--
    phrase(atom) node number

    --output--
    tuple of head word node(s)
    """

    # mapping from phrase type to good part of speech values for heads
    head_pdps = {
        "VP": {"verb"},  # verb
        "NP": {"subs", "adjv", "nmpr"},  # noun
        "PrNP": {"nmpr", "subs"},  # proper-noun
        "AdvP": {"advb", "nmpr", "subs"},  # adverbial
        "PP": {"prep"},  # prepositional
        "CP": {"conj", "prep"},  # conjunctive
        "PPrP": {"prps"},  # personal pronoun
        "DPrP": {"prde"},  # demonstrative pronoun
        "IPrP": {"prin"},  # interrogative pronoun
        "InjP": {"intj"},  # interjectional
        "NegP": {"nega"},  # negative
        "InrP": {"inrg"},  # interrogative
        "AdjP": {"adjv"},  # adjective
    }

    # get phrase-head's part of speech value and list of candidate matches
    phrase_type = F.typ.v(phrase)
    head_candidates = [
        w for w in L.d(phrase, "word") if F.pdp.v(w) in head_pdps[phrase_type]
    ]

    # VP with verbs require no further processing, return the head verb
    if phrase_type == "VP":
        return tuple(head_candidates)

    # go head-hunting!
    heads = []

    for word in head_candidates:

        # gather the word's subphrase (+ phrase_atom if otype is phrase) relations
        word_phrases = list(L.u(word, "subphrase"))
        word_phrases += (
            list(L.u(word, "phrase_atom"))
            if (F.otype.v(phrase) == "phrase")
            else list()
        )
        word_relas = set(F.rela.v(phr) for phr in word_phrases) or {"NA"}

        # check (sub)phrase relations for independency
        if word_relas - {"NA", "par", "Para"}:
            continue

        # check parallel relations for independency
        elif word_relas & {"par", "Para"} and mother_is_head(word_phrases, heads):
            this_head = find_quantified(word) or find_attributed(word) or word
            heads.append(this_head)

        # save all others as heads, check for quantifiers first
        elif word_relas == {"NA"}:
            this_head = find_quantified(word) or find_attributed(word) or word
            heads.append(this_head)

    return tuple(sorted(set(heads)))


def mother_is_head(word_phrases, previous_heads):

    """
    Test and validate parallel relationships for independency.
    Must gather the mother for each relation and check whether
    the mother contains a head word.

    --input--
    * list of phrase nodes for a given word (includes subphrases)
    * list of previously approved heads

    --output--
    boolean
    """

    # get word's enclosing phrases that are parallel
    parallel_phrases = [ph for ph in word_phrases if F.rela.v(ph) in {"par", "Para"}]
    # get the mother for the parallel phrases
    parallel_mothers = [E.mother.f(ph)[0] for ph in parallel_phrases]
    # get mothers' words, by mother
    parallel_mom_words = [set(L.d(mom, "word")) for mom in parallel_mothers]
    # test for head in each mother
    test_mothers = [
        bool(phrs_words & set(previous_heads)) for phrs_words in parallel_mom_words
    ]

    return all(test_mothers)


# %% [markdown]
# ### How many subphrases with a parallel relation to a validated head consist of more than one word?
#
# We take the first head element for every noun phrase and check its parallel elements.

# %%
par_word_count = collections.Counter()
par_word_list = collections.defaultdict(list)

for np in F.typ.s("NP"):

    heads = OLD_get_heads(np)

    if not heads:
        continue

    the_head = heads[0]

    if not L.u(the_head, "subphrase"):
        continue

    head_smallest_sp = sorted(sp for sp in L.u(the_head, "subphrase"))[0]

    par_daughter = [d for d in E.mother.t(head_smallest_sp) if F.rela.v(d) == "par"]

    for pd in par_daughter:

        word_length = len(L.d(par_daughter[0], "word"))

        par_word_count[word_length] += 1
        par_word_list[word_length].append((the_head, head_smallest_sp))

for w_count, count in par_word_count.items():
    print("length:", w_count)
    print("\t", count)

# %% [markdown]
# Let's see some of the larger cases...

# %%
B.show(par_word_list[6], condenseType="phrase", withNodes=True)

# %%
ex_subphrase = par_word_list[6][1][1]

for daughter in [d for d in E.mother.t(ex_subphrase) if F.rela.v(d) == "par"]:

    print(daughter, F.rela.v(daughter), T.text(L.d(daughter, "word")))

# %% [markdown]
# And now some examples of 2 word lengths...

# %%
B.show(par_word_list[2][:5], condenseType="phrase", withNodes=True)

# %% [markdown]
# These examples raise an important possibility. If we take the first word labeled "subs" (substantive) within the parallel, will that give us the coordinate head?
#
# Below is an example from Genesis 5:3 which shows a potential pitfall of the method 2 approach, and even of the current approach.

# %%
example = L.d(T.nodeFromSection(("Genesis", 5, 3)), "phrase")[3]

print(example, T.text(L.d(example, "word")))
show_subphrases(example)

# %% [markdown]
# The example above illustrates the shortcoming of even the current method of separating quantifiers and quantifieds, as seen in this result:

# %%
"|".join(T.text([h]) for h in get_heads(example))

# %% [markdown]
# The algorithm retrieves both "thirty" and "year," even though the only head in this case is "year". This is a shortcoming of the quantifier function, which in this case has not detected a complex quantifier that is formed with a parallel relation.
#
# The quantifier algorithm should have passed שלשים along to another test before validating it as a head. That is, it should look for this case of a complex quantifier. This is actually another good reason to change the parallels finder to approach 2, so that parallels are processed at the head level rather than disconnected from it. In this setup, the algorithm will gather all parallels to the head. If the syntactic head is a quantifier. If it has no quantified noun, then the algorithm will look further at the parallel relationship to see if it is also a quantifier. If it is, then it will look to find that quantifier's substantive and return it instead. This is a complex recursive process that will have to be coded.

# %%
# B.show(par_word_list[3][:15]) # uncomment me

# %% [markdown]
# ### Are there cases where there is multiple coordinate relations with a single subphrase?

# %%
test_multiple_coor = """

sp1:subphrase
sp2:subphrase rela=par
sp3:subphrase rela=par

sp2 -mother> sp1
sp3 -mother> sp1

sp2 # sp3
"""

test_multiple_coor = sorted(S.search(test_multiple_coor))

len(test_multiple_coor)

# %% [markdown]
# No, it does not happen. Thus, coordinate relations are chained to each other, not multiplied to a single mother.

# %% [markdown]
# #### Conclusions
# This inquiry sparked the one above it about the subphrase by subphrase approach. We have decided to table this method for now.
