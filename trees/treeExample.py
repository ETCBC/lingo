import sys
import os

from tree import Tree

VERSION = '2017'

sp = 'part_of_speech' if VERSION == '3' else 'sp'
rela = 'clause_constituent_relation' if VERSION == '3' else 'rela'
ptyp = 'phrase_type' if VERSION == '3' else 'typ'
ctyp = 'clause_atom_type' if VERSION == '3' else 'typ'
g_word_utf8 = 'text' if VERSION == '3' else 'g_word_utf8'

LOC = ('~/github', 'etcbc/bhsa', 'test')
sys.path.append(os.path.expanduser(f'{LOC[0]}/{LOC[1]}/programs'))
from bhsa import Bhsa  # noqa: E402
B = Bhsa(*LOC, version='2017')
B.api.makeAvailableIn(globals())

B.load(f'''
    {sp} {rela} {ptyp} {ctyp}
    {g_word_utf8}
    mother
''')

treeTypes = ('sentence', 'clause', 'phrase', 'subphrase', 'word')
(rootType, leafType, clauseType) = (treeTypes[0], treeTypes[-1], 'clause')

tree = Tree(
    B.TF,
    otypes=treeTypes,
    clauseType=clauseType,
    ccrFeature=rela,
    ptFeature=ptyp,
    posFeature=sp,
    motherFeature='mother',
)
