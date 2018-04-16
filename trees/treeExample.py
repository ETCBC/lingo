from tf.fabric import Fabric
from tf.extra.bhsa import Bhsa

from tree import Tree


VERSION = '2017'

sp = 'part_of_speech' if VERSION == '3' else 'sp'
rela = 'clause_constituent_relation' if VERSION == '3' else 'rela'
ptyp = 'phrase_type' if VERSION == '3' else 'typ'
ctyp = 'clause_atom_type' if VERSION == '3' else 'typ'
g_word_utf8 = 'text' if VERSION == '3' else 'g_word_utf8'

TF = Fabric(locations=[
    f'~/github/etcbc/bhsa/tf/{VERSION}',
])
api = TF.load(f'''
    {sp} {rela} {ptyp} {ctyp}
    {g_word_utf8}
    mother
''')
api.makeAvailableIn(globals())

B = Bhsa(api, 'test', version=VERSION)

treeTypes = ('sentence', 'clause', 'phrase', 'subphrase', 'word')
(rootType, leafType, clauseType) = (treeTypes[0], treeTypes[-1], 'clause')

tree = Tree(
    TF,
    otypes=treeTypes,
    clauseType=clauseType,
    ccrFeature=rela,
    ptFeature=ptyp,
    posFeature=sp,
    motherFeature='mother',
)
