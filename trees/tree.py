import collections
import copy
import functools

from tf.helpers import rangesFromList, specFromRanges

class Tree(object):
    def __init__(self, TF, otypes=None, clauseType=None, phraseType=None,
            ccrFeature=None, ptFeature=None, posFeature=None, motherFeature=None
        ):
        features = '{} {} {} {}'.format(
            ccrFeature if ccrFeature != None else '',
            ptFeature if ptFeature != None else '',
            posFeature if posFeature != None else '',
            motherFeature if motherFeature != None else '',
        )
        TF.load(features, add=True)
        api = TF.api
        self.api = api
        self.ccrFeature = ccrFeature
        self.posFeature = posFeature
        self.ptFeature = ptFeature
        N = api.N
        F = api.F
        C = api.C
        E = api.E
        info = api.info

        if otypes == None: otypes = [x[0] for x in C.levels.data]
        if clauseType == None: clauseType = 'clause'
        if phraseType == None: phraseType = 'phrase'
        self.rootType = otypes[0]
        self.leafType = otypes[-1]
        self.clauseType = clauseType
        self.phraseType = phraseType
        info('Start computing parent and children relations for objects of type {}'.format(', '.join(otypes)))
        otypeSet = set(otypes)
        self.otypeSet = otypeSet
        self.leafList = {'r': {}, 'e': {}}
        baseType = otypes[-1]
        curStack = []
        eparent = {}
        echildren = collections.defaultdict(lambda: [])
        nn = 0
        cn = 0
        chunk = 100000
        Fotypev = F.otype.v
        Eoslotss = E.oslots.s
        for node in N():
            otype = Fotypev(node)
            if otype not in otypeSet: continue
            nn += 1
            cn += 1
            if cn == chunk:
                info('{} nodes'.format(nn))
                cn = 0
            nmSlots = Eoslotss(node)
            nmMin = nmSlots[0]
            nmMax = nmSlots[-1]
            ls = len(curStack)
            tobeRemoved = set()
            for si in range(ls):
                actOn = ls -si - 1
                (snode, smSlots, smMax) = curStack[actOn]
                if nmMin > smMax:
                    tobeRemoved.add(actOn)
                    continue
                if set(nmSlots) <= set(smSlots):
                    eparent[node] = snode
                    echildren[snode].append(node)
                    break;
            curStack = [curStack[i] for i in range(len(curStack)) if i not in tobeRemoved]
            if otype != baseType: curStack.append((node, nmSlots, nmMax))
        info('{} nodes: {} have parents and {} have children'.format(nn, len(eparent), len(echildren)))
        self.eparent = eparent
        self.echildren = echildren
        self.elderSister = {}
        self.rparent = {}
        self.rchildren = {}
        self.sisters = {}
        self.elderSister = {}
        self.mother = {}

    def restructureClauses(self, ccrClass):
        api = self.api
        N = api.N
        F = api.F
        Fs = api.Fs
        E = api.E
        info = api.info
        error = api.error
        info('Restructuring {}s: deep copying tree relations'.format(self.clauseType))
        Eoslotss = E.oslots.s
        rparent = copy.deepcopy(self.eparent)
        rchildren = copy.deepcopy(self.echildren)
        sisters = collections.defaultdict(lambda: [])
        elderSister = {}
        mother = {}
        self.rparent = rparent
        self.rchildren = rchildren
        self.sisters = sisters
        self.elderSister = elderSister
        self.mother = mother
        if self.ccrFeature == None: return
        otypeSet = self.otypeSet

        info('Pass 0: Storing mother relationship')
        moutside = collections.defaultdict(lambda: 0)
        mo = 0
        mf = E.mother.f
        for c in F.otype.s(self.clauseType):
            lms = list(mf(c))
            ms = len(lms)
            if ms:
                m = lms[0]
                mtype = F.otype.v(m)
                if mtype in otypeSet: mother[c] = m
                else:
                    moutside[mtype] += 1
                    mo += 1
        info('{} {}s have a mother'.format(len(mother), self.clauseType))
        if mo:
            error('{} {}s have mothers of types outside {}.\nThese mother relationships will be ignored'.format(mo, self.clauseType, otypeSet))
            for mt in sorted(moutside):
                error('{} mothers point to {} nodes'.format(moutside[mt], mt), withtime=False)
        else:
            info('All {}s have mothers of types in {}'.format(self.clauseType, otypeSet))

        info('Pass 1: all {}s except those of type Coor'.format(self.clauseType))
        motherless = set()
        ccrf = Fs(self.ccrFeature).v
        Fotypev = F.otype.v
        for cnode in F.otype.s(self.clauseType):
            cclass = ccrClass[ccrf(cnode)]
            if cclass == 'n' or cclass == 'x': pass
            elif cclass == 'r':
                if cnode not in mother:
                    motherless.add(cnode)
                    continue
                mnode = mother[cnode]
                mtype = Fotypev(mnode)
                pnode = rparent[cnode]
                if mnode not in rparent:
                    error('Should not happen: node without parent: [{} {}]({}) =mother=> [{} {}]({}) =/=> parent'.format(
                        self.clauseType, Eoslotss(cnode), cnode, mtype, Eoslotss(mnode), mnode
                    ))
                pmnode = rparent[mnode]
                pchildren = rchildren[pnode]
                mchildren = rchildren[mnode]
                pmchildren = rchildren[pmnode]
                deli = pchildren.index(cnode)
                if mtype == self.leafType:
                    if pnode != pmnode:
                        rparent[cnode] = pmnode
                        del pchildren[deli:deli+1]
                        pmchildren.append(cnode)
                else:
                    if pnode != mnode:
                        rparent[cnode] = mnode
                        del pchildren[deli:deli+1]
                        mchildren.append(cnode)
        info('Pass 2: {}s of type Coor only'.format(self.clauseType))
        for cnode in F.otype.s(self.clauseType):
            cclass = ccrClass[ccrf(cnode)]
            if cclass != 'x': continue
            if cnode not in mother:
                motherless.add(cnode)
                continue
            mnode = mother[cnode]
            pnode = rparent[cnode]
            pchildren = rchildren[pnode]
            deli = pchildren.index(cnode)
            sisters[mnode].append(cnode)
            elderSister[cnode] = mnode
            del rparent[cnode]
            del pchildren[deli:deli+1]

        sisterCount = collections.defaultdict(lambda: 0)
        for n in sisters:
            sns = sisters[n]
            sisterCount[len(sns)] += 1
        info('Mothers applied. Found {} motherless {}s.'.format(len(motherless), self.clauseType))
        ts = 0
        for l in sorted(sisterCount):
            c = sisterCount[l]
            ts += c * l
            info('{} nodes have {} sisters'.format(c, l))
        info('There are {} sisters, {} nodes have sisters.'.format(ts, len(sisters)))
        motherless = None

    def relations(self): return {
        'eparent': self.eparent,
        'echildren': self.echildren,
        'rparent': self.rparent,
        'rchildren': self.rchildren,
        'sisters': self.sisters,
        'elderSister': self.elderSister,
        'mother': self.mother,
    }

    def getSisters(self, node):
        sisters = self.sisters
        sortKey = self.api.sortKey
        def _getSisters(n):
            result = (n,)
            if n in sisters and len(sisters[n]):
                for sn in sisters[n]: result += _getSisters(sn)
            return result
        return sorted(_getSisters(node), key=sortKey)

    def getChildren(self, node, kind):
        children = self.rchildren if kind == 'r' else self.echildren 
        sortKey = self.api.sortKey
        cnodes = ()
        if node in children and len(children[node]):
            cnodes = children[node]
        return sorted(cnodes, key=sortKey)

    def slotss(self, node):
        api = self.api
        E = api.E
        return specFromRanges(rangesFromList(E.oslots.s(node)))

    def debugWriteTree(self, node, kind, legenda=False):
        api = self.api
        F = api.F
        Fs = api.Fs
        E = api.E
        result = []
        ids= {}
        maxid = 0
        ccrf = Fs(self.ccrFeature).v
        Fotypev = F.otype.v
        Eoslotss = E.oslots.s
        bSlot = Eoslotss(node)[0]
        elderSister = self.elderSister
        sisters = self.sisters if kind == 'r' else {}
        mother = self.mother

        def rep(n):
            if n in ids: return ids[n]
            nonlocal maxid
            maxid += 1
            ids[n] = maxid
            return maxid

        def _fillids(node):
            otype = F.otype.v(node)
            parent = self.eparent 
            children = self.echildren 
            if node in mother: rep(mother[node])
            rep(node)
            if node in children:
                for cnode in children[node]:
                    _fillids(cnode)

        def _debugWriteTree(node, level, indent):
            cnodes = self.getChildren(node, kind)
            otype = Fotypev(node)
            subType = ''
            subTypeSep = ''
            mspec = ''
            if otype == self.clauseType:
                subType = ccrf(node)
                if subType != None and subType != 'none':
                    subTypeSep = '.'
                else:
                    subType = ''
                    subTypeSep = ''
                if kind == 'e':
                    if node in mother:
                        mspec = '=> ({:>3})'.format(rep(mother[node]))
                elif kind == 'r':
                    if node in elderSister:
                        mspec = '=> ({:>3})'.format(rep(elderSister[node]))
            elif otype == self.phraseType:
                subType = Fs(self.ptFeature).v(node)
                if subType != None:
                    subTypeSep = '.'
                else:
                    subType = ''
                    subTypeSep = ''
            elif otype == self.leafType:
                posf = Fs(self.posFeature).v
                subType = posf(node)
                if subType != None:
                    subTypeSep = '.'
                else:
                    subType = ''
                    subTypeSep = ''

            result.append('{:>2}{:<30} {:<30}] ({:>3}) {:<8} <{}>\n'.format(
                level,
                '{}[{:<10}'.format(indent, '{}{}{}'.format(otype, subTypeSep, subType)),
                self.slotss(node),
                rep(node),
                mspec,
                ','.join('{:>3}'.format(rep(c)) for c in cnodes),
            ))
            if kind == 'e':
                for cnode in cnodes: _debugWriteTree(cnode, level + 1, indent + '  ')
            else:
                for cnode in cnodes:
                    snodes = self.getSisters(cnode)
                    if len(snodes) == 1:
                        _debugWriteTree(cnode, level + 1, indent + '  ')
                    else:
                        for snode in snodes:
                            if snode == cnode:
                                _debugWriteTree(snode, level + 1, indent + '  ' + '*')
                            else:
                                _debugWriteTree(snode, level + 1, indent + '  ' + '=')
        _fillids(node)
        _debugWriteTree(node, 0, '')
        if legenda:
            result.append('\nstart slot = {}\n\n'.format(bSlot))
            result.append('{:>3} = {:>8}\n'.format('#', 'TF-node'))
            for (n, s) in sorted(ids.items(), key=lambda x: x[1]):
                result.append('{:>3} = {:>8}\n'.format(s, n))
        return ''.join(result)

    def writeTree(self, node, kind, getTag, rev=False, leafNumbers=True):
        api = self.api
        F = api.F
        E = api.E
        info = api.info
        otype = F.otype.v(node)
        children = self.rchildren if kind == 'r' else self.echildren 
        sisters = self.sisters if kind == 'r' else {}
        bSlot = E.oslots.s(node)[0]

        words = []
        sequential = []
        def _writeTree(node):
            eldest = node in sisters and len(sisters[node])
            (tag, pos, slot, text, isWord) = getTag(node)
            if isWord:
                sequential.append(('W', len(words)))
                words.append((slot - bSlot, text, pos))
            else: sequential.append(('O', tag))
            cnodes = self.getChildren(node, kind)
            if kind == 'e':
                for cnode in cnodes: _writeTree(cnode)
            else:
                for cnode in cnodes:
                    snodes = self.getSisters(cnode)
                    if len(snodes) == 1: _writeTree(cnode)
                    else:
                        ctag = getTag(cnode)[0]
                        sequential.append(('O', ctag))
                        for snode in snodes:
                            if snode == cnode:
                                 sequential.append(('O', 'Ccoor'))
                                 for csnode in self.getChildren(snode, kind): _writeTree(csnode)
                                 sequential.append(('C', 'Ccoor'))
                            else:
                                _writeTree(snode)
                        sequential.append(('C', ctag))
            if not isWord: sequential.append(('C', tag))

        def doSequential():
            if leafNumbers: wordRep = ' '.join(x[1] for x in sorted(words, key=lambda x: x[0]))
            else: wordRep = ' '.join(str(x[0]) for x in words)
                            
            treeRep = []
            for (code, data) in sequential:
                if code == 'O' or code == 'C':
                    if code == 'O': treeRep.append('({}'.format(data))
                    else: treeRep.append(')')
                elif code == 'W':
                    (slot, text, pos) = words[data]
                    leaf = slot if leafNumbers else text[::-1] if rev else text
                    treeRep.append('({} {})'.format(pos, leaf))
            return (''.join(treeRep), wordRep[::-1] if rev and leafNumbers else wordRep, bSlot) 

        _writeTree(node)
        return doSequential()

    def depth(self, node, kind):
        api = self.api
        F = api.F
        def _depth(node, kind):
            parent = self.rparent if kind == 'r' else self.eparent 
            children = self.rchildren if kind == 'r' else self.echildren 
            sisters = self.sisters
            elderSister = self.elderSister
            hasSisters =  node in sisters and len(sisters[node])
            hasChildren = node in children and len(children[node])
            cdepth = 1 + max(_depth(c, kind) for c in children[node]) if hasChildren else 0
            sdepth = 1 + max(_depth(s, kind) for s in sisters[node]) if hasSisters else 0
            if kind == 'e': return cdepth
            elif kind == 'r': return max(cdepth + 1, sdepth) if hasSisters else cdepth
        return _depth(node, kind)

    def length(self, node): return len(self.api.E.oslots.s(node))

    def getLeaves(self, node, kind):
        api = self.api
        F = api.F
        visited = set()
        myLeafList = self.leafList[kind]
        parent = self.rparent if kind == 'r' else self.eparent 
        children = self.rchildren if kind == 'r' else self.echildren 
        sisters = self.sisters if kind == 'r' else {}
        elderSister = self.elderSister

        def _getLeaves(node, withSisters=False):
            if node in myLeafList:
                return  myLeafList[node]
            if node in visited: return ()
            visited.add(node)
            result = ()
            if node in children and len(children[node]) > 0:
                for cnode in children[node]:
                    result += _getLeaves(cnode, withSisters=True)
            else: result += (node,)
            if withSisters and node in sisters:
                for snode in sisters[node]:
                    result += _getLeaves(snode, withSisters=True)
            myLeafList[node] = result
            return result

        return _getLeaves(node, withSisters=False)

    def getSlots(self, node, kind):
        api = self.api
        F = api.F
        E = api.E
        Eoslotss = E.oslots.s
        return functools.reduce(lambda x,y: x | y, (set(Eoslotss(leaf)) for leaf in self.getLeaves(node, kind)), set())

    def getRoot(self, node, kind):
        api = self.api
        F = api.F
        otype = F.otype.v(node)
        parent = self.rparent if kind == 'r' else self.eparent 
        children = self.rchildren if kind == 'r' else self.echildren 
        if node in parent: return self.getRoot(parent[node], kind)
        if kind == 'r':
            if node in elderSister: return self.getRoot(elderSister[node], kind)
        return (node, otype)
    
