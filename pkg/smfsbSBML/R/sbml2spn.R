## sbml2spn.R
## Parse an SBML file into a smfsb SPN object

require(libSBML)
require(smfsb)

sbml2spn = function(filename, verb=FALSE) {
    d = readSBML(filename)
    m = d$getModel()
    if (is.null(m))
        stop(paste("Can't parse SBML file:",filename))
    ## Species and initial amounts
    P=list()
    ns = m$getNumSpecies()
    if (verb) cat(ns,"species\n")
    Mv = numeric(ns)
    Mn = vector("character",ns)
    for (i in 0:(ns-1)) {
        s = m$getSpecies(i)
        Mn[i+1] = s$getId()
        a = s$getInitialAmount()
        Mv[i+1] = a
    }
    names(Mv) = Mn
    if (verb) print(Mn)
    P$M = Mv
    P$P = Mn
    ## Compartments
    ncomp = m$getNumCompartments()
    Cv = vector("numeric",ncomp)
    Cn = vector("character",ncomp)
    if (ncomp>0) {
        for (i in 0:(ncomp-1)) {
            comp = m$getCompartment(i)
            Cv[i+1] = comp$getSize()
            Cn[i+1] = comp$getId()
        }
        names(Cv) = Cn
    }
    ## Global parameters
    nparm = m$getNumParameters()
    GPv = vector("numeric",nparm)
    GPn = vector("character",nparm)
    if (nparm>0) {
        for (i in 0:(nparm-1)) {
            parm = m$getParameter(i)
            GPv[i+1] = parm$getValue()
            GPn[i+1] = parm$getId()
        }
        names(GPv) = GPn
    }
    ## Reactions
    nr = m$getNumReactions()
    if (verb) cat(nr,"reactions\n")
    Pre = matrix(0,nrow=nr,ncol=ns)
    colnames(Pre)=Mn
    Post = matrix(0,nrow=nr,ncol=ns)
    colnames(Post)=Mn
    Rn = vector("character",nr)
    KLv = vector("expression",nr)
    LPl = vector("list",nr)
    for (i in 0:(nr-1)) {
        r = m$getReaction(i)
        Rn[i+1] = r$getId()
        nPre = r$getNumReactants()
        if (nPre>0) {
            for (j in 0:(nPre-1)) {
                sr = r$getReactant(j)
                sto = sr$getStoichiometry()
                Pre[i+1, sr$getSpecies()] = sto
            }
        }
        nPost = r$getNumProducts()
        if (nPost>0) {
            for (j in 0:(nPost-1)) {
                sr = r$getProduct(j)
                sto = sr$getStoichiometry()
                Post[i+1,sr$getSpecies()] = sto
            }
        }
        kl = r$getKineticLaw()
        KLv[i+1] = parse(text=formulaToString(kl$getMath()))
        nparm = kl$getNumLocalParameters()
        if (nparm>0) {
            lpv = vector("numeric",nparm)
            lpn = vector("character",nparm)
            for (j in 0:(nparm-1)) {
                parm = kl$getLocalParameter(j)
                lpv[j+1] = parm$getValue()
                lpn[j+1] = parm$getId()
            }
            names(lpv) = lpn
            LPl[[i+1]] = lpv
        }
    }
    if (verb) print(Rn)
    rownames(Pre)=Rn
    rownames(Post)=Rn
    P$T = Rn
    P$Pre = Pre
    P$Post = Post
    P$Comp = Cv
    P$GP = GPv
    P$KL = KLv
    P$LP = LPl
    P$h = function(x, t, comp = Cv, gp = GPv, lp = LPl, param=NULL) {
        with(as.list(c(x, param, comp, gp)), {
            x = vector("numeric",nr)
            for (i in 1:nr) {
                x[i] = with(as.list(lp[[i]]), eval(KLv[i]))
            }
            return(x)
        })
    }
    P
}



## eof

