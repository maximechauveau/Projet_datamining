###############################################
######## Phase d'automatisation ###############
###############################################

install.packages('tm')
install.packages('stringr')
library('tm')
library('stringr')

# Les arguments :
#   x               data frame contenant :     $text     Les textes a noter
#
#   lexique         data frame contenant :    $mots     les mots du lexique
#                                             $poids    le poids des mots
#
#   dictionnaire    char vector, NULL par defaut. Dictionnaire de la langue utilisee
#
#   motimp          char vector, NULL par defaut. Liste des noms propres ou mots important inconnus du dictionnaire et qui ne doivent pas etre modifies
#
#   seuilTot        numeric, 3.4 par defaut. Frequence minimale d'apparition d'un mot pour etre ajoute au lexique
#   
#   seuilIntra      numeric, 64 par defaut. Frequence miniale d'appartition d'un mot dans sa categorie pour etre ajoute au lexique
#
#   seuilNeutre     numeric, 0 par defaut. Score en dessous duquel un texte est classe comme neutre

# En sortie : une liste de tables
#   sortie[[1]] :   l'echantillon test note contenant :
#                                             $text           les textes
#                                             $note           la note attribue a priori : -1 | 0 | 1
#                                             $MatchPos       le nombre de mots positifs trouves dans le texte
#                                             $MatchNeg       le nombre de mots negatifs trouves dans le texte
#                                             $scoreCalcule   le score calcule
#                                             $noteApost      la classification evaluee a posteriori :   -1 | 0 | 1
#
#   sortie[[2]] :   le lexique (enrichie par les derniers resultats) :
#                                             $mots
#                                             $poids

classtext <- function(x, Lexique, dictionnaire=NULL, motimp=NULL, seuilTot=3.4, seuilIntra=64, seuilNeutre=0){
  #########################################
  #### Quelques fonctions utiles ##########
  #########################################
  
  #retire les accents
  Unaccent    <- function(text) {
    text <- gsub("['`^~\"]", " ", text)
    text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
    text <- gsub("['`^~\"]", "", text)
    return(text)
  }
  
  # premiere function de nettoyage 
  cleanCorpus <- function(corpus){
    #ces premieres fonctions s'appliquent a des corpus de documents
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "(f|ht)(tp)(s?)(://)(.*)[.|/](.*)" , replacement="") # supprime les URL
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "(f|ht)(tp)(s?)(*)" , replacement="")
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "t'|l'|d'|j'|m'|c'|n'|N'|T'|L'|D'|'|c'|n'|qu'|Qu'", replacement ="") # supprime les approstrophes
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "RT|via|Retweeted", replacement ="") # retire des mots specifiques a twitter
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "_", replacement =" ") # retire les underscore
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "à", replacement ='a') #        Retire les accents
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "é|è|ê|ë", replacement = 'e')
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "î|ï", replacement ='i')
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'ö|ô', replacement = 'o')
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'ù', replacement = 'u')
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "([[:lower:]])([[:upper:]])", replacement = "\\1 \\2") # separe des deux mots s'il y a une majuscule
    corpus <- tm_map(corpus, removeWords, stopwords("french")) # supprimes les mots sans importance (le la donc quand...)
    corpus <- tm_map(corpus, removeNumbers) # supprime les chiffres
    corpus <- tm_map(corpus, removePunctuation) # supprime la ponctuation
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "@", replacement="") #supprime les @
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "#", replacement="") # supprime les #
    corpus <- tm_map(corpus, stripWhitespace) # supprime les espaces superflus
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "([[:lower:]])([[:upper:]])", replacement = "\\1 \\2") # separe des deux mots s'il y a une majuscule
    corpus <- tm_map(corpus, tolower) # met tout en majuscules
    return(corpus)
  }    
  
  # Correction affinee mot a mot, suppression des mots de moins de 3 lettres
  corrigeMots <- function(chain, dictionnaire=NULL, wordbank=NULL){
    if(is.na(chain)){return()} #pour eviter les erreurs, on evacue les eventuelles chaines vides
    n= str_length(chain)
    if (n<4){return()}
    if(chain %in% dictionnaire){#on regarde d'abord s'il est dans le dictionnaire
      return(chain)
    }
    for(mot in wordbank){#on regarde s'il ne s'agit pas de l'un des mots important noye dans une chaine. Ex FillonPresident -> Fillon
      if (!is.na(str_extract(chain, mot))){
        return(mot)
      }
    }
    
    if(n>25){return()}
    if(n>5){                #on regarde si il manque peut etre un espace entre deux mots
      for (i in 1:(n-1)){
        a=substr(chain, 1,i)
        b=substr(chain,i+1,n)
        c=(!is.na(match(a,dictionnaire)))
        d=(!is.na(match(b,dictionnaire)))
        if(c&d){
          return(c(a,b))
        }
      }
    }
    n= str_length(chain)
    if (n<4){return()}
    return(removePunctuation(chain))
  } 
  
  # Extrait tous les mots CORRIGES d'un corpus de textes
  WordsCorpus <- function(corp, dictionnaire=NULL, wordbank=NULL){
    liste=unlist(corp)
    N=length(corp)
    liste_mots=character()
    for(j in 1:N){
      phrase<-liste[j]
      mots=unlist(str_split(phrase, '\\s+'))
      n=length(mots)
      for(mot in mots){
        mot=corrigeMots(mot, dictionnaire = dictionnaire, wordbank = wordbank)
        if(!is.null(mot)) {
          #mot=stemDocument(mot, language='french')
          if(length(mot)>1){
            for(moti in mot){
              if(str_length(moti)>3){
                liste_mots=c(liste_mots, mot)
              }
            }
          }
          else {
            if(str_length(mot)>3){
              liste_mots=c(liste_mots, mot)
            }
          }
        }
      }
    }
    return(liste_mots)
  }  
  
  # Fonction recursive qui Classe les tweets en positif, neutre, negatif en fonction de leur score calcule
  getnote     <- function(x){
    if(is.null(x)){return()}
    if(is.na(x[1])){return()}
    n=length(x)
    if(x[1]>seuilNeutre){
      x[1]=1
      return(c(x[1],getnote(x[2:n])))
    }
    if(x[1]<(-seuilNeutre)){
      x[1]=-1
      return(c(x[1],getnote(x[2:n])))
    }
    return(c(0,getnote(x[2:n])))
  }
  
  # Fonction recursive qui transforme un vecteur Char en phrase
  str_concat <- function(x){
    if(is.null(x[1])){return()}
    if(is.na(x[1])){return()}
    return(paste(x[1],str_concat(x[-1])))
  }
  
  #on met en minuscule et on enleve les accents du dictionnaire et des Mots Importants
  dictionnaire=unique(tolower(Unaccent(dictionnaire)))
  motimp=tolower(Unaccent(motimp)) 
  
  #########################################
  #### classification des textes ##########
  #########################################
  textes=as.character(x$text)
  textescor=unlist(cleanCorpus(Corpus(VectorSource(textes))))
  N=length(textescor)
  score=NULL  #Contiendra les scores des textes
  PM=NULL     #Contiendra le nombre de match positifs
  NM=NULL     #Contiendra le nombre de match negatifs
  tx=NULL     #COntiendra les textes purs
  tx_corr=NULL #Contiendra les textes nettoyes
  for(j in 1:(N-1)){
    phrase=textescor[j]  ## On travaillera sur chaque texte de l'echantillon
    mots=unlist(str_split(phrase, ' ')) ## on extrait les mots du texte
    motcor=NULL # Contiendra les mots corriges du texte
    for(mot in mots){
      mot=corrigeMots(mot, dictionnaire = dictionnaire, wordbank = motimp)  # on corrige le mot 
      motcor=c(motcor,mot)                                                  # et on le place de notre liste de mots corriges
    }
    if(!is.null(motcor)){
      tx=c(tx,textes[j])
      posmatch=length(Lexique$mots[(Lexique$poids>0)&(Lexique$mots %in% motcor)]) # On compte les mots du lexique trouves dans le tweet
      negmatch=length(Lexique$mots[(Lexique$poids<0)&(Lexique$mots %in% motcor)])
      tx_corr=c(tx_corr,str_concat(motcor))
      sc=sum(Lexique$poids[Lexique$mots %in% motcor])                             # le score est la somme du poids de chaque mot matche
      score=c(score,sc) ; PM=c(PM, posmatch) ; NM=c(NM,negmatch) ## on sauvegarde les mesures
    }
  }
  # On construit la table "test" finale
  resultats=data.frame(text=tx,text_corrige=tx_corr, MatchPos=PM, MatchNeg=NM, scoreCalcule=score, noteApost=getnote(score), stringsAsFactors=F)

  #########################################
  #### Mise a jour du dictionnaire ########
  #########################################
  xMatch=resultats[resultats$MatchPos!=0|resultats$MatchNeg!=0,]
  # lex est un vecteur contenant tous les mots corriges du corpus.
  # c'est une etape preliminaire avant d'etudier quels sont les mots positifs et negatifs
  # Les textes passent d'abord dans la fonction cleancorpus
  # puis dans la fonction WordCorpus qui extrait les mots et les corrige en faisant appel a CorrigeMots
  lex=WordsCorpus(cleanCorpus(Corpus(VectorSource(xMatch$text))),dictionnaire = dictionnaire, wordbank = motimp)
  lex=unique(lex) # on extrait tous les mots corrige et supprime les doublons
  # On construit la freqtab, c'est une etape intermediaire
  # Cette table contiendra :    $mots     la liste de tous les mots
  #                             $tot      frequence totale du mot dans les textes
  #                             $neg      frequence du mot dans les textes negatifs
  #                             $pos      frequence du mot dans les textes positifs
  uliste=xMatch$text[xMatch$note!=0] ## on extrait l'ensemble des textes non neutres (les neutres ne doivent pas biaiser le dictionnaire)
  u=length(uliste)
  nliste=xMatch$text[xMatch$note<0]  ## on construit une liste de textes negatifs
  n=length(nliste)
  pliste=xMatch$text[xMatch$note>0]  ## on construit une liste de textes positifs
  p=length(pliste)
  freqtot=NULL ; freqneg=NULL ; freqpos=NULL
  for(mot in lex){ ## pour chaque mot du lexique on execute :
    r=length(uliste[!is.na(str_extract(uliste,mot))])   ## le nombre d'occurences du mot dans l'ensemble des textes non neutres
    rn=length(nliste[!is.na(str_extract(nliste,mot))])  ## le nombre de textes negatifs qui contiennent le mot
    rp=length(pliste[!is.na(str_extract(pliste,mot))])  ## le nombre de textes positifs qui contiennent le mot : donc rn + rp =r
    freqtot=c(freqtot,100*r/u) ; freqneg=c(freqneg,100*rn/r) ; freqpos=c(freqpos,100*rp/r) #On sauvegarde les mesures
    #print(c(100*r/u,100*rn/r,100*rp/r))
  }
  freqtab=data.frame(mots=lex, tot=freqtot, neg=freqneg, pos=freqpos, stringsAsFactors=F)
  # Enfin on construit la table Lexique 
  # Cette table contient :      $mots     les mots choisis pour apparaitre dans le lexique
  #                             $poids    le poids du mot (-1|1 x frequence du mot dans sa categorie)
  lexMots=c(as.character(freqtab$mots[(freqtab$tot>seuilTot)&(freqtab$neg>seuilIntra)]),as.character(freqtab$mots[(freqtab$tot>seuilTot)&(freqtab$pos>seuilIntra)]))
  lexPoids=c(-(freqtab$neg[(freqtab$tot>seuilTot)&(freqtab$neg>seuilIntra)]),freqtab$pos[(freqtab$tot>seuilTot)&(freqtab$pos>seuilIntra)])
  
  lexMots2=  c(lexMots,  Lexique$mots[!(Lexique$mots %in% lexMots)])
  lexPoids2= c(lexPoids, Lexique$poids[!(Lexique$mots %in% lexMots)])
  
  NewLexique=data.frame(mots=lexMots2, poids=lexPoids2, stringsAsFactors=F)
  return(list(xMatch,NewLexique))
}















