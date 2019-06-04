###############################################
####### Construction du dictionnaire ##########
############# d'apprentissage #################
###############################################

install.packages('tm')
install.packages('stringr')
library('tm')
library('stringr')

#Les arguments :
#   app             data frame contenant :    $text     les textes de l'échantillon d'apprentissage
#                                             $note     les notes attribuées au texte :-1 | 0 | 1
#
#   test            data frame, NULL par defaut. Echantillon test de la même forme que l'échantillon d'apprentissage :    $text   $note
#
#   dictionnaire    char vector, NULL par défaut. Dictionnaire de la langue utilisée
#
#   motimp          char vector, NULL par défaut. Liste des noms propres où mots important inconnus du dictionnaire et qui ne doivent pas être modifiés
#
#   seuilTot        numeric, 3.4 par defaut. Fréquence minimale d'apparition d'un mot pour etre ajouté au lexique
#   
#   seuilIntra      numeric, 64 par défaut. Fréquence miniale d'appartition d'un mot dans sa catégorie pour etre ajouté au léxique
#
#   seuilNeutre     numeric, 0 par défaut. Score en dessous duquel un texte est classé comme neutre


# En sortie : une liste de tables
#   sortie[[1]] :   le lexique contenant :    $mots
#                                             $poids          +1/-1 x la fréquence du mot dans sa catégorie
#
#   sortie[[2]] :   la table des fréquences de tous les mot contenant $mots $tot $neg $pos
#
#   sortie[[3]] :   l'echantillon test noté contenant :
#                                             $text           les textes
#                                             $note           la attribué à priori : -1 | 0 | 1
#                                             $MatchPos       le nombre de mots positifs trouvés dans le texte
#                                             $MatchNeg       le nombre de mots négatifs trouvés dans le texte
#                                             $scoreCalcule   le score calculé)
#                                             $noteApost      la classification évaluée a posteriori :   -1 | 0 | 1



apprentissage <- function(app , test=NULL, dictionnaire=NULL, motimp=NULL, seuilTot=3.4, seuilIntra=64, seuilNeutre=0){
  #########################################
  ##### Quelques vérifications ############
  #########################################
  
  if(is.null(app$text)){
    print("L'echantillon d'apprentissage doit etre de la forme $text / $note")
    return()
  }
  if(is.null(app$note)){
    print("L'echantillon d'apprentissage doit etre de la forme $text / $note")
    return()
  }
  if(!is.null(test)){
    if(is.null(test$text)){
      print("L'echantillon test doit etre de la forme $text / $note")
      return()
    }
  }
  
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
  
  # première function de nettoyage 
  cleanCorpus <- function(corpus){
    #ces premières fonctions s'appliquent à des corpus de documents
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "(f|ht)(tp)(s?)(://)(.*)[.|/](.*)" , replacement="") # supprime les URL
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "(f|ht)(tp)(s?)(*)" , replacement="")
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "t'|l'|d'|j'|m'|c'|n'|N'|T'|L'|D'|'|c'|n'|qu'|Qu'", replacement ="") # supprime les approstrophes
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "RT|via|Retweeted", replacement ="") # retire des mots spécifiques à twitter
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "_", replacement =" ") # retire les underscore
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "à", replacement ='a') #        Retire les accents
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "é|è|ê|ë", replacement = 'e')
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "ï|î", replacement ='i')
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'ô|ö', replacement = 'o')
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'ù', replacement = 'u')
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "([[:lower:]])([[:upper:]])", replacement = "\\1 \\2") # sépare des deux mots s'il y a une majuscule
    corpus <- tm_map(corpus, removeWords, stopwords("french")) # supprimes les mots sans importance (le la donc quand...)
    corpus <- tm_map(corpus, removeNumbers) # supprime les chiffres
    corpus <- tm_map(corpus, removePunctuation) # supprime la ponctuation
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "@", replacement="") #supprime les @
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "#", replacement="") # supprime les #
    corpus <- tm_map(corpus, stripWhitespace) # supprime les espaces superflus
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "([[:lower:]])([[:upper:]])", replacement = "\\1 \\2") # sépare des deux mots s'il y a une majuscule
    corpus <- tm_map(corpus, tolower) # met tout en majuscules
    return(corpus)
  }    
  
  # Correction affinée mot à mot, suppression des mots de moins de 3 lettres
  corrigeMots <- function(chain, dictionnaire=NULL, wordbank=NULL){
    if(is.na(chain)){return()} #pour éviter les erreurs, on évacue les éventuelles chaines vides
    n= str_length(chain)
    if (n<4){return()}
    if(chain %in% dictionnaire){#on regarde d'abord s'il est dans le dictionnaire
      return(chain)
    }
    for(mot in wordbank){#on regarde s'il ne s'agit pas de l'un des mots important noyé dans une chaine. Ex FillonPresident -> Fillon
      if (!is.na(str_extract(chain, mot))){
        return(mot)
      }
    }
    
    if(n>25){return()}
    if((n>5)&(!is.null(dictionnaire))){                #on regarde si il manque peut etre un espace entre deux mots
      for (i in 1:(n-1)){
        a=substr(chain, 1,i)
        b=substr(chain,i+1,n)
        c=(!(a %in% dictionnaire))
        d=(!(b %in% dictionnaire))
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
  
  # Fonction récursive qui Classe les tweets en positif, neutre, négatif en fonction de leur score calculé
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
  
  #on met en minuscule et on enlève les accents du dictionnaire et des Mots Importants
  dictionnaire=unique(tolower(Unaccent(dictionnaire)))
  motimp=tolower(Unaccent(motimp)) 
  
  ########################################
  #### construction du dictionnaire ######
  ########################################
  
  # lex est un vecteur contenant tous les mots corrigés du corpus.
  # c'est une étape préliminaire avant d'étudier quels sont les mots positifs et négatifs
  # Les textes passent d'abord dans la fonction cleancorpus
  # puis dans la fonction WordCorpus qui extrait les mots et les corrige en faisant appel à CorrigeMots
  lex=WordsCorpus(cleanCorpus(Corpus(VectorSource(app$text))),dictionnaire = dictionnaire, wordbank = motimp)
  lex=unique(lex) # on extrait tous les mots corrigé et supprime les doublons
  
  # On construit la freqtab, c'est une étape intermédiaire
  # Cette table contiendra :    $mots     la liste de tous les mots
  #                             $tot      fréquence totale du mot dans les textes
  #                             $neg      fréquence du mot dans les textes négatifs
  #                             $pos      fréquence du mot dans les textes positifs
  uliste=app$text[app$note!=0] ## on extrait l'ensemble des textes non neutres (les neutres ne doivent pas biaiser le dictionnaire)
  u=length(uliste)
  nliste=app$text[app$note<0]  ## on construit une liste de textes négatifs
  n=length(nliste)
  pliste=app$text[app$note>0]  ## on construit une liste de textes positifs
  p=length(pliste)
  freqtot=NULL ; freqneg=NULL ; freqpos=NULL
  for(mot in lex){ ## pour chaque mot du lexique on execute :
    r=length(uliste[!is.na(str_extract(uliste,mot))])   ## le nombre d'occurences du mot dans l'ensemble des textes non neutres
    rn=length(nliste[!is.na(str_extract(nliste,mot))])  ## le nombre de textes negatifs qui contiennent le mot
    rp=length(pliste[!is.na(str_extract(pliste,mot))])  ## le nombre de textes positifs qui contiennent le mot : donc rn + rp =r
    freqtot=c(freqtot,100*r/u) ; freqneg=c(freqneg,100*rn/r) ; freqpos=c(freqpos,100*rp/r) #On sauvegarde les mesures
  }
  freqtab=data.frame(mots=as.character(lex), tot=freqtot, neg=freqneg, pos=freqpos)
  freqtab=freqtab[freqtab$tot!=0,]
  
  # Enfin on construit la table Lexique 
  # Cette table contient :      $mots     les mots choisis pour apparaitre dans le lexique
  #                             $poids    le poids du mot (-1|1 x frequence du mot dans sa catégorie)
  lexMots=c(as.character(freqtab$mots[(freqtab$tot>seuilTot)&(freqtab$neg>seuilIntra)]),as.character(freqtab$mots[(freqtab$tot>seuilTot)&(freqtab$pos>seuilIntra)]))
  lexPoids=c(-freqtab$neg[(freqtab$tot>seuilTot)&(freqtab$neg>seuilIntra)],freqtab$pos[(freqtab$tot>seuilTot)&(freqtab$pos>seuilIntra)])
  Lexique=data.frame(mots=lexMots,poids=lexPoids, stringsAsFactors=F)

  if(is.null(test)){
    return(list(Lexique,freqtab))
  }
  #######################################
  #### application à l'échantillon ######
  ############### test ##################
  #######################################

  textes=as.character(test$text)
  N=length(textes)
  score=NULL  #Contiendra les scores des textes
  PM=NULL     #Contiendra le nombre de match positifs
  NM=NULL     #Contiendra le nombre de match négatifs
  for(j in 1:N){
    phrase=textes[j]  ## On travaillera sur chaque texte de l'échantillon
    mots=unlist(str_split(phrase, ' ')) ## on extrait les mots du j° texte
    motcor=NULL # Contiendra les mots corrigés du texte
    for(mot in mots){
      mot=corrigeMots(mot, dictionnaire = dictionnaire, wordbank = motimp)  # on corrige le mot 
      motcor=c(motcor,mot)                                                  # et on le place de notre liste de mots corrigés
    }
    posmatch=length(Lexique$mots[(Lexique$poids>0)&(Lexique$mots %in% motcor)]) # On compte les mots du lexique trouvés dans le tweet
    negmatch=length(Lexique$mots[(Lexique$poids<0)&(Lexique$mots %in% motcor)])
    
    sc=sum(Lexique$poids[Lexique$mots %in% motcor])                             # le score est la somme du poids de chaque mot matché
    score=c(score,sc) ; PM=c(PM, posmatch) ; NM=c(NM,negmatch) ## on sauvegarde les mesures
  }
  # On construit la table "test" finale
  test=data.frame(test, MatchPos=PM, MatchNeg=NM, scoreCalcule=score, noteApost=getnote(score))
  
  return(list(Lexique,freqtab,test))
}















