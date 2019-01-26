###########################################################################################
#                                                                                         #
#                     ALGORITHME TYPE MTGS OPTIMISE                                       #
#                                                                                         #
#                     AUTEUR: Rémi FELIN                                                  #
#                     NOM EQUIPE: LionsRT                                                 #
#                                                                                         #
###########################################################################################


LionsRT <- function(g) {
  
  if (length(g) == 0) {                                       #si on a un vecteur nul 
    return (logical(0))
  }
  else {                                                      #pour tous les autres vecteurs
    
    
    G             <- sort(g, decreasing = TRUE)               #vecteur trié
    ordre         <- order(g,decreasing = TRUE)               #les indices du vecteur d'entré (permet de mettre les T/F dans le bon ordre) 
    contrainte    <- (sum(as.numeric(g))%/%2)                 #primordial pour appliquer GS, sera un élément test (c'est la somme de tous les éléments divisé par 2)   
    Resultat      <- logical(length(g))                       #sera le vecteur que la fonction sortira, pour l'instant il est vide
    GS1           <- 0                                        #somme du 1er paquet
    GS2           <- 0                                        #somme du 2nd paquet
  
#GS SANS DECALAGE
    
    i=1                                                       #valeur initiale pour la boucle while(1 car on test tous les éléments du vecteur)
    
    while (i <= length(g)){                                   #pour tous les élements:
      if ((GS1+G[i]) <= contrainte) {                         #si le premier élément du veceur trié + GS1 inférieur à la contrainte
        GS1           <- as.numeric(GS1 + G[i])               #nouvelle valeur de GS1 (on lui ajoute G[i])
        Resultat[i]  <- TRUE                                  #on assigne au vecteur Resultat en position i la valeur T
      }
      else {                                                  #dans le cas ou le premier élément du veceur trié + GS1 supérieur à la contrainte                                              
        GS2           <- as.numeric(GS2 + G[i])               #nouvelle valeur de GS2 (on lui ajoute G[i])
        Resultat[i]  <- FALSE                                 #on assigne au vecteur Resultat en position i la valeur F
      }
      i=i+1                                                   #cela signifie que le while devra de nouveau boucler pour le i suivant
    }
    
    Diff <- abs(GS1-GS2)                                      #différence des 2 paquets (>0) que l'on va tester
    
    #on test:
    
    if (Diff == 0){                                           #trie parfait !
      Resultat[ordre]=Resultat
                                                              #on remet les T/F dans l'ordre initial par rapport à g
      return(Resultat)                                        #on retourne le vecteur booléen
    }
    if (Diff == 1){                                           #de même pour celui-ci...
      Resultat[ordre]=Resultat
      print(Diff)
      return(Resultat)
    }
    else {                                                    #sinon on applique mtgs:
      
#GS AVEC DECALAGE
      
    for (n in 1:length(g)){
        
      newGS1 <- 0                                             #nouvelle somme du 1er paquet
      newGS2 <- sum(as.numeric(G[1:n]))                       #nouvelle somme du 2nd paquet
      newResultat <- logical(length(g))                       #nouveau résultat (vide pour l'instant)
      newResultat[ordre[1:n]] <- FALSE                        #on définie resultat tel que les n premiers éléments seront FALSE
      i=n+1                                                   #comme on cherche T/F que pour les autres éléments du vecteur alors i=n+1
        
        
      while (i <= length(g)){                                 #GS pour les éléments restant...
        if ((newGS1+G[i]) <= contrainte) {         
          newGS1           <- as.numeric(newGS1 + G[i]) 
          newResultat[i]  <- TRUE
        }
        else {
          newGS2           <- as.numeric(newGS2 + G[i])
          newResultat[i]  <- FALSE 
        }
        i=i+1
      }
        
      newDiff <- abs((newGS1)-(newGS2))                       #à l'issue du n éme décalage on a une nouvelle différence (>0) que l'on va tester
        
    #Nouveau tests:        
        
         
      if (newDiff < Diff){                                    #si la nouvelle différence < différence alors le trie est + satisfaisant donc:
          Diff <- newDiff                                     #différence prendra la valeur de la nouvelle différence trouvé
          Resultat <- newResultat                             #resultat prendra les valeurs du nouveau résultat trouvé
      }
      if (Diff == 0){                                         #si on a un trie parfait...
          Resultat[ordre]=Resultat
          
          return(Resultat)
      }
      if (Diff == 1){                                         #si on a un trie parfait...
          Resultat[ordre]=Resultat
        
          return(Resultat)
      }
      else{                                                   #est-ce qu'il peut y avoir un meilleur trie possible lors du prochain décalage ?
      }
                                                              #on reboucle dans for, etc ...
        
    }     
                                                              #si à l'issue du n éme décalage on a pas de trie 'parfait' alors on a retenue (grace au test dans for) la meilleure différence des 2 paquets
    Resultat[ordre]=Resultat                              
    return(Resultat)                                          #alors on retourne le meilleur résultat de mtgs
    }
  }
}

