###########################################################################################
#                                                                                         #
#                     ALGORITHME TYPE MTGS OPTIMISE                                       #
#                                                                                         #
#                     AUTEUR: R�mi FELIN                                                  #
#                     NOM EQUIPE: LionsRT                                                 #
#                                                                                         #
###########################################################################################


LionsRT <- function(g) {
  
  if (length(g) == 0) {                                       #si on a un vecteur nul 
    return (logical(0))
  }
  else {                                                      #pour tous les autres vecteurs
    
    
    G             <- sort(g, decreasing = TRUE)               #vecteur tri�
    ordre         <- order(g,decreasing = TRUE)               #les indices du vecteur d'entr� (permet de mettre les T/F dans le bon ordre) 
    contrainte    <- (sum(as.numeric(g))%/%2)                 #primordial pour appliquer GS, sera un �l�ment test (c'est la somme de tous les �l�ments divis� par 2)   
    Resultat      <- logical(length(g))                       #sera le vecteur que la fonction sortira, pour l'instant il est vide
    GS1           <- 0                                        #somme du 1er paquet
    GS2           <- 0                                        #somme du 2nd paquet
  
#GS SANS DECALAGE
    
    i=1                                                       #valeur initiale pour la boucle while(1 car on test tous les �l�ments du vecteur)
    
    while (i <= length(g)){                                   #pour tous les �lements:
      if ((GS1+G[i]) <= contrainte) {                         #si le premier �l�ment du veceur tri� + GS1 inf�rieur � la contrainte
        GS1           <- as.numeric(GS1 + G[i])               #nouvelle valeur de GS1 (on lui ajoute G[i])
        Resultat[i]  <- TRUE                                  #on assigne au vecteur Resultat en position i la valeur T
      }
      else {                                                  #dans le cas ou le premier �l�ment du veceur tri� + GS1 sup�rieur � la contrainte                                              
        GS2           <- as.numeric(GS2 + G[i])               #nouvelle valeur de GS2 (on lui ajoute G[i])
        Resultat[i]  <- FALSE                                 #on assigne au vecteur Resultat en position i la valeur F
      }
      i=i+1                                                   #cela signifie que le while devra de nouveau boucler pour le i suivant
    }
    
    Diff <- abs(GS1-GS2)                                      #diff�rence des 2 paquets (>0) que l'on va tester
    
    #on test:
    
    if (Diff == 0){                                           #trie parfait !
      Resultat[ordre]=Resultat
                                                              #on remet les T/F dans l'ordre initial par rapport � g
      return(Resultat)                                        #on retourne le vecteur bool�en
    }
    if (Diff == 1){                                           #de m�me pour celui-ci...
      Resultat[ordre]=Resultat
      print(Diff)
      return(Resultat)
    }
    else {                                                    #sinon on applique mtgs:
      
#GS AVEC DECALAGE
      
    for (n in 1:length(g)){
        
      newGS1 <- 0                                             #nouvelle somme du 1er paquet
      newGS2 <- sum(as.numeric(G[1:n]))                       #nouvelle somme du 2nd paquet
      newResultat <- logical(length(g))                       #nouveau r�sultat (vide pour l'instant)
      newResultat[ordre[1:n]] <- FALSE                        #on d�finie resultat tel que les n premiers �l�ments seront FALSE
      i=n+1                                                   #comme on cherche T/F que pour les autres �l�ments du vecteur alors i=n+1
        
        
      while (i <= length(g)){                                 #GS pour les �l�ments restant...
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
        
      newDiff <- abs((newGS1)-(newGS2))                       #� l'issue du n �me d�calage on a une nouvelle diff�rence (>0) que l'on va tester
        
    #Nouveau tests:        
        
         
      if (newDiff < Diff){                                    #si la nouvelle diff�rence < diff�rence alors le trie est + satisfaisant donc:
          Diff <- newDiff                                     #diff�rence prendra la valeur de la nouvelle diff�rence trouv�
          Resultat <- newResultat                             #resultat prendra les valeurs du nouveau r�sultat trouv�
      }
      if (Diff == 0){                                         #si on a un trie parfait...
          Resultat[ordre]=Resultat
          
          return(Resultat)
      }
      if (Diff == 1){                                         #si on a un trie parfait...
          Resultat[ordre]=Resultat
        
          return(Resultat)
      }
      else{                                                   #est-ce qu'il peut y avoir un meilleur trie possible lors du prochain d�calage ?
      }
                                                              #on reboucle dans for, etc ...
        
    }     
                                                              #si � l'issue du n �me d�calage on a pas de trie 'parfait' alors on a retenue (grace au test dans for) la meilleure diff�rence des 2 paquets
    Resultat[ordre]=Resultat                              
    return(Resultat)                                          #alors on retourne le meilleur r�sultat de mtgs
    }
  }
}

