type couleur = Trefle | Carreau | Coeur | Pique

type valeur = Num of int | Valet | Reine | Roi

type carte = Carte of couleur * valeur

type paquet = carte list

let lt_couleur = [ Trefle; Carreau; Coeur; Pique ]

let cartesian_product l1 l2 = 

let paquet1 = cartesian_product lt_couleur 
