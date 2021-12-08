type categorie = Chien | Chat

type animal = { nom_a : string; genre : categorie }

type personne = { nom_p : string; prenom : string }

type ville = { nom_c : string; code_postal : int }

val animaux : animal list

val proprietaires : (animal * personne) list
val adresses : (personne * ville) list

val find_animal_by_name : string -> animal option
val find_adresse : string -> ville option