

type candidat = {
  nom : string;
  mutable voeux : (int option * string) list;
}

type formation = {
  nom : string;
  capacite : int;
  appel : candidat list;
  eligible : candidat list;
}



type session = {
  mutable nb_candidats : int;
  mutable candidats : candidat list;
  mutable nb_formations : int;
  mutable formations : formation list;
}


let nouvelle_session () = {nb_candidats = 0; candidats = []; nb_formations = 0; formations = []}

let ajoute_candidat session ~nom_candidat = session.candidats <- {nom = nom_candidat; voeux = []} :: session.candidats

let ajoute_formation session ~nom_formation ~capacite = session.formations <- {nom = nom_formation; capacite = capacite; appel = []; eligible = []} :: session.formations

let ajoute_voeu session ~rang_repondeur ~nom_candidat ~nom_formation = 
  ignore session;
  ignore rang_repondeur;
  ignore nom_candidat;
  ignore nom_formation;
  failwith "non implémenté"
  (*
  let rec recuperer_candidat l = match l with
    | [] -> []
    | hd :: tl -> recuperer_candidat tl  
  in

  let test = recuperer_candidat session.candidats in

  test.voeux <- (Some rang_repondeur,nom_formation) test
  *)

let ajoute_commission session ~nom_formation ~fonction_comparaison = 
  ignore session;
  ignore nom_formation;
  ignore fonction_comparaison;
  failwith "non implémenté"

let reunit_commissions session =
  ignore session;
  failwith "non implémenté"

let nouveau_jour session =
  ignore session;
  failwith "non implémenté"

let renonce session ~nom_candidat ~nom_formation = 
  ignore session;
  ignore nom_candidat;
  ignore nom_formation;
  failwith "non implémenté"

let consulte_propositions session ~nom_candidat =
  ignore session;
  ignore nom_candidat;
  failwith "non implémenté"

let consulte_voeux_en_attente session ~nom_candidat = 
  ignore session;
  ignore nom_candidat;
  failwith "non implémenté"
