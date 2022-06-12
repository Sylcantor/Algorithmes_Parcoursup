open Definitions

let algo ?(affiche_config=false) entree =
  ignore affiche_config;
  (* ignore entree;
  failwith "non implémenté"
  *)

  let n = entree.n in

  let new_conf = {
    rang_appel_de = Array.make n 0;
    fiance_de = Array.make n None
  } in

  let x = ref 0 in

  let y = ref 0 in (* GRAND X*)

  let est_celibataire configuration = 
    let bool = ref false in
    let i = ref 0 in
    while !bool == false && !i < n do
      if configuration.fiance_de.(!i) == None then bool := true;
      i := !i +1;
    done;
    !bool;
  in

  let liste_des_pretendants = Array.make_matrix n n (-1) in

  let homme_est_celibataire x =
    let bool = ref true in
    let i = ref 0 in

    while !bool == true && !i < n do
      if new_conf.fiance_de.(!i) == Some x then bool := false;
      i := !i +1;
    done;
    !bool;
  in

  let femme_a_proposition x =
    let bool = ref false in
    let i = ref 0 in
    while !bool == false && !i < n do
      if liste_des_pretendants.(x).(!i) != (-1) then bool := true;
      i := !i +1;
    done;
    !bool
  in

  let preference_femme liste_pretendant_de_x x =
    let homme = ref (-1) in
    for i = 0 to n - 1 do
      if !homme == (-1) && liste_pretendant_de_x.(i) != (-1) then homme := i
      else if liste_pretendant_de_x.(i) != (-1) && entree.prefere.(x) i !homme then homme := i;
    done;
    !homme
  in

  let int_of_intoption = function None -> -1 | Some n -> n in

  let test = ref est_celibataire in


  while est_celibataire new_conf do

    for i = 0 to n-1 do
      
      if homme_est_celibataire i then
      begin
        x := entree.liste_appel_de.(i).(new_conf.rang_appel_de.(i));
        liste_des_pretendants.(!x).(i) <- 1;
      end;
    done;
    
    for i = 0 to n-1 do
      if femme_a_proposition i then
      begin
        y := preference_femme liste_des_pretendants.(i) i;
        if (new_conf.fiance_de.(i) == None) || (entree.prefere.(i) !y (int_of_intoption new_conf.fiance_de.(i))) then
        begin
          if new_conf.fiance_de.(i) != None then new_conf.rang_appel_de.((int_of_intoption new_conf.fiance_de.(i))) <- new_conf.rang_appel_de.((int_of_intoption new_conf.fiance_de.(i))) + 1;
          new_conf.fiance_de.(i) <- Some !y;
        end;

        for u = 0 to n-1 do
          if liste_des_pretendants.(i).(u) != (-1) && ((int_of_intoption new_conf.fiance_de.(i)) != u) then new_conf.rang_appel_de.(u) <- new_conf.rang_appel_de.(u) + 1;
        done;
        (* tab à -1 *)
        for u = 0 to n-1 do
          liste_des_pretendants.(i).(u) <- (-1)
        done;
      end;
      
    done;
    if affiche_config then print_configuration new_conf;
    test := est_celibataire;
  done;
  let int_of_intoption = function None -> -1 | Some n -> n in

  List.init n (fun i -> ((int_of_intoption new_conf.fiance_de.(i)), i));
