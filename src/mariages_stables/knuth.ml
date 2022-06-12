open Definitions

let algo ?(affiche_config=false) entree =

  let n = entree.n in
  let k = ref 0 in

  let y = ref 0 in
  let omega = None in

  let new_conf = {
    rang_appel_de = Array.make n 0;
    fiance_de = Array.make n omega
  } in
 
  let x = ref 0 in
  
  let fiance_de_x = ref 0 in  

  let y_tmp = ref 0 in

  let int_of_intoption = function None -> -1 | Some n -> n in


  while !k < n   do
    y := !k;

    while !y <> (int_of_intoption omega) do
      
      x := entree.liste_appel_de.(!y).(new_conf.rang_appel_de.(!y));

      fiance_de_x := (int_of_intoption (new_conf.fiance_de.(!x)));

      if (!fiance_de_x == (int_of_intoption omega)) || (entree.prefere.(!x) !y !fiance_de_x) then
      begin
        y_tmp := (int_of_intoption new_conf.fiance_de.(!x));
        
        new_conf.fiance_de.(!x) <- Some !y;
        y := !y_tmp
      end;
      if !y <> (int_of_intoption omega) then new_conf.rang_appel_de.(!y) <- new_conf.rang_appel_de.(!y) + 1;
      
      (* afficher config courante *)
      if affiche_config then print_configuration new_conf;
    done;
    k := !k + 1;
  done;

  List.init n (fun i -> ((int_of_intoption new_conf.fiance_de.(i)), i));
;;