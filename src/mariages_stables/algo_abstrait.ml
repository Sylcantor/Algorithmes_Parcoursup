open Definitions

module type PIOCHE = sig
  type 'a t
    val of_list: 'a list -> 'a t 
    val pioche : 'a t -> 'a option 
     val defausse : 'a -> 'a t -> unit 
end



module Pile : PIOCHE = struct
  
  type 'a t = {mutable liste : 'a list} (* <- à modifier *)

  let of_list l = { liste = l}

  let pioche p = 
    
    let test p = match p.liste with
    | [] -> None
    | hd :: _ -> Some hd
    in

    let hd = test p in

    if p.liste <> [] then p.liste <- List.tl p.liste;
    hd

  let defausse x p = p.liste <- x :: p.liste
    
end



module File : PIOCHE = struct
  
  type 'a t = {mutable liste : 'a list} (* <- à modifier *)

  let of_list l = {liste = l}

  let pioche p =
     let test p = match p.liste with
    | [] -> None
    | hd :: _ -> Some hd
    in

    let hd = test p in

    if p.liste <> [] then p.liste <- List.tl p.liste;
    hd

  let defausse x p = p.liste <- p.liste @ x::[]

end




module Algo(P:PIOCHE) = struct
  
  let run entree = 
    
    let n = entree.n in
    
    let new_conf = {
      rang_appel_de = Array.make n 0;
      fiance_de = Array.make n None
    } in


    
    let p = P.of_list (List.init n (fun i -> i)) in

    let y = ref (P.pioche p) in (* Correspond à X *)

    let x = ref 0 in

    let gagnant = ref None in
    let perdant = ref 0 in
    
    let int_of_intoption = function None -> -1 | Some n -> n in
    
    while !y != None do

      x := entree.liste_appel_de.((int_of_intoption !y)).(new_conf.rang_appel_de.((int_of_intoption !y)));

      if new_conf.fiance_de.(!x) == None then new_conf.fiance_de.(!x) <- !y
      else
      begin
        if (entree.prefere.(!x) (int_of_intoption !y) (int_of_intoption new_conf.fiance_de.(!x))) then
        begin
          gagnant:= !y;
          perdant:= (int_of_intoption new_conf.fiance_de.(!x))
        end
        else
        begin
          gagnant:= new_conf.fiance_de.(!x);
          perdant := (int_of_intoption !y)
        end;

        new_conf.fiance_de.(!x) <- !gagnant;
        new_conf.rang_appel_de.(!perdant) <- new_conf.rang_appel_de.(!perdant) + 1;

        P.defausse !perdant p;
      end;
      y := P.pioche p;
    done;

    List.init n (fun i -> ((int_of_intoption new_conf.fiance_de.(i)), i)); (* la sortie *)
end
