
(* Types *)
type feedback = int list


(* Fonctions *)
let rec get_lignes canal ret = 
    
    try
        let current_line = input_line canal in
        get_lignes canal (current_line :: ret)
    with
        | End_of_file -> begin 
            close_in canal; 
            ret 
          end


let feedback_giver essai cible = 
    let rec green_check essai cible indice fb =
        let new_fb =
            if essai.[indice] = cible.[indice] then
                2 :: fb
            else
                0 :: fb
        in

        if indice > 0 then
            green_check essai cible (indice - 1) new_fb
        else
            new_fb
    in

    let rec remaining_letters mot fb lst indice =
        match fb with
            | x :: ll -> begin
                let new_lst = 
                    if x = 0 then mot.[indice] :: lst
                    else lst
                in
                remaining_letters mot ll new_lst (indice + 1)
                end
            | [] -> lst
    in

    let rec yellow_check essai reste_cible old_fb new_fb indice = 

        let rec remove_from_lst to_rem lst =
            match lst with
                | x :: ll when x = to_rem -> ll
                | x :: ll -> x :: remove_from_lst to_rem ll
                | [] -> []

        in

        match old_fb with
            | elem :: nxt_old_fb ->
                if elem = 2 then 
                    yellow_check essai reste_cible nxt_old_fb (2 :: new_fb) (indice - 1)
                else
                    if List.mem essai.[indice] reste_cible then
                        yellow_check essai (remove_from_lst essai.[indice] reste_cible) nxt_old_fb (1 :: new_fb) (indice - 1)
                    else
                        yellow_check essai reste_cible nxt_old_fb (0 :: new_fb) (indice - 1)
            | [] -> new_fb
    in

    let first_fb = green_check essai cible 4 [] in

    let lettres_rest = remaining_letters cible first_fb [] 0 in

    (* On retourne le first_fb car on parcourt notre liste a l'envers. *)
    let final_fb = yellow_check essai lettres_rest (List.rev first_fb) [] 4 in

    final_fb        


let rec filtrage essai lst_mots expected_fb =
    let rec filtrage_all lst_mots expected_fb new_lst =
        match lst_mots with
            | mot :: lst -> 
                if (feedback_giver essai mot) = expected_fb then filtrage_all lst expected_fb (mot :: new_lst)
                else filtrage_all lst expected_fb new_lst
            | [] -> new_lst
    in
    filtrage_all lst_mots expected_fb []



let ajouter_feedback fb dico_fb_occ =
    let rec incr_occ fb dico_fb_occ fin =
        match dico_fb_occ with
            | t1 :: suite when (fst t1) = fb -> incr_occ fb suite ((fb, (snd t1) + 1) :: fin)
            | t1 :: suite -> incr_occ fb suite (t1 :: fin)
            | [] -> fin
    in

    if List.mem_assoc fb dico_fb_occ then
        incr_occ fb dico_fb_occ []
    else
        (fb, 1) :: dico_fb_occ
    

let regrouper_feedback essai lst_mots =
    let rec gestion essai lst_mots fbs =
        match lst_mots with
            | mot :: suite -> begin    
                let fb = feedback_giver essai mot in
                gestion essai suite (ajouter_feedback fb fbs)
              end
            | [] -> fbs
    in
    
    gestion essai lst_mots []


let proba nb_occ total =
    float_of_int nb_occ /. float_of_int total 


let terme_shannon prob =
    let log2 x = log x /. log 2.0 in
    -.prob *. (log2 prob)



let rec taille_lst lst acc =
    match lst with
        | elt :: suite -> taille_lst suite (acc + 1)
        | [] -> acc


let entropie distri nb_elems =
    let rec calc_entropie distri nb_elems acc = 
        match distri with
            | fb_occ :: suite -> begin
                let p = proba (snd fb_occ) nb_elems in
                calc_entropie suite nb_elems (acc +. (terme_shannon p))
            end
            | [] -> acc
    in

    calc_entropie distri nb_elems 0.


let entropie_essai mot lst_mots =
    let gp = regrouper_feedback mot lst_mots in
    entropie gp (taille_lst lst_mots 0)



let evaluer_mots lst_mots =
    let rec evaluer_mot_aux lst_mots lst_mots_complete fin =
        match lst_mots with
            | mot :: suite -> begin
                Printf.printf "Calcul entropie : %s\n" mot;
                evaluer_mot_aux suite lst_mots_complete ((mot, entropie_essai mot lst_mots_complete) :: fin)
            end    
            | [] -> fin
    in

    evaluer_mot_aux lst_mots lst_mots []



let recup_meilleur_mot lst_mot_entrop =
    let rec recup_meilleur_mot_aux lst_mot_entrop best max_entropie =
        match lst_mot_entrop with
            | cpl :: suite -> 
                if snd cpl > max_entropie then recup_meilleur_mot_aux suite (fst cpl) (snd cpl)
                else recup_meilleur_mot_aux suite best max_entropie
            | [] -> (best, max_entropie)
    in

    recup_meilleur_mot_aux lst_mot_entrop (fst (List.hd lst_mot_entrop)) (snd (List.hd lst_mot_entrop)) 
    
let feedback_of_str str =
    let rec feedback_of_str_aux str fb indice =
        if indice >= 0 then
            feedback_of_str_aux str (int_of_string (String.make 1 str.[indice]) :: fb) (indice - 1)
        else fb
    in

    feedback_of_str_aux str [] 4


let rec tour lst_mot =
    let evaluations = evaluer_mots lst_mot in
    let meilleur = recup_meilleur_mot evaluations in
    let () = Printf.printf "Mot proposé : %s\n" (fst meilleur) in
    let () = Printf.printf "Entrez le feedback donné pour ce mot :" in
    let fdb = read_line () in
    if fdb = "22222" then Printf.printf "Gagné !"
    else
        let parsed = feedback_of_str fdb in
        let new_lst = filtrage (fst meilleur) lst_mot parsed in
        if new_lst = [] then
            Printf.printf "Aucun mot ne correspond au feedback !\n"
        else
            tour new_lst



(* Execution *)
let mots_file = open_in "../data/mots.txt";;
let liste_mots = get_lignes mots_file [];;

let () = Printf.printf "Entrez votre premier mot :\n"
let mot_entre = read_line ();;
let () = Printf.printf "Entrez le feedback associé :\n"
let fb = read_line ();;
let parsed = feedback_of_str fb;;
let new_lst = filtrage mot_entre liste_mots parsed;;
Printf.printf "Nombre de candidats : %d\n" (taille_lst new_lst 0);;

tour new_lst;;

(*
(* Tests *)
let () = Printf.printf "Nombre de mots : %d\n" (List.length liste_mots)

let tests = [
    ("POMME", "POMME", [2; 2; 2; 2; 2]);
    ("POMME", "POMPA", [2; 2; 2; 0; 0]);
    ("POMME", "XXXXX", [0; 0; 0; 0; 0]);
    ("POMME", "MPOME", [1; 1; 1; 2; 2]);
    ("POMME", "MAMIE", [0; 0; 2; 1; 2]);
    ("POMME", "PAPAL", [2; 0; 0; 0; 0]);
    ("POMME", "MMMMM", [0; 0; 2; 2; 0]);
    ("POMME", "EMMMM", [0; 0; 2; 2; 1]);
]
;;

List.iter (fun (essai, cible, attendu) ->
    let obtenu = feedback_giver essai cible in
    Printf.printf "%s vs %s : %s\n"
        essai cible
        (if obtenu = attendu then "OK" else "ÉCHEC")
) tests

let mots = ["POMME"; "POMPA"; "MPOME"; "MAMIE"; "PAPAL"; "XXXXX"]
let essai = "POMME"
let expected_fb = [2; 2; 2; 0; 0]

let test_filtrage =
    filtrage essai mots expected_fb

let () = 
    List.iter print_endline test_filtrage


let distribution = [
    ([2; 2; 2; 2; 2], 3);
    ([0; 0; 0; 0; 0], 1)
]

let test = entropie distribution 4

let () = Printf.printf "Entropie : %f bits\n" test
*)