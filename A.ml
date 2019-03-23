open Printf
open Scanf
open List

(* --------------  Leitura e armazenamento de dados  -------------------------------*)
let n = scanf " %d" (fun n -> n)                    (* Número de estados *)

let card_so = scanf " %d" (fun card_so -> card_so)  (* Cardinalidade de estados iniciais *)
let so = []                                         (* Lista de estados iniciais  (estado, valor interno, posicao do caracter desejado na palavra) *)
let x,y,z = scanf " %d %d %d" (fun x y z -> x, y, z) 
let so = so @ [(x,0,0)]
let so = so @ [(y,0,0)]
let so = so @ [(z,0,0)]

let card_f = scanf " %d" (fun card_f -> card_f)     (* Cardinalidade de estados finais *)
let f = []                                          (* Lista de estados finais *)

let x1,y1,z1 = scanf " %d %d %d" (fun x1 y1 z1 -> x1, y1, z1) 
let f = f @ [x1]
let f = f @ [y1]
let f = f @ [z1]

let card_trans = scanf " %d" (fun card_trans -> card_trans) (* Número de transições *)
let transicoes = []

let rec armazenar_transicoes transicoes card_trans = (* Armazenar Transições *)
    if card_trans > 0 then
        let a1,a2,a3,a4,a5,a6 = scanf " %d %c %c %c %c %d " (fun a1 a2 a3 a4 a5 a6 -> a1,a2,a3,a4,a5,a6) in 
        let a4 = if a4 = '_' then (-1) else (int_of_char a4 - 48) in (* Converter para decimal *)
        let a5 = if a5 = '_' then (-1) else (int_of_char a5 - 48) in
        let x = (a1,a2,a3,a4,a5,a6) in 
        let transicoes = transicoes @ [x] in 
        armazenar_transicoes transicoes (card_trans-1) 
    else transicoes
     
let transicoes = armazenar_transicoes transicoes card_trans
let palavra = scanf " %s" (fun palavra -> palavra) (* String final *)
let length = String.length palavra  (*Nº de caracteres na palavra *)
(*--------------------------------------------------------------------------------------------*)

let rec num_vizinhos num transicoes cont = (* Encontrar número de vizinhos *)
  match transicoes with
       [] -> cont
       |(a1,a2,a3,a4,a5,a6)::tl -> if a1 = num then num_vizinhos num tl (cont+1) else num_vizinhos num tl cont

let rec is_estadofinal estadofinal estado = (* Verificar se é estado final (se pertence à lista de estados finais)*)
    match estado with
      |[] -> false
      |(v1,v2,v3)::tl -> if (List.mem v1 estadofinal) then true else is_estadofinal estadofinal tl
 
let conversao_booleano operacao a b = (*Converter operacao por booleano -> Evitar caso != que nao tem significado desejado em ocaml*)
    match operacao with
    | "_" -> true
    | "!=" -> a <> b
    | "<" -> a < b
    | ">" -> a > b
    | "=" -> a = b
    | "<=" -> a <= b
    | ">=" -> a>=b
    | _ -> false
(* ---------------------Percorrer transições epsilon ----------------------------- *)
let rec obter_estadoepsilon estado vizinhos transicoes =
    match estado with 
    |[]-> vizinhos
    |(v1,v2,v3)::resto -> let vizinhos = if (List.mem (v1,v2,v3) estado) then vizinhos else vizinhos@[(v1,v2,v3)] in
    transicao_epsilon v1 v2 v3 transicoes vizinhos resto

let rec transicao_epsilon v1 v2 v3 transicao vizinhos restor =  (* Obter transições epsilon *)
    match transicao with 
      | []-> vizinhos
      | (a1,a2,a3,a4,a5,a6)::resto -> let vizinhos =
        if( a3 = '_') && v1 = a1 then 
          if a5=(-1) then vizinhos@[(v1, v2, v3)] else vizinhos
        else  
          vizinhos in transicao_epsilon v1 v2 v3 resto vizinhos restor 
        let vizinhos = transicao_epsilon v1 v2 v3 transicao vizinhos restor in obter_estadoepsilon restor vizinhos transicao
      
let rec distribuicao_vizinhos estado anterior transicao =
    let estado = obter_estadoepsilon estado [] transicao
    if estado <> anterior then let anterior = estado in distribuicao_vizinhos estado anterior transicao
    else estado

(*------------------------ Obter caracteres ----------------------------------------*)        
let rec obter_estado estado vizinhos transicoes palavra =
  match estado with
  |[]-> vizinhos
  |(v1,v2,v3)::resto -> transicao_possivel vizinhos transicao v1 v3 palavra resto
  
let rec transicao_possivel vizinhos transicao v1 v3 palavra restor= (* Obter transições com caracter *)
    match transicao with 
      | [] -> vizinhos
      | (a1,a2,a3,a4,a5,a6)::resto -> let vizinhos = 
        if v1 = a1 && (String.get palavra v3) = a2 && conversao_booleano a3 v2 a4 then  (* Verificar condições para poder usar transição*) 
          if a5 = (-1) then
            vizinhos@[(v1, v2, v3+1)]
          else
            vizinhos@[(v1, a5, v3+1)]  
        else 
          vizinhos in transicao_possivel vizinhos transicao v1 v3 palavra restor
      in 
        let vizinhos = transicao_possivel vizinhos transicao v1 v3 palavra restor in
        obter_estado restor vizinhos transicao palavra restor
        
(*------------------------------------------------------------------------------ *)

let rec main palavra estado transicoes length estadofinal =
  let (v1,v2,v3) = estado in  
  (*Usar funcao obter estado *)
  if v3 = length || estado = [] then
      let estado = transicao_Epsilon estado transicoes estado in 
      is_estadofinal estadofinal estado
  else
      let estado = transicao_epsilon estado transicoes estado in
      let estado = obter_estado estado [] transicoes palavra [] in
      main palavra estado transicoes length estadofinal 

(* Obter resposta final ao problema *)         
let () = if main palavra so transicoes length f then printf "YES\n" else printf "NO\n" 


(* Funcões de leitura adicionais (para testes) *)
(*
let rec print_arr lis =
  match lis with
  | [] -> printf "\n"
  | h::t -> printf "%d " h ;print_arr t
let () = print_arr so  
*)
(*
let rec print_list lista =
    match lista with
    | [] -> printf "\n"
    | (a1,a2,a3,a4,a5,a6)::resto -> let () = printf "(%d, %c, %c, %d, %d, %d)\n" a1 a2 a3 a4 a5 a6 in print_list resto
let () = print_list transicoes *)
    

