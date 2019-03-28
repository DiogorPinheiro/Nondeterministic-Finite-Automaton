open Printf
open Scanf
open List

(* --------------  Leitura e armazenamento de dados  -------------------------------*)
let n = scanf " %d" (fun n -> n)                    (* Número de estados *)

let card_so = scanf " %d" (fun card_so -> card_so)  (* Cardinalidade de estados iniciais *)
let so = []                                         (* Lista de estados iniciais  (estado, valor interno) *)

let rec leitura_so so card_so =                     (* Leitura dos estados iniciais*)
   if card_so = 0 then
    so
  else
    let x = scanf " %d" (fun x -> x) in
    let so = so@[(x,0)] in
   leitura_so so (card_so-1)

let so = leitura_so [] card_so 

let card_f = scanf " %d" (fun card_f -> card_f)     (* Cardinalidade de estados finais *)

let rec leitura_f f card_f =                        (* Leitura dos estados finais *)     
  if card_f = 0 then
   f
 else
   let x = scanf " %d" (fun x -> x) in
   let f = f@[x] in
  leitura_f f (card_f-1)

let f = leitura_f [] card_f 

let card_trans = scanf " %d" (fun card_trans -> card_trans) (* Número de transições *)
let transicoes = []

let rec armazenar_transicoes transicoes card_trans = (* Armazenar Transições *)
    if card_trans > 0 then
        let a1,a2,a3,a4,a5,a6 = scanf " %d %c %s %c %c %d " (fun a1 a2 a3 a4 a5 a6 -> a1,a2,a3,a4,a5,a6) in 
        let a4 = if a4 = '_' then (-1) else (int_of_char a4 - 48) in (* Converter para decimal *)
        let a5 = if a5 = '_' then (-1) else (int_of_char a5 - 48) in
        let x = (a1,a2,a3,a4,a5,a6) in 
        let transicoes = transicoes @ [x] in 
        armazenar_transicoes transicoes (card_trans-1) 
    else transicoes
     
let transicoes  = armazenar_transicoes transicoes card_trans
let palavra = scanf " %s" (fun palavra -> palavra) (* String final *)
let length = String.length palavra  (*Nº de caracteres na palavra *)
(*--------------------------------------------------------------------------------------------*)

let rec num_vizinhosepsilon v1 transicoes cont = (* Encontrar número de vizinhos epsilon *)
   match transicoes with
      | [] -> cont
      | (a1,a2,a3,a4,a5,a6)::tl -> if a1 = v1 && a2 = '_' then num_vizinhosepsilon v1 tl (cont+1) else num_vizinhosepsilon v1 tl cont

let rec is_estadofinal estadofinal estado = (* Verificar se é estado final (se pertence à lista de estados finais)*)
    match estado with
      | [] -> false
      | (v1,v2)::tl -> if (List.mem v1 estadofinal) then true else is_estadofinal estadofinal tl
 
let conversao_booleano operacao a b = (*Converter operacao por booleano -> Evitar caso != que nao tem significado desejado em ocaml*)
    match operacao with
    | "_"  -> true
    | "!=" -> a <> b
    | "<"  -> a < b
    | ">"  -> a > b
    | "="  -> a = b
    | "<=" -> a <= b
    | ">=" -> a >= b
    | _ -> false

let rec comparar_estados estado anterior =      (* Comparar lista de estados atuais com a lista de estados anteriores *)
  match estado with
  | [] -> true
  | (v1,v2)::resto -> if (List.mem (v1,v2) anterior) then comparar_estados resto anterior else false

(* ---------------------Percorrer transições epsilon ----------------------------- *)
let rec distribuicao_vizinhos estado anterior transicao =   (* Função para controlar quando chega ao último epsilon *)
    let estado = obter_estadoepsilon estado [] transicao in
    let () = print_estados anterior in 
      if comparar_estados estado anterior = false then (* Se o estado anterior e o atual forem diferentes *) 
        let anterior = estado in distribuicao_vizinhos estado anterior transicao
      else 
        estado

and obter_estadoepsilon estado vizinhos transicoes =  
  match estado with 
  | []->  vizinhos
  | (v1,v2)::resto -> let vizinhos = if (List.mem (v1,v2) vizinhos) then vizinhos else vizinhos@[(v1,v2)] in
    let vizinhos = transicao_epsilon v1 v2 transicoes vizinhos in 
    obter_estadoepsilon resto vizinhos transicoes 

and transicao_epsilon v1 v2 transicao vizinhos =  (* Analisar todas as transições para o estado recebido *)
  match transicao with 
    | []->  vizinhos
    | (a1,a2,a3,a4,a5,a6)::restote -> 
      let vizinhos = if( a2 = '_') && v1 = a1   (* Caso seja epsilon *)
        then 
          if a5=(-1) then                       (* Caso não tenha valor a atualizar*)
            if (List.mem (a6,v2) vizinhos ) then
              vizinhos
            else
              vizinhos@[(a6, v2)] 
          else 
            if (List.mem (a6,a5) vizinhos) then
              vizinhos
            else
              vizinhos@[(a6,a5)]
        else  
          vizinhos 
      in transicao_epsilon v1 v2 restote vizinhos 
                           
(*------------------------ Obter caracteres ----------------------------------------*)        
let rec obter_estado estado vizinhos transicoes palavra v3 =
  match estado with
  | []-> vizinhos
  | (v1,v2)::resto -> 
    let vizinhos = transicao_possivel vizinhos transicoes v1 v2 v3 palavra in 
    obter_estado resto vizinhos transicoes palavra v3
  
and transicao_possivel vizinhos transicao v1 v2 v3 palavra = (* Obter transições com caracter *)
  match transicao with 
    | [] -> vizinhos
    | (a1,a2,a3,a4,a5,a6)::resto -> 
      let vizinhos =  if v1 = a1 && (String.get palavra v3) = a2 && conversao_booleano a3 v2 a4   (* String.get palavra v3 -> Ir à posicao v3 da palavra e verificar se a transição tem o caracter pretendido*)
        then  (* Verificar condições para poder usar transição*) 
          if a5 = (-1) then           (* Se não tiver nenhum valor para atualizar *)
            vizinhos@[(a6, v2)]
          else
            vizinhos@[(a6, a5)]  
        else 
          vizinhos 
      in transicao_possivel vizinhos resto v1 v2 v3 palavra
             
(*------------------------------------------------------------------------------ *)

let rec main palavra estado transicoes length estadofinal v3 =  (* Obter epsilons e depois caracteres até o index v3 ser do comprimento da palavra ou a lista de estados estar vazia*)
  match estado with                                             (* v3 -> index representativo do caracter da palavra*)
  | [] -> let estado = distribuicao_vizinhos estado estado transicoes in 
          is_estadofinal estadofinal estado
  | _ -> if v3 = length then
          let estado = distribuicao_vizinhos estado estado transicoes in 
          is_estadofinal estadofinal estado
        else    
          let estado = distribuicao_vizinhos estado estado transicoes in 
          let estado = obter_estado estado [] transicoes palavra v3 in
          main palavra estado transicoes length estadofinal (v3+1)

(* Obter resposta final ao problema *)         
let () = if main palavra so transicoes length f 0 then printf "YES\n" else printf "NO\n" 

(* Funcões de leitura adicionais (para testes) *)
(*
let rec print_arr lis =
  match lis with
  | [] -> printf "\n"
  | h::t -> printf "%d " h ;print_arr t
let () = print_arr so  
*)
(*
let rec print_transicoes lista =
    match lista with
    | [] -> printf "\n"
    | (a1,a2,a3,a4,a5,a6)::resto -> let () = printf "(%d, %c, %s, %d, %d, %d)\n" a1 a2 a3 a4 a5 a6 in print_transicoes resto
let () = print_transicoes transicoes *)

(*
let rec print_estados estado =
  match estado with
  | []->printf "\n"
  | (v1,v2)::resto -> let () = printf "(%d)\n" v1 v2 in print_estados resto 
*)

    

