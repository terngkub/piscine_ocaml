module Card = struct

  module Color = struct

    type t = Spade | Heart | Diamond | Club


    let all = [Spade; Heart; Diamond; Club]


    let toString color =
      match color with
      | Spade   -> "S"
      | Heart   -> "H"
      | Diamond -> "D"
      | Club    -> "C"


    let toStringVerbose color =
      match color with
      | Spade   -> "Spade"
      | Heart   -> "Heart"
      | Diamond -> "Diamond"
      | Club    -> "Club"

  end


  module Value = struct

    type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As


    let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]


    let toInt value =
      match value with
      | T2    -> 1
      | T3    -> 2
      | T4    -> 3
      | T5    -> 4
      | T6    -> 5
      | T7    -> 6
      | T8    -> 7
      | T9    -> 8
      | T10   -> 9
      | Jack  -> 10
      | Queen -> 11
      | King  -> 12
      | As    -> 13


    let toString value =
      match value with
      | T2    -> "2"
      | T3    -> "3"
      | T4    -> "4"
      | T5    -> "5"
      | T6    -> "6"
      | T7    -> "7"
      | T8    -> "8"
      | T9    -> "9"
      | T10   -> "10"
      | Jack  -> "J"
      | Queen -> "Q"
      | King  -> "K"
      | As    -> "A"


    let toStringVerbose value =
      match value with
      | T2    -> "2"
      | T3    -> "3"
      | T4    -> "4"
      | T5    -> "5"
      | T6    -> "6"
      | T7    -> "7"
      | T8    -> "8"
      | T9    -> "9"
      | T10   -> "10"
      | Jack  -> "Jack"
      | Queen -> "Queen"
      | King  -> "King"
      | As    -> "As"


    let next value =
      match value with
      | T2    -> T3
      | T3    -> T4
      | T4    -> T5
      | T5    -> T6
      | T6    -> T7
      | T7    -> T8
      | T8    -> T9
      | T9    -> T10
      | T10   -> Jack
      | Jack  -> Queen
      | Queen -> King
      | King  -> As
      | As    -> invalid_arg "As don't have next value"


    let previous value =
      match value with
      | T2    -> invalid_arg "T2 don't have previous value"
      | T3    -> T2
      | T4    -> T3
      | T5    -> T4
      | T6    -> T5
      | T7    -> T6
      | T8    -> T7
      | T9    -> T8
      | T10   -> T9
      | Jack  -> T10
      | Queen -> Jack
      | King  -> Queen
      | As    -> King

  end


  type t = {
    value : Value.t;
    color : Color.t
  }


  let newCard value color =
    {
      value = value;
      color = color
    }


  let allSpades =
    let new_spade value =
      newCard value Color.Spade
    in
    List.map new_spade Value.all 


  let allHearts =
    let new_heart value =
      newCard value Color.Heart
    in
    List.map new_heart Value.all


  let allDiamonds =
    let new_diamond value =
      newCard value Color.Diamond
    in
    List.map new_diamond Value.all


  let allClubs =
    let new_club value =
      newCard value Color.Club
    in
    List.map new_club Value.all


  let all = allSpades @ allHearts @ allDiamonds @ allClubs


  let getValue (card:t) = card.value


  let getColor (card:t) = card.color


  let toString (card:t) =
    Printf.sprintf "%s%s" (Value.toString card.value) (Color.toString card.color)


  let toStringVerbose (card:t) =
    Printf.sprintf "Card(%s, %s)" (Value.toStringVerbose card.value) (Color.toStringVerbose card.color)


  let compare (one:t) (two:t) =
    if one > two then 1
    else if one < two then (-1)
    else 0


  let max (one:t) (two:t) =
    if compare one two >= 0 then one
    else two


  let min (one:t) (two:t) =
    if compare one two < 0 then one
    else two


  let best (lst:t list) =
    match lst with
    | [] -> invalid_arg "Error: empty list"
    | head :: [] -> head
    | head :: tail -> List.fold_left max head tail


  let isOf card color =
    if card.color = color then true
    else false
    

  let isSpade card =
    if card.color = Spade then true
    else false


  let isHeart card =
    if card.color = Heart then true
    else false


  let isDiamond card =
    if card.color = Diamond then true
    else false


  let isClub card =
    if card.color = Club then true
    else false

end


type t = Card.t list


exception Failure of string


let newDeck () :t =
  let random_list =
    Random.self_init ();
    let create_random_pair elem = (Random.bits (), elem) in
    List.map create_random_pair Card.all
  in
  let sorted = List.sort compare random_list in
  List.map snd sorted


let toStringList (deck:t) =
  List.map Card.toString deck


let toStringListVerbose (deck:t) =
  List.map Card.toStringVerbose deck


let drawCard (deck:t) :Card.t * t =
  match deck with
  | [] -> raise (Failure "deck is empty")
  | head :: tail -> (head, tail)