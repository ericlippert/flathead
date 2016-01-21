(* Simplified version of Chris Okasaki's deque. *)
type 'a t =
{
  front : 'a list;
  front_length : int;
  back : 'a list;
  back_length : int
}

(* Invariants: front_length and back_length are the lengths of the lists *)
(* Invariants: front_length <= c * back_length + 1 *)
(* Invariants: back_length <= c * front_length + 1 *)

exception Empty
exception InvalidIndex

let empty =
{
  front = [];
  front_length = 0;
  back = [];
  back_length = 0
}

let is_empty deque =
  deque.front_length + deque.back_length = 0

let length deque =
  deque.front_length + deque.back_length

(* TODO: balance is not part of the public surface of the module;
consider creating a module interface file. *)
let balance deque =
  let c = 3 in
  let take items n =
    let rec aux items n acc =
      match (items, n) with
      | (_, 0) -> acc
      | ([], _) -> acc
      | ((h :: t), _) -> aux t (n - 1) (h :: acc) in
    List.rev (aux items n []) in
  let rec drop items n =
    match(items, n) with
    | (_, 0) -> items
    | ([], _) -> []
    | (h :: t, _) -> drop t (n - 1) in
  if deque.front_length > c * deque.back_length + 1 then
    let new_front_length = (deque.front_length + deque.back_length) / 2 in
    let new_back_length = deque.front_length + deque.back_length - new_front_length in
    {
      front = take deque.front new_front_length;
      front_length = new_front_length;
      back = deque.back @ (List.rev (drop deque.front new_front_length));
      back_length = new_back_length
    }
    else if deque.back_length > c * deque.front_length + 1 then
      let new_front_length = (deque.front_length + deque.back_length) / 2 in
      let new_back_length = deque.front_length + deque.back_length - new_front_length in
      {
        front = deque.front @ List.rev(drop deque.back new_back_length);
        front_length = new_front_length;
        back = take deque.back new_back_length;
        back_length = new_back_length
      }
      else deque

let enqueue_front deque item =
  balance { deque with
    front = item :: deque.front;
    front_length = deque.front_length + 1}

let enqueue_back deque item =
  balance { deque with
    back = item :: deque.back;
    back_length = deque.back_length + 1}

let peek_front deque =
  match deque with
  | { front = []; back = [] } -> raise Empty
  | { front = h :: _} -> h
  | { back = [h] } -> h
  | _ -> failwith "peek_front: Front is empty, back has more than one item"

let peek_back deque =
  match deque with
  | { front = []; back = [] } -> raise Empty
  | { back = h :: _} -> h
  | { front = [h] } -> h
  | _ -> failwith "peek_back: Back is empty, front has more than one item"

let dequeue_front deque =
  match deque with
  | { front = []; back = [] } -> raise Empty
  | { front = [_]; back = [] } -> empty
  | { front = []; back = [_] } -> empty
  | { front = _ :: t } ->
    balance { deque with front = t; front_length = deque.front_length - 1 }
  | _ -> failwith "dequeue_front: Front is empty, back has more than one item"

let dequeue_back deque =
  match deque with
  | { front = []; back = [] } -> raise Empty
  | { front = [_]; back = [] } -> empty
  | { front = []; back = [_] } -> empty
  | { back = _ :: t } ->
    balance { deque with back = t; back_length = deque.back_length - 1 }
  | _ -> failwith "dequeue_back: Back is empty, front has more than one item"

let peek_front_at deque n =
  let length = deque.front_length + deque.back_length in
  if (n < 0) || (n >= length) then raise InvalidIndex
  else if n < deque.front_length then List.nth deque.front n
  else List.nth deque.back (length - 1 - n)

let peek_back_at deque n =
  let length = deque.front_length + deque.back_length in
  if (n < 0) || (n >= length) then raise InvalidIndex
  else if n < deque.back_length then List.nth deque.back n
  else List.nth deque.front (length - 1 - n)

let rec set_front_at deque item n =
  if n = 0 then enqueue_front (dequeue_front deque) item
  else enqueue_front (set_front_at (dequeue_front deque) item (n - 1)) (peek_front deque)

let rec set_back_at deque item n =
  if n = 0 then enqueue_back (dequeue_back deque) item
  else enqueue_back (set_back_at (dequeue_back deque) item (n - 1)) (peek_back deque)

let rec merge d1 d2 =
  if is_empty d1 then d2
  else if is_empty d2 then d1
  else if (length d1) < (length d2) then
    merge (dequeue_front d1) (enqueue_back d2 (peek_front d1))
  else
    merge (enqueue_front d1 (peek_back d2)) (dequeue_back d2)

let split deque c =
  let rec move d1 d2 =
    if (length d1) = c then (d1, d2)
    else move (enqueue_front d1 (peek_back d2)) (dequeue_back d2) in
  move empty deque
