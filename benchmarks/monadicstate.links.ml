module type STATE = sig
  type t
  val empty : t
end

module type MONAD = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type STATE_MONAD =
  functor (S : STATE) ->
  sig
    include MONAD
    val access : 'a t -> 'a
    val get    : unit -> S.t t
    val put    : S.t  -> unit t
    val eval   : 'a t -> S.t -> 'a
  end

module StateMonad : STATE_MONAD =
   functor(S : STATE) ->
     struct
       type state = S.t
       type 'a t = state -> ('a * state)
       let bind m f =
         fun s ->
           match m s with 
              | (x, s') -> f x s'
       let return a = fun s -> (a, s)
       let access m =
           match m S.empty with
             | (x, s) -> x
       let put s =
           fun _ -> ((), s)
       let get () =
         fun s -> (s, s)
       let eval f init =
         match f init with
         | (x, _) -> x
     end

module IS = StateMonad(struct type t = int let empty = 0 end)

let (>>=) = IS.bind
let return = IS.return
let get    = IS.get
let put    = IS.put               
                             
let rec count () =
  get () >>=
    fun n -> if n == 0 then return n
             else
               put (n - 1) >>= fun _ -> count ()

let _ = print_endline (string_of_int (IS.eval (count ()) 10000000))
