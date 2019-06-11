namespace Snake

    module GameTypes =

        type Random = private Random of float

        module Random =
            let create n =
                match n < 1.0 && n >= 0.0 with
                | true ->
                    Some (Random n)
                | false ->
                    None

            let value (Random n) = n

        type Direction = Up | Down | Left | Right

        module Direction =

            let opposite dir =
                match dir with
                | Up -> Down
                | Down -> Up
                | Left -> Right
                | Right -> Left

            let areOpposite d1 d2 =
                d1 = (opposite d2)
            

        type Position = { x:int; y:int; }

        type Bounds = { width:int; height:int; }
        
        /// <summary>
        /// Body is structured like a queue.
        /// </summary>
        type Body = private Body of Position list * Position list

        module Body =
            let private unwrap (Body(f, r)) = (f,r)

            let create list =
                Body([],list)

            /// <summary>
            /// Add to back of queue.
            /// </summary>
            let add piece body =
                match body with
                | Body(forward, reverse) -> Body(piece::forward, reverse)

            /// <summary>
            /// Remove from front of queue.
            /// </summary>
            let remove body =
                match body with
                | Body([],[]) ->
                    Body([],[])
                | Body(forward, r::reverse) ->
                    Body(forward,reverse)
                | Body(forward, []) ->
                    let reverse = forward |> List.rev
                    Body([], reverse |> List.tail)

            let exists predicate body =
                let (forward, reverse) = unwrap body

                match forward |> List.exists predicate with
                | true ->
                    true
                | false ->
                    reverse |> List.exists predicate

            let iter action body =
                let (forward, reverse) = unwrap body

                do forward |> List.iter action
                do reverse |> List.iter action

        type Player = 
            { 
                dir : Direction;
                body : Body;
                head : Position;
                size : int;
            }
        
        type Score =
            {
                present : int;
                high : int;
            }

        type Board =
            {
                player : Player;
                prize : Position;
                score : Score;
                bounds : Bounds;
            }

        type RunningData = private RunningData of Board

        module RunningData =
            let create b =
                RunningData b

            let value (RunningData b) = b

        type PausedData = private PausedData of Board

        module PausedData =
            let create b =
                PausedData b

            let value (PausedData b) = b

        type State = 
            | Running of RunningData
            | Paused of PausedData 
            | Quit

        module State =
            let switch state =
                match state with
                | Running state ->
                    let board = RunningData.value state
                    Paused (PausedData.create board)
                | Paused state ->
                    let board = PausedData.value state
                    Running (RunningData.create board)
                | Quit ->
                    Quit
