module Snake.Game

let DetectCollision (pos1:State.Position) (pos2:State.Position) =
    not (pos1.y > pos2.y + 10.0 || 
         pos1.x + 10.0 < pos2.x || 
         pos1.y + 10.0 < pos2.y || 
         pos1.x > pos2.x + 10.0)

let AcquiredPrize (newPos:State.Position) (state:State.Board) =
    DetectCollision newPos state.prize

let IsDead newPos (state:State.Board) =
    let unwrap (State.Body(x,y)) = (x,y)

    let (forward, reverse) = unwrap state.player.body
    let forwardCollision = forward |> List.exists (fun pos -> DetectCollision newPos pos)
    let reverseCollision = reverse |> List.exists (fun pos -> DetectCollision newPos pos)
    forwardCollision || reverseCollision

let DeadSnake newPos (state:State.Board) =
    match IsDead newPos (state:State.Board) with
    | true ->
        State.start
    | false ->
        let newPlayer = State.MovePlayer state.player
        let newState = { state with player=newPlayer }
        newState

let changeDir (state:State.Board) newDir =
    match (state.pause || (State.areOpposite state.player.dir newDir)) with
    | true -> state
    | false -> { state with player={state.player with dir=newDir} }

let ProcessInput keyPress (state:State.Board)=
    match keyPress with
    | Gdk.Key.p | Gdk.Key.P ->
        { state with pause = true }
    | Gdk.Key.Up ->
        changeDir { state with pause = false } State.Up
    | Gdk.Key.Down ->
        changeDir { state with pause = false } State.Down
    | Gdk.Key.Left ->
        changeDir { state with pause = false } State.Left
    | Gdk.Key.Right -> 
        changeDir { state with pause = false } State.Right
    | _ -> state

let Update (state:State.Board) =
    match state.pause with
    | true ->
        state
    | false ->
        let newPos = State.NextMove state.player
        match AcquiredPrize newPos state with
        | true ->
            let newPlayer = {state.player with head=newPos; body=(State.add newPos state.player.body)}
            let newPrize = State.randomPos 800.0 800.0
            let newState = { state with player=newPlayer; prize=newPrize }
            newState
        | false ->
            let newState = DeadSnake newPos state
            newState