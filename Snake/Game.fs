module Snake.Game

let detectCollision (pos1:State.Position) (pos2:State.Position) =
    not (pos1.y >= pos2.y + State.plSize || 
         pos1.x + State.plSize <= pos2.x || 
         pos1.y + State.plSize <= pos2.y || 
         pos1.x >= pos2.x + State.plSize)

let acquiredPrize (newPos:State.Position) (state:State.Board) =
    detectCollision newPos state.prize

let outOfBounds (player:State.Player) =
    (player.head.y > (MainWindow.wHeight - State.plSize)) || 
    (player.head.y < 0) || 
    (player.head.x > (MainWindow.wWidth - State.plSize)) || 
    (player.head.x < 0)

let isDead newPos (state:State.Board) =
    let unwrap (State.Body(x,y)) = (x,y)

    match outOfBounds state.player with
    | true ->
        true
    | false ->
        let (forward, reverse) = unwrap state.player.body
        let forwardCollision = forward |> List.exists (fun pos -> detectCollision newPos pos)
        let reverseCollision = reverse |> List.exists (fun pos -> detectCollision newPos pos)
        forwardCollision || reverseCollision

let deadSnake newPos (state:State.Board) =
    match state |> isDead newPos with
    | true ->
        { State.start 
            with prize=(State.randomPos (float(MainWindow.wWidth) - float(State.plSize)) (float(MainWindow.wHeight) - float(State.plSize))); }
    | false ->
        let newPlayer = State.movePlayer state.player
        let newState = { state with player=newPlayer }
        newState

let changeDir newDir (state:State.Board) =
    match (state.pause || (State.areOpposite state.player.dir newDir)) with
    | true -> state
    | false -> { state with player={ state.player with dir=newDir } }

let ProcessInput keyPress (state:State.Board) =
    match keyPress with
    | Gdk.Key.p | Gdk.Key.P ->
        { state with pause = true }
    | Gdk.Key.Up ->
        { state with pause = false } |> changeDir State.Up
    | Gdk.Key.Down ->
        { state with pause = false } |> changeDir State.Down
    | Gdk.Key.Left ->
        { state with pause = false } |> changeDir State.Left
    | Gdk.Key.Right -> 
        { state with pause = false } |> changeDir State.Right
    | _ -> state

let Update (state:State.Board) =
    match state.pause with
    | true ->
        state
    | false ->
        let newPos = State.nextMove state.player
        match state |> acquiredPrize newPos with
        | true ->
            let newPlayer = { state.player with head=newPos; body=(State.add newPos state.player.body) }
            let newPrize = State.randomPos (float(MainWindow.wWidth) - float(State.plSize)) (float(MainWindow.wHeight) - float(State.plSize))
            let newState = { state with player=newPlayer; prize=newPrize; score=state.score+1 }
            newState
        | false ->
            let newState = state |> deadSnake newPos
            newState