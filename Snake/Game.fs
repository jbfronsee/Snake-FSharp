module Snake.Game

//let rand = System.Random().NextDouble

type Direction = Up | Down | Left | Right

let opposite dir =
    match dir with
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

let areOpposite d1 d2 =
    d1 = (opposite d2)

type Position = { x:int; y:int; }

/// <summary>
/// Converts a random number into a position.
/// </summary>
let randomPos xHigh yHigh rand =
    { 
        x = int(rand() * xHigh);
        y = int(rand() * yHigh);
    }

/// <summary>
/// Body is structured like a queue.
/// </summary>
type Body = Body of Position list * Position list

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

let plSize = 20;

/// <summary>
/// Queue is actually the reverse of the snake's body the tail is the front
/// </summary>
let startingBody = Body([],[{x=0;y=0};{x=1*plSize;y=0};{x=2*plSize;y=0};{x=3*plSize;y=0};
                            {x=4*plSize;y=0};{x=5*plSize;y=0};{x=6*plSize;y=0};{x=7*plSize;y=0};{x=8*plSize;y=0}])
                                                       
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
    }

type RunningData = RunningData of Board

type PausedData = PausedData of Board

type State = 
    | Running of RunningData
    | Paused of PausedData 
    | Quit

let switch state =
    match state with
    | Running state ->
        let (RunningData board) = state
        Paused (PausedData board)
    | Paused state ->
        let (PausedData board) = state
        Running (RunningData board)
    | Quit ->
        Quit


let startingBoard highScore rand = 
    {

        player = 
            {
                body = startingBody;
                dir = Right; 
                head = 
                    { 
                        x = 8*plSize; 
                        y = 0;
                    }; 
                size = plSize;
            };

        prize = rand |> randomPos 
            (float(MainWindow.wWidth) - float(plSize)) (float(MainWindow.wHeight) - float(plSize));
        score = 
            { 
                present = 0; 
                high = highScore;
            };
    }

let start highScore rand = 
    Running(RunningData(startingBoard highScore rand))

let nextMove player =
    match player with
    | { dir = Up } ->
        { player.head with y = player.head.y - player.size }
    | { dir = Down } -> 
        { player.head with y = player.head.y + player.size }
    | { dir = Left }  ->
        { player.head with x = player.head.x - player.size }
    | { dir = Right }  ->
        { player.head with x = player.head.x + player.size }

let movePlayer player =
    let newPos = nextMove player
    let newBody = player.body |> remove |> add newPos
    let newPlayer = { player with head = newPos; body=newBody }
    newPlayer

let detectCollision pos1 pos2 =
    not (pos1.y >= pos2.y + plSize || 
         pos1.x + plSize <= pos2.x || 
         pos1.y + plSize <= pos2.y || 
         pos1.x >= pos2.x + plSize)

let acquiredPrize newPos board =
    detectCollision newPos board.prize

let outOfBounds player =
    (player.head.y > (MainWindow.wHeight - plSize)) || 
    (player.head.y < 0) || 
    (player.head.x > (MainWindow.wWidth - plSize)) || 
    (player.head.x < 0)

let isDead newPos board =
    let unwrap (Body(x,y)) = (x,y)

    match outOfBounds board.player with
    | true ->
        true
    | false ->
        let (forward, reverse) = unwrap board.player.body
        let forwardCollision = forward |> List.exists (fun pos -> detectCollision pos newPos)
        let reverseCollision = reverse |> List.exists (fun pos -> detectCollision pos newPos)
        forwardCollision || reverseCollision

let newHighScore score =
        match (score.present > score.high) with
        | true ->
            score.present
        | false ->
            score.high

let deadSnake newPos rand board =
    match board |> isDead newPos with
    | true ->
        rand |> startingBoard (newHighScore board.score)
    | false ->
        let newPlayer = movePlayer board.player
        let newState = { board with player=newPlayer }
        newState

let changeDir newDir state =
    match state with
    | Running state ->
        let (RunningData board) = state
        match (areOpposite board.player.dir newDir) with
        | true ->
            Running state
        | false ->
            let newBoard = { board with player={ board.player with dir=newDir } }
            Running (RunningData newBoard)
    | Paused state ->
        Paused state
    | Quit ->
        Quit

let Update rand state =
    match state with
    | Paused state ->
        Paused state
    | Running state ->
        let (RunningData board) = state
        let newPos = nextMove board.player
        match board |> acquiredPrize newPos with
        | true ->
            let newPlayer = { board.player with head=newPos; body=(add newPos board.player.body) }
            let newPrize = rand |> randomPos ( float(MainWindow.wWidth) - float(plSize) ) ( float(MainWindow.wHeight) - float(plSize) )
            let newState = { board with player=newPlayer; prize=newPrize; score = { board.score with present=board.score.present+1 }; }
            Running ( RunningData newState )
        | false ->
            let newState = board |> (rand |> deadSnake newPos)
            Running ( RunningData newState )
    | Quit ->
        Quit