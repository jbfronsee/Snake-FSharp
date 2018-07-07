module Snake.State

let rand = System.Random().NextDouble

type Direction = Up | Down | Left | Right

let opposite dir =
    match dir with
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

let areOpposite d1 d2 =
    d1 = (opposite d2)

type Position = { x:int; y:int }

let randomPos xHigh yHigh =
    { 
        x = int(rand() * xHigh);
        y = int(rand() * yHigh);
    }

type Body = Body of Position list * Position list

let plSize = 20;

let startingBody = Body([],[{x=0;y=0};{x=1*plSize;y=0};{x=2*plSize;y=0};{x=3*plSize;y=0};
                            {x=4*plSize;y=0};{x=5*plSize;y=0};{x=6*plSize;y=0};{x=7*plSize;y=0};{x=8*plSize;y=0}])

let add piece body =
    match body with
    | Body(forward, reverse) -> Body(piece::forward, reverse)

let remove body =
    match body with
    | Body([],[]) ->
        Body([],[]) //TODO: change maybe
    | Body(forward, r::reverse) ->
        Body(forward,reverse)
    | Body(forward, []) ->
        let reverse = forward |> List.rev
        Body([], reverse |> List.tail)

type Player = 
    { 
        dir : Direction;
        body : Body;
        head : Position;
        size : int;
    }

type Board =
    {
        player : Player;
        prize : Position;
        pause : bool;
        score : int;
    }

let start = {

    player = 
        {
            body = startingBody;
            dir = Right; 
            head = { x = 8*plSize; y = 0}; 
            size = plSize;
        };

    prize = randomPos (float(MainWindow.wWidth) - float(plSize)) (float(MainWindow.wHeight) - float(plSize));
    pause = false;
    score = 0;
}

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