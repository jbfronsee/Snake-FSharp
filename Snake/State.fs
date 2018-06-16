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

let startingBody = Body([],[{x=0;y=0};{x=10;y=0};{x=20;y=0};{x=30;y=0};{x=40;y=0};{x=50;y=0};{x=60;y=0};{x=70;y=0};{x=80;y=0}])

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
        speed : int;
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
            head = { x = 80; y = 0}; 
            speed = 10;
        };

    // TODO: make an initialize function for this so it can be different on death
    // TODO: constants for randomPos
    prize = randomPos 790.0 70.0;
    pause = false;
    score = 0;
}

let nextMove player =
    match player with
    | { dir = Up } ->
        { player.head with y = player.head.y - player.speed }
    | { dir = Down } -> 
        { player.head with y = player.head.y + player.speed }
    | { dir = Left }  ->
        { player.head with x = player.head.x - player.speed }
    | { dir = Right }  ->
        { player.head with x = player.head.x + player.speed }

let movePlayer player =
    let newPos = nextMove player
    let newBody = player.body |> remove |> add newPos
    let newPlayer = { player with head = newPos; body=newBody }
    newPlayer