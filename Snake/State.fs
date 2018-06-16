module Snake.State

let rand = System.Random()

type Direction = Up | Down | Left | Right

let opposite dir =
    match dir with
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

let areOpposite d1 d2 =
    d1 = (opposite d2)

type Position = { x:float; y:float }

let randomPos xHigh yHigh =
    { 
        x = rand.NextDouble() * xHigh;
        y = rand.NextDouble() * yHigh;
    }

type Speed = Speed of float

type Body = Body of Position list * Position list

let StartingBody = Body([],[{x=0.0;y=0.0};{x=10.0;y=0.0};{x=20.0;y=0.0};{x=30.0;y=0.0};{x=40.0;y=0.0};{x=50.0;y=0.0};{x=60.0;y=0.0};{x=70.0;y=0.0};{x=80.0;y=0.0}])

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
        speed : float;
    }

type Board =
    {
        player : Player
        prize : Position
        pause: bool;
    }

let start = {

    player = 
        {
        body = StartingBody;
        dir = Right; 
        head={ x = 80.0; y = 0.0}; 
        speed = 10.01;
        };

    pause=false;

    prize=randomPos 800.0 800.0;
}

let NextMove player =
    match player with
    | { dir = Up } ->
        { player.head with y = player.head.y - player.speed }
    | { dir = Down } -> 
        { player.head with y = player.head.y + player.speed }
    | { dir = Left }  ->
        { player.head with x = player.head.x - player.speed }
    | { dir = Right }  ->
        { player.head with x = player.head.x + player.speed }

let MovePlayer player =
    let newPos = NextMove player
    let newBody = player.body |> remove |> add newPos
    let newState = { player with head = newPos; body=newBody }
    newState