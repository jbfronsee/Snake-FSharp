namespace Snake

    module Game =
        
        open Snake.GameTypes

        /// <summary>
        /// Converts a random number into a position.
        /// </summary>
        let randomPos xHigh yHigh rand =
            let randomPos' multx multy =
                {
                    x = int(multx * xHigh);
                    y = int(multy * yHigh);
                }

            let getVal r coord =
                match r with
                | Some value ->
                    Random.value value
                | None ->
                    do Log.write [sprintf "Random value is None using default position for %A co-ordinate." coord]
                    0.5

            let (r1, r2) = rand
            randomPos' <| getVal r1 "X" <| getVal r2 "Y"

        let plSize = 20

        /// <summary>
        /// Queue is actually the reverse of the snake's body the tail is the front
        /// </summary>
        let startingBody = 
            Body.create [
                {x=0;y=0};
                {x=1*plSize;y=0};
                {x=2*plSize;y=0};
                {x=3*plSize;y=0};
                {x=4*plSize;y=0};
                {x=5*plSize;y=0};
                {x=6*plSize;y=0};
                {x=7*plSize;y=0};
                {x=8*plSize;y=0}]

        let startingBoard bounds highScore rand = 
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
                    (float(bounds.width) - float(plSize)) (float(bounds.height) - float(plSize));
                score = 
                    { 
                        present = 0; 
                        high = highScore;
                    };

                bounds = bounds;
            }

        let start bounds highScore rand = 
            Running (RunningData.create (startingBoard bounds highScore rand))

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
            let newBody = player.body |> Body.remove |> Body.add newPos
            let newPlayer = { player with head = newPos; body=newBody }
            newPlayer

        let detectCollision pos1 pos2 =
            not (pos1.y >= pos2.y + plSize || 
                 pos1.x + plSize <= pos2.x || 
                 pos1.y + plSize <= pos2.y || 
                 pos1.x >= pos2.x + plSize)

        let acquiredPrize newPos board =
            detectCollision newPos board.prize

        let outOfBounds player bounds =
            (player.head.y > (bounds.height - plSize)) || 
            (player.head.y < 0) || 
            (player.head.x > (bounds.width - plSize)) || 
            (player.head.x < 0)

        let isDead newPos board =
            match outOfBounds board.player board.bounds with
            | true ->
                true
            | false ->
                board.player.body |> Body.exists (fun pos -> detectCollision pos newPos)

        let newHighScore score =
                match (score.present > score.high) with
                | true ->
                    score.present
                | false ->
                    score.high

        let deadSnake newPos rand board =
            match board |> isDead newPos with
            | true ->
                rand |> startingBoard board.bounds (newHighScore board.score)
            | false ->
                let newPlayer = movePlayer board.player
                let newState = { board with player=newPlayer }
                newState

        let changeDir newDir state =
            match state with
            | Running state ->
                let board = RunningData.value state
                match (Direction.areOpposite board.player.dir newDir) with
                | true ->
                    Running state
                | false ->
                    let newBoard = { board with player={ board.player with dir=newDir } }
                    Running (RunningData.create newBoard)
            | Paused state ->
                Paused state
            | Quit ->
                Quit

        let newBounds b state =
            match state with
            | Running state ->
                let board = RunningData.value state
                Running <| RunningData.create { board with bounds=b }
            | Paused state ->
                let board = PausedData.value state
                Paused <| PausedData.create { board with bounds=b }
            | Quit ->
                state
        
        let Update rand state =
            match state with
            | Paused state ->
                Paused state
            | Running state ->
                let board = RunningData.value state
                let newPos = nextMove board.player
                match board |> acquiredPrize newPos with
                | true ->
                    let newPlayer = { board.player with head=newPos; body=(Body.add newPos board.player.body) }
                    let newPrize = rand |> randomPos ( float(board.bounds.width) - float(plSize) ) ( float(board.bounds.height) - float(plSize) )
                    let newState = { board with player=newPlayer; prize=newPrize; score = { board.score with present=board.score.present+1 }; }
                    Running ( RunningData.create newState )
                | false ->
                    let newState = board |> (rand |> deadSnake newPos)
                    Running ( RunningData.create newState )
            | Quit ->
                Quit
