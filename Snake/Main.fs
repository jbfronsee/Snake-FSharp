namespace Snake

    module Main =

        open Gtk
        open Snake.GameTypes

        let rand = System.Random().NextDouble

        let readHighScore =
            try
                let highScore = System.IO.File.ReadLines "snake.txt" |> Seq.head |> int
                Some highScore
            with _ ->
                None 

        let rec run state =
            match Application.EventsPending() with
            | true ->
                do Application.RunIteration()
                state |> run
            | false ->
                do System.Threading.Thread.Sleep(50) |> ignore

                let boundState = state |> Game.newBounds (Events.readWinSize())

                let input = Events.readInput()
                let inputState = boundState |> Events.ProcessInput input

                let randTuple = (rand() |> Random.create, rand() |> Random.create)
                let newState = inputState |> Game.Update randTuple

                match newState with
                | Paused _ | Running _ ->
                    do newState |> Events.draw |> ignore
                    newState |> run
                | Quit -> 
                    // Exit
                    ()

        [<EntryPoint>]
        let main(args) =
            try 
                let high = 
                    match readHighScore with
                    | Some x ->
                        x
                    | None ->
                        0

                Application.Init()

                MainWindow.GameWindow.Canvas.QueueDraw()
                MainWindow.GameWindow.Show();
                let randTuple = (rand() |> Random.create, rand() |> Random.create)
                randTuple |> Game.start { width=MainWindow.wWidth; height=MainWindow.wHeight } high |> run
                0
            with e -> 
                do Log.write [e.Message]
                0
