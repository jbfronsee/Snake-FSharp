namespace Snake

    module Main =

        open Gtk
        open Snake.GameTypes
        open Snake.MainWindow

        let rand = System.Random().NextDouble

        let readHighScore() =
            try
                let highScore = System.IO.File.ReadLines "snake.txt" |> Seq.head |> int
                Some(highScore)
            with _ ->
                None 

        let rec run state =
            match Application.EventsPending() with
            | true ->
                do Application.RunIteration()
                state |> run
            | false ->
                do System.Threading.Thread.Sleep(50) |> ignore

                let input = Events.readInput()
                let inputState = state |> Events.ProcessInput input           

                let maybeRand = rand() |> Random.create
                let newState = inputState |> Game.Update maybeRand

                match newState with
                | Paused _ | Running _ ->
                    do newState |> Events.draw |> ignore
                    newState |> run
                | Quit -> 
                    // Exit
                    ()

        [<EntryPoint>]
        let main(args) = 
            let high = 
                match readHighScore() with
                | Some x ->
                    x
                | None ->
                    0

            Application.Init()

            MainWindow.GameWindow.Canvas.QueueDraw()
            MainWindow.GameWindow.Show();
            rand() |> Random.create |> Game.start { width=MainWindow.wWidth; height=MainWindow.wHeight } high |> run
            0
