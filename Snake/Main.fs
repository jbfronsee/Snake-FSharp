module Snake.Main

open Gtk
open MainWindow

let rec run state =
    match Application.EventsPending() with
    | true ->
        do Application.RunIteration()
        state |> run
    | false ->
        do System.Threading.Thread.Sleep(50) |> ignore
        let newState = 
            ( state |> Events.ProcessInput ( Events.readInput() ) ) |> Game.Update
        match newState with
        | Game.Paused _ | Game.Running _ ->
            do newState |> Events.draw |> ignore
            newState |> run
        | Game.Quit -> 
            // Exit
            ()

[<EntryPoint>]
let main(args) = 
    Application.Init()

    GameWindow.Canvas.QueueDraw()
    GameWindow.Show();
    Game.start |> run
    0
