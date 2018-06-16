module Snake.Main

open Gtk
open MainWindow

let rec run (state:State.Board) =
    match Application.EventsPending() with
    | true ->
        do Application.RunIteration()
        state |> run
    | false ->
        do System.Threading.Thread.Sleep(50) |> ignore
        let newState = (state |> Game.ProcessInput (Events.readInput())) |> Game.Update 
        do newState |> Events.draw |> ignore
        newState |> run

[<EntryPoint>]
let main(args) = 
    Application.Init()

    GameWindow.Canvas.QueueDraw()
    GameWindow.Show();
    State.start |> run
    0
