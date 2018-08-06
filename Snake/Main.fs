module Snake.Main

open Gtk
open MainWindow

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
        let newState = 
            ( state |> Events.ProcessInput input ) |> Game.Update rand
        match newState with
        | Game.Paused _ | Game.Running _ ->
            do newState |> Events.draw |> ignore
            newState |> run
        | Game.Quit -> 
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

    GameWindow.Canvas.QueueDraw()
    GameWindow.Show();
    rand |> Game.start high |> run
    0
