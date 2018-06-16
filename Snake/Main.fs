module Snake.Main

open System
open Gtk
open MainWindow

let g_Clock = new Timers.Timer(3000.0)



let rec run (state:State.Board) =
    match Application.EventsPending() with
    | true ->
        do Application.RunIteration()
        run state
    | false ->
        do System.Threading.Thread.Sleep(50) |> ignore
        let newState = Game.Update (Game.ProcessInput (Events.ReadInput()) state)
        do Events.Draw newState |> ignore
        run newState

[<EntryPoint>]
let Main(args) = 
    Application.Init()

    do Events.Initialize() |> ignore
    Window.Canvas.QueueDraw()
    Window.Show()
    run State.start
    0
