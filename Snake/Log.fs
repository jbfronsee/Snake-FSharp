namespace Snake

    open System.IO    

    module Log =

        let private logFile = "Log - " + (System.DateTime.Now.ToString "MM-dd-yyyy") + ".txt"

        let write str =
            try
                let timestamp = sprintf "Date/Time: %A" System.DateTime.Now
                let record = timestamp::str @ [""]

                File.AppendAllLines(logFile, List.toSeq record)
            with e -> printfn "%A %A" (e.GetType()) e.Message