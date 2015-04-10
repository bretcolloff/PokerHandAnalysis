namespace FSHHP

open System
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Text.RegularExpressions

module Parser =
    // InvalidHand exception expected when parsing has failed.
    exception InvalidHand of string

    // Contains basic player information.
    type Player = { name: string; stack: float }

    // Stores the raw input of the players 2 cards.
    type Hand = { left: string; right: string }

    // Stores the information about the pot size and how much 'rake' is removed.
    type Result = { pot: float; rake: float }

    // Describes an action taken in the progression of the hand.
    type Action = 
        | DoesntShow of unit 
        | TimedOut of unit 
        | Mucks of unit 
        | MucksAndShows of Hand 
        | Shows of Hand 
        | ShowedAndWon of Hand * float 
        | Check of unit 
        | Fold of unit 
        | Call of float 
        | Bet of float 
        | Raise of float * float 
        | CollectUncalled of float 
        | CollectFromPot of float 
        | Posts of float
        | ShowedAndLost of Hand

    // Distinguishes each level of play for a single hand.
    type Streets = 
        | MetaData 
        | Players 
        | Blinds 
        | Hero 
        | Preflop 
        | Flop 
        | Turn 
        | River  
        | ShowDown 
        | Summary

    // Regular expression patterns used in processing the data.
    let Patterns = Map.ofSeq [("MetaData", @"(?<name>\S+)\s(Game|Zoom\sHand)\s\#[0-9]+\:\s+(?<game>.+)\s\(\$(?<sb>\d+\.\d{2})\/\$(?<bb>\d+\.\d{2})( USD|)\)");
                                ("Table", @"Table\s\'(?<table>.+)\'"); 
                                ("Player", @"Seat \d: (?<name>.+) \(\$(?<stack>\d+(\.\d{2})?) in chips\)");
                                ("Hero", @"Dealt to (?<hero>.+)\s\[(?<leftcard>.{2})\s(?<rightcard>.{2})\]");
                                ("StandardAction", @"(?<name>.+)\:\s(?<action>.+)"); 
                                ("UncalledAction", @"(?<action>Uncalled bet \(\$\d+\.\d+\) returned to )(?<name>.+)");
                                ("CollectedUncalledAction", @"(?<name>.+)\s(?<action>collected.+)"); 
                                ("TimedOut", @"(?<name>.+)\s(?<action>has\stimed\sout)"); 
                                ("Posted", "(?<name>.+)\:\s(?<action>posts.+)")]

    // Maps a hand to the player from whose perspective the action is happening.
    type Hero = { player: Player; hand: Hand }

    // Maps an action to a player.
    type PlayerAction = { player: Player; action: Action }

    // Maps actions taken to a 'street'.
    type Street = { actions: PlayerAction list; streetType: Streets }

    // Stores the mandatory and minimum bet values.
    type Stake = { smallBlind: float; bigBlind: float }

    type MetaData = { site: string; tableName: string; stake: Stake }
    type HandHistory = { metaData: MetaData; players: Player list; streets: Street list; result: Result}

    /// Processes the metadata from the hand.
    let ProcessMetaData (input: string list) = 
        if input.Count() < 2 then raise (InvalidHand("This hand is not valid."))

        let matches = Regex.Matches(input.[0], Patterns.Item("MetaData"))
        if matches.Count = 0 then raise (InvalidHand("This hand is not valid."))
        
        let siteName = matches.[0].Groups.["name"].Value
        let stake = { smallBlind = float matches.[0].Groups.["sb"].Value; bigBlind = float matches.[0].Groups.["bb"].Value }
        let matches = Regex.Matches(input.[1], Patterns.Item("Table"))
        { site = siteName; tableName = matches.[0].Groups.["table"].Value; stake = stake }

    /// Gets and processes the players from the hand.
    let GetPlayers (input: string list) =
        let toPlayer line = 
            let matches = Regex.Matches(line, Patterns.Item("Player"))
            let name = matches.[0].Groups.["name"].Value
            let stack = float (matches.[0].Groups.["stack"].Value)
            { name = name; stack = stack }

        input |> List.filter (fun line -> Regex.Matches(line, Patterns.Item("Player")).Count > 0)
              |> List.map (fun line -> toPlayer line)

    /// Get the hero player from the lines provided.
    let GetHero (input: string list, players: Player list) =
        let line = input.Where(fun (x:string) -> x.StartsWith("Dealt to ")).Single() 
        let matches = Regex.Matches(line, Patterns.Item("Hero"))
        let name = matches.[0].Groups.["hero"].Value
        let leftCard = matches.[0].Groups.["leftcard"].Value
        let rightCard = matches.[0].Groups.["rightcard"].Value
        let hand = { left = leftCard; right = rightCard }
        { player = players.Single(fun x -> x.name = name); hand = hand }

    /// Get the street actions from the line provided.
    let ProcessStreet (input: string list, players: Player list) = 
        let splitStreet x =        
            let normalOutput = Regex.Matches(x, Patterns.Item("StandardAction"))
            let uncalledOutput = Regex.Matches(x, Patterns.Item("UncalledAction"))
            let collectedOutput = Regex.Matches(x, Patterns.Item("CollectedUncalledAction"))
            let timedOutOutput = Regex.Matches(x, Patterns.Item("TimedOut"))
            let postedOutput = Regex.Matches(x, Patterns.Item("Posted"))
        
            if normalOutput.Count <> 0 then (players.Single(fun x -> x.name = normalOutput.[0].Groups.["name"].Value), normalOutput.[0].Groups.["action"].Value)
            else if uncalledOutput.Count <> 0 then (players.Single(fun x -> x.name = uncalledOutput.[0].Groups.["name"].Value), uncalledOutput.[0].Groups.["action"].Value)
            else if collectedOutput.Count <> 0 then (players.Single(fun x -> x.name = collectedOutput.[0].Groups.["name"].Value), collectedOutput.[0].Groups.["action"].Value)
            else if timedOutOutput.Count <> 0 then (players.Single(fun x -> x.name = timedOutOutput.[0].Groups.["name"].Value), timedOutOutput.[0].Groups.["action"].Value)
            else if postedOutput.Count <> 0 then (players.Single(fun x -> x.name = postedOutput.[0].Groups.["name"].Value), postedOutput.[0].Groups.["action"].Value)
            else raise (InvalidHand("This hand is invalid"))

        // Matches strings into actions taken by a player.
        let processAction (action: string, player: Player) = 
            let ActionPatterns = Map.ofSeq [("Checks", "checks"); 
                                            ("Folds", "folds"); 
                                            ("Calls", "calls\s\$(?<cost>\d+\.\d{2})");
                                            ("Bets", "bets\s\$(?<cost>\d+\.\d{2})"); 
                                            ("Raises", "raises\s\$(?<base>\d+\.\d{2})\sto\s\$(?<raise>\d\.\d{2})");
                                            ("CollectUncalled", "Uncalled\sbet\s\(\$(?<amount>\d+\.\d{2})\)\sreturned\sto");
                                            ("CollectFromPot", "collected\s\$(?<amount>\d+\.\d{2})"); 
                                            ("TimedOut", "has\stimed\sout");
                                            ("DoesntShow", "doesn\'t\sshow\shand"); 
                                            ("Shows", "shows\s\[(?<left>.{2})\s(?<right>.{2})\]");
                                            ("Mucks", "mucks\shand"); 
                                            ("Posts", "blind\s\$(?<amount>\d+\.\d{2})")]

            // Run the input against all of the regular expressions.
            match action with
                | action when Regex.Matches(action, ActionPatterns.Item("Checks")).Count > 0 -> { player = player; action = Action.Check() }
                | action when Regex.Matches(action, ActionPatterns.Item("Folds")).Count > 0 -> { player = player; action = Action.Fold() }
                | action when Regex.Matches(action, ActionPatterns.Item("Calls")).Count > 0 -> { player = player; action = Action.Call(float (Regex.Matches(action, ActionPatterns.Item("Calls")).[0].Groups.["cost"].Value)) }
                | action when Regex.Matches(action, ActionPatterns.Item("Bets")).Count > 0 -> { player = player; action = Action.Bet(float (Regex.Matches(action, ActionPatterns.Item("Bets")).[0].Groups.["cost"].Value)) }
                | action when Regex.Matches(action, ActionPatterns.Item("Raises")).Count > 0 -> { player = player; action = Action.Raise(float (Regex.Matches(action, ActionPatterns.Item("Raises")).[0].Groups.["base"].Value), float (Regex.Matches(action, ActionPatterns.Item("Raises")).[0].Groups.["raise"].Value)) }
                | action when Regex.Matches(action, ActionPatterns.Item("CollectUncalled")).Count > 0 -> { player = player; action = Action.CollectUncalled(float (Regex.Matches(action, ActionPatterns.Item("CollectUncalled")).[0].Groups.["amount"].Value)) }
                | action when Regex.Matches(action, ActionPatterns.Item("CollectFromPot")).Count > 0 -> { player = player; action = Action.CollectFromPot(float (Regex.Matches(action, ActionPatterns.Item("CollectFromPot")).[0].Groups.["amount"].Value)) }
                | action when Regex.Matches(action, ActionPatterns.Item("TimedOut")).Count > 0 -> { player = player; action = Action.TimedOut() }
                | action when Regex.Matches(action, ActionPatterns.Item("DoesntShow")).Count > 0 -> { player = player; action = Action.DoesntShow() }
                | action when Regex.Matches(action, ActionPatterns.Item("Shows")).Count > 0 -> { player = player; action = Action.Shows({left = Regex.Matches(action, ActionPatterns.Item("Shows")).[0].Groups.["left"].Value; right = Regex.Matches(action, ActionPatterns.Item("Shows")).[0].Groups.["right"].Value}) }
                | action when Regex.Matches(action, ActionPatterns.Item("Mucks")).Count > 0 -> { player = player; action = Action.Mucks() }
                | action when Regex.Matches(action, ActionPatterns.Item("Posts")).Count > 0 -> { player = player; action = Action.Posts(float (Regex.Matches(action, ActionPatterns.Item("Posts")).[0].Groups.["amount"].Value)) }
                | action -> raise (InvalidHand("This hand is invalid"))

        input |> List.map (fun x -> splitStreet x) |> List.map (fun act -> processAction (snd act, fst act))

    /// Filters out lines that we don't need to process.
    let FilterAction (input: string) = 
        Regex.Matches(input, "(?<name>.+)\s(?<action>leaves\sthe\stable)").Count = 0 && Regex.Matches(input, "\*\*\*\sFLOP\s\*\*\*").Count = 0
            && Regex.Matches(input, "\*\*\*\sTURN\s\*\*\*").Count = 0 && Regex.Matches(input, "\*\*\*\sRIVER\s\*\*\*").Count = 0 
            && Regex.Matches(input, "\*\*\*\sSHOW\sDOWN\s\*\*\*").Count = 0 && Regex.Matches(input, "\*\*\*\sSUMMARY\s\*\*\*").Count = 0 
            && Regex.Matches(input, "Board").Count = 0 && Regex.Matches(input, "\*\*\*\sHOLE\sCARDS\s\*\*\*").Count = 0 
            && Regex.Matches(input, "sits\sout").Count = 0 && Regex.Matches(input, "Total pot").Count = 0
            && Regex.Matches(input, "\sis\ssitting\sout").Count = 0

    /// Processes the summary information from a hand
    let ProcessSummary (input: string list, players: Player list) = 
        let SplitLine line = 
            let foldedPattern = Regex.Matches(line, "Seat\s\d:\s(?<player>[\w\s\.]+)\s(\(((big|small) blind|button)\)\s)?(?<action>folded)")
            let showedAndLostPattern = Regex.Matches(line, "Seat\s\d:\s(?<player>[\w\s\.]+)\s(\(((big|small) blind|button)\)\s)?(?<action>showed.+)")
            let showedAndWonPattern = Regex.Matches(line, "Seat\s\d:\s(?<player>[\w\s\.]+)\s(\(((big|small) blind|button)\)\s)?(?<action>showed\s\[(?<left>.{2})\s(?<right>.{2})\]\sand\swon\s\(\$(?<amount>\d+\.\d{2}))")
            let muckedPattern = Regex.Matches(line, "Seat\s\d\:\s(?<player>[\w\s\.]+)\s(?<action>mucked\s\[(?<left>.{2})\s(?<right>.{2})\])")
            let collectedPattern = Regex.Matches(line, "Seat\s\d\:\s(?<player>[\w\s\.]+)\s(\(((big|small) blind|button)\)\s)?(?<action>collected.+)")

            if foldedPattern.Count <> 0 then (players.Single(fun x -> x.name = foldedPattern.[0].Groups.["player"].Value), foldedPattern.[0].Groups.["action"].Value)
            else if showedAndWonPattern.Count <> 0 then (players.Single(fun x -> x.name = showedAndWonPattern.[0].Groups.["player"].Value), showedAndWonPattern.[0].Groups.["action"].Value)
            else if showedAndLostPattern.Count <> 0 then (players.Single(fun x -> x.name = showedAndLostPattern.[0].Groups.["player"].Value), showedAndLostPattern.[0].Groups.["action"].Value)
            else if muckedPattern.Count <> 0 then (players.Single(fun x -> x.name = muckedPattern.[0].Groups.["player"].Value), muckedPattern.[0].Groups.["action"].Value)
            else if collectedPattern.Count <> 0 then (players.Single(fun x -> x.name = collectedPattern.[0].Groups.["player"].Value), collectedPattern.[0].Groups.["action"].Value)
            else raise (InvalidHand("Hand is invalid"))

        // Process the showdown actions.
        let ProcessAction (player: Player, action: string) =
            let showedAndWonMatch = Regex.Matches(action, "showed\s\[(?<left>.{2})\s(?<right>.{2})\]\sand\swon\s\(\$(?<amount>\d+\.\d{2})")
            let showedAndLostMatch = Regex.Matches(action, "showed\s\[(?<left>.{2})\s(?<right>.{2})\]\sand\slost")
            let muckedAndShowed = Regex.Matches(action,  "mucked\s\[(?<left>.{2})\s(?<right>.{2})\]")
            let collected = Regex.Matches(action, "collected\s\(\$(?<amount>\d+\.\d{2})\)")

            match action with
            | action when collected.Count <> 0 -> { player = player; action = Action.CollectFromPot(float collected.[0].Groups.["amount"].Value) }
            | action when Regex.Matches(action, "folded").Count <> 0 -> { player = player; action = Action.Fold() }
            | action when showedAndWonMatch.Count <> 0 -> { player = player; action = Action.ShowedAndWon({left = showedAndWonMatch.[0].Groups.["left"].Value; right = showedAndWonMatch.[0].Groups.["right"].Value}, float (showedAndWonMatch.[0].Groups.["amount"].Value))}
            | action when showedAndLostMatch.Count <> 0 -> { player = player; action = Action.ShowedAndLost({left = showedAndLostMatch.[0].Groups.["left"].Value; right = showedAndLostMatch.[0].Groups.["right"].Value}) }
            | action when muckedAndShowed.Count <> 0 -> { player = player; action = Action.MucksAndShows({left = muckedAndShowed.[0].Groups.["left"].Value; right = muckedAndShowed.[0].Groups.["right"].Value})}
            | action -> raise (InvalidHand("Hand is invalid."))

        input |> List.map (fun x -> SplitLine x)
              |> List.map (fun x -> ProcessAction x)

    /// Processes the result of the hand.
    let ProcessResult (input: string list) = 
        let matchString = "Total pot \$(?<pot>(\d+\.\d{2}|\d)) \| Rake \$(?<rake>(\d+\.\d{2}|\d))"
        try
            input |> List.filter (fun line -> Regex.Matches(line, matchString).Count <> 0)
                  |> List.head
                  |> (fun line -> let matches = Regex.Matches(line, matchString)
                                  {pot =  float matches.[0].Groups.["pot"].Value; rake = float matches.[0].Groups.["rake"].Value})
        with
        | ArgumentException -> raise (InvalidHand("This hand is invalid."))

    /// Maps a street identifier to each action.
    let MapHand (input: List<string>) = 
        let street = ref Streets.MetaData
        seq { for line in input do
              if Regex.Matches(line, "Seat \d: (?<name>.+) \(\$(?<stack>\d+(\.\d{2})?) in chips\)").Count > 0 then street := Streets.Players
              else if Regex.Matches(line, "posts small blind").Count > 0 then street := Streets.Blinds
              else if !street = Streets.Hero then street := Streets.Preflop
              else if Regex.Matches(line, "Dealt to (?<hero>.+)\s\[(?<leftcard>.{2})\s(?<rightcard>.{2})\]").Count > 0 then street := Streets.Hero
              else if Regex.Matches(line, "\*\*\*\sFLOP\s\*\*\*").Count > 0 then street := Streets.Flop
              else if Regex.Matches(line, "\*\*\*\sTURN\s\*\*\*").Count > 0 then street := Streets.Turn
              else if Regex.Matches(line, "\*\*\*\sRIVER\s\*\*\*").Count > 0 then street := Streets.River
              else if Regex.Matches(line, "\*\*\*\sSHOW\sDOWN\s\*\*\*").Count > 0 then street := Streets.ShowDown
              else if Regex.Matches(line, "\*\*\*\sSUMMARY\s\*\*\*").Count > 0 then street := Streets.Summary
              yield (!street, line) } |> Seq.toList

    /// Processes Street - action string combos.
    let ProcessHand (input: (Streets * string) list) =
        // Filter all of the lines by the street they occurred on.
        let FilterByStreet lines filterStreet =
            lines |> List.filter (fun (street, _) -> street = filterStreet)
                  |> List.map (fun (_, action) -> action)

        // Process the result and the actions.
        let result = ProcessResult (FilterByStreet input Streets.Summary)
        let filtered = input |> List.filter (fun (_, action) -> FilterAction action)

        // Process each street of action.
        let metaData = ProcessMetaData (FilterByStreet filtered Streets.MetaData)
        let players = GetPlayers (FilterByStreet filtered Streets.Players)
        let hero = GetHero ((FilterByStreet filtered Streets.Hero), players) 
        let blinds = { actions = ProcessStreet ((FilterByStreet filtered Streets.Blinds), players); streetType = Streets.Blinds }   
        let preflop = { actions = ProcessStreet ((FilterByStreet filtered Streets.Preflop), players); streetType = Streets.Preflop }
        let flop = { actions = ProcessStreet ((FilterByStreet filtered Streets.Flop), players); streetType = Streets.Flop }
        let turn = { actions = ProcessStreet ((FilterByStreet filtered Streets.Turn), players); streetType = Streets.Turn }
        let river = { actions = ProcessStreet ((FilterByStreet filtered Streets.River), players); streetType = Streets.River }
        let showdown = { actions = ProcessStreet ((FilterByStreet filtered Streets.ShowDown), players); streetType = Streets.ShowDown }
        let summary = { actions = ProcessSummary ((FilterByStreet filtered Streets.Summary), players); streetType = Streets.Summary }
        
        // Bundle it all up together and return the result.
        let streets = [blinds; preflop; flop; turn; river; showdown; summary] |> List.filter (fun x -> x.actions.Length <> 0)
        { metaData = metaData; players = players; streets = streets; result = result }

    /// Processes a full file into several hand histories
    let ProcessFile (allLines: string list) = 
        let list = new List<List<string>>()

        let rec SplitFile (input: string list) =
            if input.Length <> 0 then
                list.Add(new List<string>(input.TakeWhile(fun x -> x <> "")))
                let nextGroup = input.SkipWhile(fun x -> x <> "").SkipWhile(fun x -> x = "")
                SplitFile (Seq.toList nextGroup)

        SplitFile allLines |> ignore
        list

    /// Read the file containing player hands.
    let public ReadHands (fileName: string) = 
        let lines = Array.toList (System.IO.File.ReadAllLines(fileName))
        let processed = ProcessFile lines
        let mapped = seq { for x in processed do yield MapHand x} |> Seq.toList
        let mapToSomeNone = mapped |> List.map (fun hand -> try Some (ProcessHand hand) with | InvalidHand(_) -> None)
        let nullFiltered = mapToSomeNone |> List.filter (fun processedHand -> processedHand.IsSome)
        nullFiltered |> List.map (fun hand -> hand.Value)