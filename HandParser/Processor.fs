namespace FSHHP

open System
open System.IO

module Processor =
    // Default value for when a player has mucked their cards without showing.
    let NoCardValue = "X"
    
    // Returns the profit/loss of the provided player in the provided hand.
    let MoneyDifference (playerName: string, hand: FSHHP.Parser.HandHistory) =
        let player = hand.players |> List.find (fun x -> x.name = playerName)
          
        // Get all the players actions
        let actions = hand.streets |> List.map (fun street -> street.actions)
                                   |> List.concat

        // Get the information from the summary
        let summary = hand.streets |> List.filter (fun street -> street.streetType = Parser.Summary)
                                   |> List.map (fun playerAction -> playerAction.actions)
                                   |> List.concat
                                   |> List.find (fun action -> action.player = player)
        
        // Did the provided player win?
        let won = match summary.action with
                  | Parser.CollectFromPot(amount) -> true
                  | Parser.CollectUncalled(amount) -> true
                  | Parser.ShowedAndWon(hand, amount) -> true
                  | _ -> false

        let ApplyOpToActions (playerActions: Parser.PlayerAction list) op = 
            playerActions |> List.map (fun action -> match action.action with
                                                     | Parser.Posts(amount) -> op amount
                                                     | Parser.Bet(amount) -> op amount
                                                     | Parser.Call(amount) -> op amount
                                                     | Parser.Raise(amount, total) -> op total
                                                     | _ -> 0.0)
                          |> List.sum
                          |> (fun sum -> Math.Round(sum, 2))

        if won then
            let playerActions = actions |> List.filter (fun action -> action.player <> player)
            (ApplyOpToActions playerActions (fun x -> x)) - hand.result.rake             
        else
            let playerActions = actions |> List.filter (fun action -> action.player = player)
            ApplyOpToActions playerActions (fun x -> 0.0 - x)
                    
    // Cards of player in the provided hand
    let PlayerCards (playerName: string) (hand: FSHHP.Parser.HandHistory) =
        let actions = hand.streets |> List.map (fun street -> street.actions)
                                   |> List.concat
                                   |> List.filter (fun action -> action.player.name = playerName)
        actions |> List.tryPick (fun x -> match x.action with
                                          | FSHHP.Parser.Action.MucksAndShows(h) -> Some(h)
                                          | FSHHP.Parser.Action.ShowedAndLost(h) -> Some(h)
                                          | FSHHP.Parser.Action.ShowedAndWon(h, _) -> Some(h)
                                          | _ -> None)
                |> (fun h -> if h.IsSome then h.Value
                             else { left = NoCardValue; right = NoCardValue })

    // Filters the provided hands to one's only the provided player was involved in.
    let PlayerInvolvedHands (playerName: string, hands: FSHHP.Parser.HandHistory list) =
        hands |> List.filter (fun hand -> [for player in hand.players do 
                                           if player.name = playerName then yield player] |> Seq.length > 0)

    // Takes a list of results and creates a cumulative graph
    let public CreateGraphPoints (resultList: float list) =
        let rec CreateResultList (resultList: float list, outputList: float list, acc: float) =
            match resultList with
            | [] -> outputList
            | head :: [] -> outputList@[acc + head]
            | head :: tail -> CreateResultList (tail, outputList@[acc + head], acc + head)

        CreateResultList (resultList, List.empty, 0.0)

    // Loads all of the hand histories in the provided folder.
    let LoadHandsFromFolder folderName =
        let rec GetFiles directory =
                seq { yield! Directory.GetFiles(directory, "*.txt")
                      for subDir in Directory.GetDirectories(directory) do yield! GetFiles subDir }

        GetFiles folderName |> Seq.toList
                            |> List.map (fun file -> FSHHP.Parser.ReadHands file)
                            |> List.concat

    // Returns true if the action isn't a 'Fold' action.
    let MatchWithNotFold (action: FSHHP.Parser.PlayerAction) = 
        match action.action with
        | FSHHP.Parser.Action.Fold(_) -> false
        | _ -> true

    // The percentage of hands where it doesn't just fold around to the blinds.
    let ActionInSample (player: FSHHP.Parser.Player, hands: FSHHP.Parser.HandHistory list, filterAction) = 
        let total = List.length hands
        let handsWithAction = hands |> List.filter (fun (hand: FSHHP.Parser.HandHistory) -> filterAction hand)
                                    |> List.length
        
        ((float)handsWithAction / (float)total) * 100.0

    // Filters the hands based on a specific type of action by the specified player.
    let FilterPlayerHandsOnActionMatch (player: FSHHP.Parser.Player, filterAction) (hand: FSHHP.Parser.HandHistory) = 
        let playerActions = hand.streets |> List.map (fun street -> street.actions)
                                         |> List.concat
                                         |> List.filter (fun action -> action.player = player)

        playerActions |> List.filter (fun action -> filterAction action)
                      |> List.length <> 0

    // Work out the players VPIP from the collection of hands.
    let VPIP (player: FSHHP.Parser.Player, hands: FSHHP.Parser.HandHistory list) = 
        let filterAction = FilterPlayerHandsOnActionMatch(player, MatchWithNotFold)
        ActionInSample (player, hands, filterAction)
