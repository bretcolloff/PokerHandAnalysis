namespace HandParser.Tests

open FSHHP.Processor
open FSHHP.Parser
open FsUnit.MsTest
open Microsoft.VisualStudio.TestTools.UnitTesting
open NHamcrest.Core
open TestHelpers

/// Tests on functions from the Processor.fs part of the HandProcessor 
[<TestClass>]
type Processor ()=

    /// The test asset directory for processor tests.
    let ProcessorTestAssetsDirectory =
        CurrentDirectory + @"/TestAssets"

    /// The test asset directory for processer filter tests.
    let FilterTestAssetsDir =
        ProcessorTestAssetsDirectory + @"/FilterTests"

    /// The test asset directory for processor moneydiff tests.
    let MoneyDiffTestAssetsDir =
        ProcessorTestAssetsDirectory + @"/MoneyDiffTests"

    /// The test asset directory for processor get cards tests.
    let CardTestAssetsDir =
        ProcessorTestAssetsDirectory + @"/CardsTests"

    /// Test CreatesGraphPoints with 0 items.
    [<TestMethod>] 
    member test.CreateGraphZeroPoints() =
        let points = FSHHP.Processor.CreateGraphPoints []
        CollectionsAreEqual (points, List.empty<float>)

    /// Test CreatesGraphPoints with 10 items.
    [<TestMethod>]
    member test.CreateGraphTenPoints() =
        let input = [0.0; 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0]
        let output = [0.0; 1.0; 3.0; 6.0; 10.0; 15.0; 21.0; 28.0; 36.0; 45.0]
        let points = CreateGraphPoints input
        CollectionsAreEqual (points, output)

    /// Test CreatesGraphPoints with 10 25c inputs.
    [<TestMethod>]
    member test.CreateGraphTenQuarters() =
        let input = [0.25;0.25;0.25;0.25;0.25;0.25;0.25;0.25;0.25;0.25]
        let output = [0.25;0.50;0.75;1.0;1.25;1.50;1.75;2.0;2.25;2.50]
        let points = CreateGraphPoints input
        CollectionsAreEqual (points, output)

    /// Test filtering hands with a non existant player doesnt explode.
    [<TestMethod>]
    member test.FilterForFakePlayer() =
        let hands = LoadHandsFromFolder FilterTestAssetsDir
        let fakePlayerName = "64yu5jyr5h"
        let filtered = PlayerInvolvedHands (fakePlayerName, hands)
        Assert.IsTrue (filtered.Length = 0, "Incorrectly identified a player.")

    /// Test filtering hands with a real player returns the correct amount.
    [<TestMethod>]
    member test.FilterForRealPlayer() = 
        let hands = LoadHandsFromFolder FilterTestAssetsDir
        let playerName = "BaronMcCool"
        let filtered = PlayerInvolvedHands (playerName, hands)
        Assert.IsTrue (filtered.Length = 13, (sprintf "Actual hand count was: %d" filtered.Length))

    /// Test filtering hands with an upper variant of a real player returns nothing.
    [<TestMethod>]
    member test.FilterForUpperCased() =
        let hands = LoadHandsFromFolder FilterTestAssetsDir
        let playerName = "BARONMCCOOL"
        let filtered = PlayerInvolvedHands (playerName, hands)
        Assert.IsTrue (filtered.Length = 0, (sprintf "Actual hand count was: %d" filtered.Length))

    /// Test filtering hands with a subset of a real players name returns nothing.
    [<TestMethod>]
    member test.FilterForPlayerNameSubset() =
        let hands = LoadHandsFromFolder FilterTestAssetsDir
        let playerName = "BaronM"
        let filtered = PlayerInvolvedHands (playerName, hands)
        Assert.IsTrue (filtered.Length = 0, (sprintf "Actual hand count was: %d" filtered.Length))

    /// Loading from a folder loads the correct number of hands.
    [<TestMethod>]
    member test.FolderLoaderCorrectCount() =
        let hands = LoadHandsFromFolder FilterTestAssetsDir
        Assert.IsTrue (hands.Length = 13, (sprintf "Actual hand count was: %d" hands.Length))

    /// Loading from a folder with a large tree of subfolders loads the correct number of hands.
    [<TestMethod>]
    member test.FolderLoaderLargeTreeCorrectCount() = 
        let hands = LoadHandsFromFolder (ProcessorTestAssetsDirectory + @"/LargeTree")
        Assert.IsTrue (hands.Length = 9, (sprintf "Actual hand count was: %d" hands.Length))

    /// Loading a from a folder that contains a mix of valid and invalid .txt files, correct number of hands returned.
    [<TestMethod>]
    member test.FolderLoaderMixedFiles() =
        let hands = LoadHandsFromFolder (ProcessorTestAssetsDirectory + @"/MixedFiles")
        Assert.IsTrue (hands.Length = 1, (sprintf "Actual hand count was: %d" hands.Length))

    /// Check correct output for a non-hero player who had a 0 outcome.
    [<TestMethod>]
    member test.ZeroDifferenceForPlayer() =
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/NonHeroPlayer.txt") |> List.head
        let diff = MoneyDifference ("shpektorov", hand)
        Assert.IsTrue ((diff) = 0.0, (sprintf "Actual difference was: %f" (diff)))
        
    /// Check correct ouput for a non-hero player who had a positive outcome.
    [<TestMethod>]
    member test.PositiveDifferenceForPlayer() = 
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/NonHeroPlayer.txt") |> List.head
        let diff = MoneyDifference ("speed1D", hand)
        Assert.IsTrue ((diff) = 0.15, (sprintf "Actual difference was: %f" (diff)))

    /// Check correct output for a non-hero player who had a negative outcome.
    [<TestMethod>]
    member test.NegativeDifferenceForPlayer() =
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/NonHeroPlayer.txt") |> List.head
        let diff = MoneyDifference ("razy77razy", hand)
        Assert.IsTrue ((diff) = -0.05, (sprintf "Actual difference was: %f" (diff)))

    /// Check correct output for a hero player who had a 0 outcome.
    [<TestMethod>]
    member test.NeutralDifferenceForHero() =
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/Hero0Outcome.txt") |> List.head
        let diff = MoneyDifference ("BaronMcCool", hand)
        Assert.IsTrue ((diff) = 0.0, (sprintf "Actual difference was: %f" (diff)))

    /// Check correct output for a hero player who had a positive outcome.
    [<TestMethod>]
    member test.PositiveDifferenceForHero() =
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/HeroPosOutcome.txt") |> List.head
        let diff = MoneyDifference ("BaronMcCool", hand)
        Assert.IsTrue ((diff) = 0.02, (sprintf "Actual difference was: %f" (diff)))

    /// Check correct output for a hero player who had a negative outcome.
    [<TestMethod>]
    member test.NegativeDifferenceForHero() =
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/HeroNegOutcome.txt") |> List.head
        let diff = MoneyDifference ("BaronMcCool", hand)
        Assert.IsTrue ((diff) = -0.10, (sprintf "Actual difference was: %f" (diff)))

    /// Check correct output for a player who was the big blind at the start of the hand.
    [<TestMethod>]
    member test.CorrectWhenHeroIsBigBlind() = 
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/HeroBigBlind.txt") |> List.head
        let diff = MoneyDifference ("BaronMcCool", hand)
        Assert.IsTrue ((diff) = -0.10, (sprintf "Actual difference was: %f" (diff)))

    /// Check correct output for a player who was the small blind at the start of the hand.
    [<TestMethod>]
    member test.CorrectWhenHeroIsSmallBlind() = 
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/HeroSmallBlind.txt") |> List.head
        let diff = MoneyDifference ("BaronMcCool", hand)
        Assert.IsTrue ((diff) = -0.05, (sprintf "Actual difference was %f" (diff)))

    // Check correct ouput for a non-hero player who was the big blind at the start of the hand.
    [<TestMethod>]
    member test.CorrectWhenNonHeroIsBigBlind() =
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/NonHeroBigBlind.txt") |> List.head
        let diff = MoneyDifference ("I dinner41 I", hand)
        Assert.IsTrue((diff) = -0.10, (sprintf "Actual difference was %f" (diff)))

    // Check correct output for a non-hero player who was the small blind at the start of the hand.
    [<TestMethod>]
    member test.CorrectWhenNonHeroIsSmallBlind() =
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/NonHeroSmallBlind.txt") |> List.head
        let diff = MoneyDifference ("I dinner41 I", hand)
        Assert.IsTrue ((diff) = -0.05, (sprintf "Actual difference was: %f" (diff)))

    /// Check correct output for a hand where it was folded preflop - check both players.
    [<TestMethod>]
    member test.CorrectBlindPlayersWhenEndedPreflop() =
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/CorrectBlindsWhenEndedPreflop.txt") |> List.head
        let sbDiff = MoneyDifference ("shpektorov", hand)
        let bbDiff = MoneyDifference ("razy77razy", hand)
        Assert.IsTrue((sbDiff) = -0.05 && (bbDiff) = 0.05, (sprintf "Actual values were: sb = %f, bb = %f" (sbDiff) (bbDiff)))

    /// Check correct ouput for a hand where it was raised and reraised - check both players.
    [<TestMethod>]
    member test.CorrectInRaiseWars() =
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/CorrectInRaiseWar.txt") |> List.head
        let winnerDiff = MoneyDifference ("Wilderer", hand)
        let loserDiff = MoneyDifference ("BaronMcCool", hand)
        Assert.IsTrue((winnerDiff) = 1.05 && (loserDiff) = -1.0, (sprintf "Actual differences were: winner %f, loser %f" (winnerDiff) (loserDiff)))

    /// Check correct output for a hand where a player was all in and won.
    [<TestMethod>]
    member test.CorrectForWinningPlayerAllIn() =
        let hand = ReadHands (MoneyDiffTestAssetsDir + @"/WinningPlayerAllIn.txt") |> List.head
        let diff = MoneyDifference ("gorechin1986", hand)
        Assert.IsTrue((diff) = 3.88, (sprintf "Actual difference was: %f" (diff)))

    /// Check correct output for a hand where a player was all in and lost.
    [<TestMethod>]
    member test.CorrectForLosingPlayerAllIn() =
        let hands = ReadHands (MoneyDiffTestAssetsDir + @"/CorrectForLosingPlayerAllIn.txt")
        let hand = hands |> List.head
        let diff = MoneyDifference ("mazoumpas", hand)
        Assert.IsTrue((diff) = -3.66, (sprintf "Actual difference was: %f" (diff)))

    /// Check correct output when getting the cards for a player who doesnt show.
    [<TestMethod>]
    member test.CardsForPlayerWhoDoesntShow() =  
        let hand = ReadHands (CardTestAssetsDir + @"/DoesntShow.txt") |> List.head
        let cards = PlayerCards "drbull88" hand
        Assert.IsTrue(cards.left = NoCardValue && cards.right = NoCardValue, "Card values unexpected.")

    /// Check correct output when getting the cards for a player who shows and mucks.
    [<TestMethod>]
    member test.CardsForPlayerWhoShowsAndMucks() =
        let hand = ReadHands (CardTestAssetsDir + @"/ShowedAndMucked.txt") |> List.head
        let cards = PlayerCards "shpektorov" hand
        Assert.IsTrue(cards.left = "Ac" && cards.right = "Jc", "Card values unexpected.")

    /// Check correct output when getting the cards for a player who shows and lost.
    [<TestMethod>]
    member test.CardsForPlayerWhoShowsAndLost() = 
        let hand = ReadHands (CardTestAssetsDir + @"/ShowedAndLost.txt") |> List.head
        let cards = PlayerCards "Kolesia1970" hand
        Assert.IsTrue(cards.left = "Ts" && cards.right = "8s", "Card values unexpected.")

    /// Check correct output when getting the cards for a player who shows and won.
    [<TestMethod>]
    member test.CardsForPlayerWhoShowsAndWon() =
        let hand = ReadHands (CardTestAssetsDir + @"/ShowedAndWon.txt") |> List.head
        let cards = PlayerCards "speed1D" hand
        Assert.IsTrue(cards.left = "Ks" && cards.right = "Qd", "Card values unexpected.")