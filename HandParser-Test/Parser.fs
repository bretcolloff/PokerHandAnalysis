namespace HandParser.Tests

open FSHHP.Parser
open FsUnit.MsTest
open Microsoft.VisualStudio.TestTools.UnitTesting
open NHamcrest.Core
open TestHelpers

/// Tests on functions from the Parser.fs part of the HandProcessor 
[<TestClass>] 
type Parser() =
    let ParserTestAssetsDirectory =
        CurrentDirectory + @"/TestAssets/ParserTests"

    /// Check that the site name is correct in the metadata.
    [<TestMethod>] 
    member test.CorrectSiteName() =
        let hand = ReadHands (ParserTestAssetsDirectory + @"/CorrectSiteName.txt") |> List.head
        let metaData = hand.metaData
        Assert.IsTrue(metaData.site = "PokerStars", (sprintf "Site name was actually: %s" metaData.site))