namespace HandParser.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

/// Contains helper methods for running tests.
[<TestClass>]
module TestHelpers =
    /// Store the current directory for use in tests.
    let CurrentDirectory = 
        Directory.GetCurrentDirectory()

    /// Check that 2 provided collections are equal.
    let public CollectionsAreEqual (collectionA: 'a list, collectionB: 'a list) =
        Assert.IsFalse (collectionA.Length <> collectionB.Length, "Collections not of the same length.")

        let zip = List.zip collectionA collectionB
        for (itemA, itemB) in zip do
            Assert.IsFalse (itemA <> itemB, "Collections don't match.")