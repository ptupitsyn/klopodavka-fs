﻿namespace Klopodavka.Game.Tests

open Expecto

module Main =
    [<EntryPoint>]
    let main argv =
        Tests.runTestsInAssembly defaultConfig argv
