// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#load ".fake/build.fsx/intellisense.fsx"

#if !FAKE
#r "netstandard"
#endif

open System

open Fake.SystemHelper
open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Api

// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------

let buildDir  = "./build/"
let dotnetSdkVersion = "3.1.101"

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------

Target.create "Clean" (fun _ ->
    [buildDir]
    |> Shell.cleanDirs
)

// Lazily install DotNet SDK in the correct version if not available
let installedSdk =
    lazy DotNet.install (fun cfg ->
        { cfg with
            Version = DotNet.CliVersion.Version dotnetSdkVersion
        }) 

// Set general properties without arguments
let inline setDotNetOptions arg = DotNet.Options.lift installedSdk.Value arg

Target.create "InstallDotNetCLI" (fun _ ->
    installedSdk.Force()
    |> ignore
)

Target.create "Restore" (fun _ ->
    DotNet.restore setDotNetOptions "DomainModelingMadeFunctional.sln"
)

Target.create "Build" (fun _ -> 
    DotNet.exec setDotNetOptions "build" "DomainModelingMadeFunctional.sln"
    |> ignore
)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------

"Clean"
   ==> "InstallDotNetCLI"
   ==> "Restore"
   ==> "Build"

Target.runOrDefaultWithArguments "Build"