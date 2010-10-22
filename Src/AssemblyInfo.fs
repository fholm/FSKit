namespace FSKit

module Version =
  let [<Literal>] Major = 0
  let [<Literal>] Minor = 0
  let [<Literal>] Build = 2
  let [<Literal>] Revision = 0
  let [<Literal>] Tag = "alpha"
  let [<Literal>] String = "0.0.2.0"
  let Tupled = (0, 0, 2, 0, Tag)
  let Tagged = sprintf "%s-%s" String Tag
  let FullName = sprintf "FSKit %s" String

open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
 
[<assembly: AssemblyTitle("FSKit")>]
[<assembly: AssemblyDescription("FSKit - A collection of utility modules for F#")>]
[<assembly: AssemblyConfiguration("")>]
[<assembly: AssemblyCompany("IronJS")>]
[<assembly: AssemblyProduct("FSKit")>]
[<assembly: AssemblyCopyright("Copyright © Fredrik Holmström, 2010")>]
[<assembly: AssemblyTrademark("")>]
[<assembly: AssemblyCulture("")>]
 
[<assembly: ComVisible(false)>]
[<assembly: Guid("4fd0ab31-fccb-4210-83df-2b165c192083")>]

[<assembly: AssemblyVersion(Version.String)>]
[<assembly: AssemblyFileVersion(Version.String)>]

()