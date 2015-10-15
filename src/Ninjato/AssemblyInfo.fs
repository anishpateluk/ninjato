namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Ninjato")>]
[<assembly: AssemblyProductAttribute("Ninjato")>]
[<assembly: AssemblyDescriptionAttribute("Reporting Service Application")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
