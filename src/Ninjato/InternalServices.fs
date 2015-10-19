namespace Ninjato.Engine.Services

module stringExtensions = 
    open System.Runtime.CompilerServices
    open System.Text.RegularExpressions
    
    [<Extension>]
    type StringExtension() = 
        [<Extension>]
        static member inline RemoveWhiteSpace(x : string) = 
            List.fold (fun (str : string) (tup : string * string) -> str.Replace(fst tup, snd tup)) x [ ("\r", " ")
                                                                                                        ("\n", " ") ]
            |> fun x -> Regex.Replace(x, "\s{2,}", " ")
            |> fun x -> x.Trim()

module ExpressionEvaluation = 
    open System
    open ExpressionEvaluator
    
    let private ``remove square brackets`` (token : string) = token.Replace("[]", "")
    let private ``swap double dollars for double underscores`` (token : string) = token.Replace("$", "_")
    
    let private ``format string`` token = 
        token
        |> ``remove square brackets``
        |> ``swap double dollars for double underscores``
    
    let internal ``evaluate expression`` (expression : string) (variables : Map<string, string>) = 
        let registry = new TypeRegistry()
        for pair in variables do
            registry.RegisterSymbol(``format string`` pair.Key, pair.Value)
        let compiledExpression = new CompiledExpression(``format string`` expression)
        compiledExpression.TypeRegistry <- registry
        let result = Convert.ToBoolean(compiledExpression.Eval())
        (result)

module QueryConstruction = 
    open System
    open System.Collections.Generic
    open System.Text
    open System.Text.RegularExpressions
    open stringExtensions
    open ExpressionEvaluation
    
    let internal ``custom sql constructs`` = 
        new Regex("(?<if>\${1}\?{1}\s*\({1}(\s|.)*?\){1}\s*\{{1})|(?<else>\${1}\:{1}\s*\{{1})|(?<end>\}{1})", 
                  RegexOptions.Compiled ||| RegexOptions.ExplicitCapture)
    let internal ``if construct expression`` = 
        new Regex("\$\?\s*\((?<expression>.*?)\)\s*\{", RegexOptions.Compiled ||| RegexOptions.ExplicitCapture)
    
    type Query = 
        { Sql : string
          Parameters : Map<string, Object> }
    
    type private ConstructPartInfo = 
        { Index : int
          Length : int
          Value : string }
    
    type private ConstructPart = 
        | If of ConstructPartInfo
        | Else of ConstructPartInfo
        | End of ConstructPartInfo
        member this.Unwrap = 
            match this with
            | If o | Else o | End o -> o
    
    type private Node = 
        { Head : ConstructPart
          mutable Tail : ConstructPart option
          mutable Nested : list<Node> }
    
    let private ``evaluate if construct expression`` (``construct head`` : string) (variables : Map<string, string>) = 
        let m = ``if construct expression``.Match(``construct head``)
        
        let expression = 
            match m.Success with
            | false -> failwith "could not extract expression from if construct head"
            | _ -> m.Groups.[1].Value
        
        let evaluation = ``evaluate expression`` expression variables
        evaluation
    
    let private ``build tree`` (seq : seq<ConstructPart>) (count : int) = 
        let openConstructs = new Stack<Node>(count)
        let mutable tree = []
        let mutable depth = 0
        for c in seq do
            match c with
            | If _ | Else _ -> 
                let n = 
                    { Head = c
                      Tail = None
                      Nested = [] }
                if openConstructs.Count > 0 then 
                    let parent = openConstructs.Peek()
                    parent.Nested <- parent.Nested @ [ n ]
                openConstructs.Push(n)
                depth <- depth + 1
            | End _ -> 
                let con = openConstructs.Pop()
                con.Tail <- Some(c)
                if not (depth > 1) then tree <- tree @ [ con ]
                depth <- depth - 1
            |> ignore
        tree |> List.ofSeq
    
    let private ``evaluate construct tree`` (tree : list<Node>) (csql : string) (argMap : Map<string, string>) = 
        let ``extract content`` (start : int) (stop : int) = csql.Substring(start, stop - start)
        
        let rec traverse (str : string) (sb : StringBuilder) (``previous evaluation`` : bool) 
                (``pre content start`` : int) (tree : list<Node>) (argMap : Map<string, string>) = 
            match tree with
            | [] -> sb
            | head :: tail -> 
                let constructPartHead = head.Head
                let ``construct part start`` = constructPartHead.Unwrap
                let ``construct part stop`` = head.Tail.Value
                let ``content start`` = ``construct part start``.Index + ``construct part start``.Length
                let ``content stop`` = ``construct part stop``.Unwrap.Index
                let ``pre content stop`` = ``construct part start``.Index
                sb.Append(``extract content`` ``pre content start`` ``pre content stop``) |> ignore
                let ``next pre content start`` = ``content stop`` + ``construct part stop``.Unwrap.Length
                
                let evaluation = 
                    match constructPartHead with
                    | If cpi -> ``evaluate if construct expression`` cpi.Value argMap
                    | Else _ -> not ``previous evaluation``
                    | _ -> failwith "constructPartHead can only be an if or an else"
                if evaluation then 
                    match head.Nested.Length > 0 with
                    | true -> 
                        let ``last point`` = 
                            head.Nested.[head.Nested.Length - 1].Tail.Value.Unwrap |> fun v -> v.Index + v.Length
                        traverse csql sb false ``content start`` head.Nested argMap |> ignore
                        sb.Append(``extract content`` ``last point`` ``content stop``) |> ignore
                    | false -> sb.Append(``extract content`` ``content start`` ``content stop``) |> ignore
                traverse str sb evaluation ``next pre content start`` tail argMap
        
        let str = 
            traverse csql (new StringBuilder()) false 0 tree argMap
            |> fun sb -> 
                let lastNode = tree.[tree.Length - 1].Tail.Value.Unwrap
                sb.Append(``extract content`` (lastNode.Index + lastNode.Length) csql.Length)
            |> fun sb -> sb.ToString()
            |> fun s -> s.RemoveWhiteSpace()
        
        str
    
    let private ``parameterize query`` (sql : string) (argMap : Map<string, string>) = 
        Seq.fold (fun (tup : int * string * Map<string, Object>) (kvp : KeyValuePair<string, string>) -> 
            let i, str, map = tup
            if str.Contains(kvp.Key) then 
                if kvp.Key.Contains("[]") then 
                    let ``untrimmed args`` = kvp.Value.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
                    
                    let args = 
                        query { 
                            for a in ``untrimmed args`` do
                                let arg = a.Trim()
                                select arg
                        }
                        |> Seq.mapi (fun index arg -> (sprintf "@p%i" (i + index), box arg))
                        |> Map.ofSeq
                    
                    let replacement = 
                        query { 
                            for kvp in args do
                                select kvp.Key
                        }
                        |> Array.ofSeq
                        |> fun arr -> String.Join(",", arr)
                    
                    ((i + args.Count), str.Replace(kvp.Key, replacement), args)
                else ((i + 1), str.Replace(kvp.Key, sprintf "@p%i" i), map.Add(sprintf "@p%i" i, box kvp.Value))
            else tup) <| (1, sql, Map.empty) <| argMap |> fun tup -> 
            let i, str, map = tup
            { Sql = str
              Parameters = map }
    
    let private toMap dictionary = 
        (dictionary :> seq<_>)
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq

    let ConstructQuery (csql : string) (argMap : IEnumerable<KeyValuePair<string, string>>) = 
        let str = csql.RemoveWhiteSpace()
        let ``construct parts`` = ``custom sql constructs``.Matches(str)
        let ``number of constructs`` = ``construct parts``.Count
        let argMap = argMap |> toMap
        
        let ``parameterized sql with params`` = 
            ``construct parts``
            |> Seq.cast<Match>
            |> Seq.map (fun m -> 
                   match m with
                   | x when x.Groups.["if"].Success -> 
                       If({ Index = m.Index
                            Length = m.Length
                            Value = m.Value })
                   | x when x.Groups.["else"].Success -> 
                       Else({ Index = m.Index
                              Length = m.Length
                              Value = m.Value })
                   | x when x.Groups.["end"].Success -> 
                       End({ Index = m.Index
                             Length = m.Length
                             Value = m.Value })
                   | _ -> failwith "Construct not recognized")
            |> ``build tree``
            <| ``number of constructs``
            |> ``evaluate construct tree``
            <| str
            <| argMap
            |> ``parameterize query``
            <| argMap
        ``parameterized sql with params``
