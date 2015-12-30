// See the file alphabet-cipher.md for detailed information.

let listChars = 
    seq { for c in [(int)'a'..(int)'z'] do 
            yield c |> char } 
    |> List.ofSeq

let shiftList i l =
    List.append (List.skip i l) (List.take i l)

let buildEncodingType row col result = (row,col),result
let buildDecodingType row col result = (row,result),col
let buildDecipherType row col result = (result,col),row

let buildCodingTable buildCodingType = 
    List.mapi (fun io co -> List.mapi (fun ii ci -> buildCodingType co listChars.[ii] ci) (shiftList io listChars)) listChars 
    |> List.concat
    |> Map.ofList

let encodingTable = buildCodingTable buildEncodingType

let buildDecodingTable = buildCodingTable buildDecodingType

let buildDecipherTable = buildCodingTable buildDecipherType

let rec repeatWord i (w:string) =
    let n = w.Length
    if n = 0  then w
    else if i <= n then w.Substring(0,i)
    else w + repeatWord (i - n) w

let explode (s:string) =
    [for c in s -> c]

let implode (xs:char list) =
    let sb = System.Text.StringBuilder(xs.Length)
    xs |> List.iter (sb.Append >> ignore)
    sb.ToString()

let applyCode secret (msg:string) (t : Map<(char*char),char>) =
    List.zip (explode (repeatWord msg.Length secret)) (explode msg)
    |> List.map (fun k -> t.[k]) 
    |> implode

let shortest (msg:string) =     
    let rec findWord i =
        let n = msg.Length
        let s = msg.Substring(0,i)        
        if i = n then msg
        else if repeatWord n s = msg then s
        else findWord (i+1)
    findWord 1

type Message = string
type Keyword = string

let encode (key:Keyword) (message:Message) : Message =
    applyCode key message encodingTable

let decode (key:Keyword) (message:Message) : Message =
    applyCode key message buildDecodingTable

let decipher (cipher:Message) (message:Message) : Keyword =
    shortest (applyCode cipher message buildDecipherTable)

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
