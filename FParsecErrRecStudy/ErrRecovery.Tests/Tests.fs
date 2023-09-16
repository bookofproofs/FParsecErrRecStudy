namespace ErrRecovery.Tests
open FParsec
open ErrRecovery
open ParserExamples
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestErrRecovery () =
    /// a helper function to replace any whitespace from the string representation of AST
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestAst00 () =
        let input = "begin run {a,c };run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = run globalParser input
        let actual = sprintf "%O" result
        let expected = """Success: Ast
  [Block (RunSequence [Run (Sequence [A; C]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestTryParseNoError () =
        let input = "begin run {a,c };run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = tryParse globalParser "This should return the ast, since there is no error" ad input
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block (RunSequence [Run (Sequence [A; C]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestTryParseError () =
        let input = "begin run {a,c };run{a, b} end xxxbegin run{a,b,c}; run{a,b} end "
        let result = tryParse globalParser "parser could not recover from errors;" ad input
        let actual = sprintf "%O" result
        let expected = """Error"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 32),
   DiagnosticMessage
     "parser could not recover from errors; Error in Ln: 1 Col: 32
begin run {a,c };run{a, b} end xxxbegin run{a,b,c}; run{a,b} end 
                               ^
Expecting: end of input or 'begin'
")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)
