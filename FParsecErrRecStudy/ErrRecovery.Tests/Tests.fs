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
        // in this test, we just run the parser on an syntactically correct input and return an AST
        let input = "begin run {a,c };run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = run globalParser input
        let actual = sprintf "%O" result
        let expected = """Success: Ast
  [Block (RunSequence [Run (Sequence [A; C]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestTryParseNoError () =
        // in this test, we run the parser on an syntactically correct input 
        //, return an AST, and expect no diagnostics
        ad.Clear()
        let input = "begin run {a,c };run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = tryParse globalParser "This should return the ast, since there is no error" ad input
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block (RunSequence [Run (Sequence [A; C]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestTryParseError00 () =
        // in this test, we run the parser on an syntactically incorrect input 
        // from which the parser cannot recover
        //, return an AST that should be ERROR, and expect a diagnostic saying it
        ad.Clear()
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



    [<TestMethod>]
    member this.TestTryParseError01 () =
        // in this test, we run the parser on an syntactically incorrect input 
        // from which the parser can recover
        //, return an AST that should be an AST with some escape nodes, 
        // and we expect a diagnostic saying why and where there were escape nodes
        ad.Clear()
        let input = "begin run {a,c,d,a };run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = tryParse globalParser "parser could not recover from errors;" ad input
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block (RunSequence [Run (Sequence [A; C; Escape; A]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 16),
   DiagnosticMessage "charChoice a|b|c expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)


    [<TestMethod>]
    member this.TestTryParseError02 () =
        // in this test, we run the parser on an syntactically incorrect input 
        // from which the parser can recover
        //, return an AST that should be an AST with some escape nodes, 
        // and we expect a diagnostic saying why and where there were escape nodes
        ad.Clear()
        let input = "begin run {a,c,d,a, };run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = tryParse globalParser "parser could not recover from errors;" ad input
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block
     (RunSequence
        [Run (Sequence [A; C; Escape; A; Escape]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 16),
   DiagnosticMessage "charChoice a|b|c expected")
Diagnostic
  (Parser, Error, (Ln: 1, Col: 21),
   DiagnosticMessage "charChoice a|b|c expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)
