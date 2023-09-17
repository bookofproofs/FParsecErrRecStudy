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
        // in this test, we run the parser on an syntactically correct input, 
        // returning an AST, and expect no diagnostics
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
        // from which the parser cannot recover,
        // returning an AST that should be ERROR, and expect a diagnostic saying it
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
    member this.TestEcS01 () =
        // In this use case, the first charSequence contains a 'd' but expects 'a'|'b'|'c'.
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
    member this.TestEsS02 () =
        // In this use case, the first charSequence contains a ',' that is not followed by some 'a'|'b'|'c'.
        ad.Clear()
        let input = "begin run {a,c,b, };run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = tryParse globalParser "parser could not recover from errors;" ad input
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block
     (RunSequence
        [Run (Sequence [A; C; B; Escape]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 19),
   DiagnosticMessage "charChoice a|b|c expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)


    [<TestMethod>]
    member this.TestTryParseError03 () =
        // in this test, the use case is to escape from the context of 
        // not properly formed runBlocks
        // from which the parser should be able to recover,
        // returning an AST that contains an AST with some escape nodes, 
        // and we expect a diagnostic saying why and where these escape nodes occurred
        ad.Clear()
        let input = "begin run {a};xxx {a, b};run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = tryParse globalParser "parser could not recover from errors;" ad input
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block (RunSequence [Run (Sequence [A]); Escape; Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 15), DiagnosticMessage "run block expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestTryParseError04 () =
        // in this test, the use case is similar to TestTryParseError03 but containing 
        // a run block that is not properly opened,
        // from which the parser should be able to recover,
        // returning an AST that contains an AST with some escape nodes, 
        // and we expect a diagnostic saying why and where these escape nodes occurred
        ad.Clear()
        let input = "begin run {a};run a, b};run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = tryParse globalParser "parser could not recover from errors;" ad input
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block (RunSequence [Run (Sequence [A]); Escape; Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, actual);
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 15), DiagnosticMessage "run block expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestTryParseError05 () =
        // in this test, the use case is similar to TestTryParseError03 but containing 
        // a run block that is not properly closed,
        // from which the parser should be able to recover,
        // returning an AST that contains an AST with some escape nodes, 
        // and we expect a diagnostic saying why and where these escape nodes occurred
        ad.Clear()
        let input = "begin run {a};run{a, b ;run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = tryParse globalParser "parser could not recover from errors;" ad input
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block (RunSequence [Run (Sequence [A]); Escape; Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, actual);
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 15), DiagnosticMessage "run block expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)
