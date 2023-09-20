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
        let inputAst00 = "begin run {a,c };run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = run globalParser inputAst00
        let actual = sprintf "%O" result
        let expected = """Success: Ast
  [Block (RunSequence [Run (Sequence [A; C]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestTryParseNoError () =
        // in this test, we run the parser on an syntactically correct input, 
        // returning an AST, and expect no diagnostics
        let inputNoError = "begin run {a,c };run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = tryParse globalParser "This should return the ast, since there is no error" ad inputNoError
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block (RunSequence [Run (Sequence [A; C]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestTryParseNoErrorDiag () =
        // in this test, we run the parser on an syntactically correct input, 
        // returning an AST, and expect no diagnostics
        let inputNoError = "begin run {a,c };run{a, b} end begin run{a,b,c}; run{a,b} end "
        ad.Clear()
        let result = tryParse globalParser "This should return the ast, since there is no error" ad inputNoError
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
        let inputTestEcS01 = "begin run {a,c,d,a };run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcS01
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block (RunSequence [Run (Sequence [A; C; Escape; A]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEcS01Diag () =
        // In this use case, the first charSequence contains a 'd' but expects 'a'|'b'|'c'.
        let inputTestEcS01 = "begin run {a,c,d,a };run{a, b} end begin run{a,b,c}; run{a,b} end "
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcS01
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 16),
   DiagnosticMessage "charChoice a|b|c expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)


    [<TestMethod>]
    member this.TestEcS02 () =
        // In this use case, the first charSequence contains a ',' that is not followed by some 'a'|'b'|'c'.
        let inputTestEcS02 = "begin run {a,c,b, };run{a, b} end begin run{a,b,c}; run{a,b} end "
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcS02
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block
     (RunSequence
        [Run (Sequence [A; C; B; Escape]); Run (Sequence [A; B])]);
   Block (RunSequence [Run (Sequence [A; B; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEcS02Diag () =
        // In this use case, the first charSequence contains a ',' that is not followed by some 'a'|'b'|'c'.
        let inputTestEcS02 = "begin run {a,c,b, };run{a, b} end begin run{a,b,c}; run{a,b} end "
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcS02
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 19),
   DiagnosticMessage "charChoice a|b|c expected")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestEcS03 () =
        // In this use case, the second charSequence contains a ',' that is not followed by some 'a'|'b'|'c', 
        // escaping the error is still possible, because a third, properly closed char Sequence occurs.
        let inputTestEcS03 = " begin run {a};run {a, b, ;run{a, b} end "
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcS03
        let actual = sprintf "%O" result
        let expected = """Ast [Block (RunSequence [Run (Sequence [A]); Run (Sequence [A; B; Escape; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEcS03Diag () =
        // In this use case, the second charSequence contains a ',' that is not followed by some 'a'|'b'|'c', 
        // escaping the error is still possible, because a third, properly closed char Sequence occurs.
        let inputTestEcS03 = " begin run {a};run {a, b, ;run{a, b} end "
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestEcS03
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (Parser, Error, (Ln: 1, Col: 27),
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
    member this.TestErB01 () =
        // In this use case, the second run block does not have a closing "}".
        let inputTestErB01 = "begin run {a};run a, b };run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErB01
        let actual = sprintf "%O" result
        let expected = """Ast
      [Block
         (RunSequence
            [Run (Sequence [A]); Run (Sequence [A; B]); Run (Sequence [A; B])])]""" 
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErB01Diag () =
        // In this use case, the second run block does not have a closing "}".
        let inputTestErB01 = "begin run {a};run a, b };run{a, b} end"
        ad.Clear()
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErB01
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 19), DiagnosticMessage "missing opening {")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.TestErB02 () =
        // In this use case, the second run block does not have an closing "}".
        let inputTestErB02 = "begin run {a,c,a};run a,c ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErB02
        let actual = sprintf "%O" result
        let expected = """Ast
  [Block
     (RunSequence
        [Run (Sequence [A; C; A]); Run (Sequence [A; C]); Run (Sequence [A; B])])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestErB02Diag () =
        // In this use case, the second run block does not have an closing "}".
        ad.Clear()
        let inputTestErB02 = "begin run {a,c,a};run a,c ;run{a, b} end"
        let result = tryParse globalParser "parser could not recover from errors;" ad inputTestErB02
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
      (Parser, Error, (Ln: 1, Col: 23),
       DiagnosticMessage "missing opening { and closing }")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)
