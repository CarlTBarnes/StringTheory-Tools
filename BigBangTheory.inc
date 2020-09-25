    OMIT('_EndOfInclude_',_IFDef_BigBangTheory_)
_IFDef_BigBangTheory_  EQUATE(1)
    INCLUDE('StringTheory.INC'),ONCE
!StringTheoryLinkMode=>1;StringTheoryDllMode=>0  or StringTheoryLinkMode=>0;StringTheoryDllMode=>1
!----------------- BigBangTheory -----------------
BigBangTheory    CLASS,TYPE,MODULE('BigBangTheory.CLW'),LINK('BigBangTheory.CLW') !,StringTheoryLinkMode),DLL(StringTheoryDllMode)
LinesViewInList     PROCEDURE(StringTheory STwithLinesSplitDone)
LinesViewSplitCSV   PROCEDURE(StringTheory STwithLinesSplitDone, BYTE pRemoveQuotes=TRUE)  
LinesViewSplitTAB   PROCEDURE(StringTheory STwithLinesSplitDone)  
LinesViewSplit      PROCEDURE(StringTheory STwithLinesSplitDone, string CsvSplitDelim,string CsvQuoteStart,string CsvQuoteEnd, BYTE pRemoveQuotes=TRUE)  
     END
    _EndOfInclude_
