    OMIT('_EndOfInclude_',_IFDef_BigBangSystemString_)
_IFDef_BigBangSystemString_  EQUATE(1)
    INCLUDE('SystemString.INC'),ONCE  
!----------------- BigBangSystemString -----------------
BigBangSystemString    CLASS,TYPE,MODULE('BigBangSystemString.CLW'),LINK('BigBangSystemString.CLW') !,_ABCLinkMode_),DLL(_ABCDllMode_)
LinesViewInList     PROCEDURE(SystemStringClass SysStrwithLinesSplitDone)
LinesViewSplitCSV   PROCEDURE(SystemStringClass STwithLinesSplitDone, BYTE NoSupport_RemoveQuotes=TRUE)  
LinesViewSplitTAB   PROCEDURE(SystemStringClass STwithLinesSplitDone)
    !AFAIK SystemStringClass does NOT support the CSV with Quotes e.g. "Tony Fauci, MD"  
LinesViewSplit      PROCEDURE(SystemStringClass STwithLinesSplitDone, string CsvSplitDelim)
LinesViewSplit      PROCEDURE(SystemStringClass STwithLinesSplitDone, string CsvSplitDelim,string NoSupport_CsvQuoteStart,string NoSupport_CsvQuoteEnd, BYTE NoSupport_RemoveQuotes=TRUE)  
     END
    _EndOfInclude_
