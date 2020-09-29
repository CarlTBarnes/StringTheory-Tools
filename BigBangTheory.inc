    OMIT('_EndOfInclude_',_IFDef_BigBangTheory_)
_IFDef_BigBangTheory_  EQUATE(1) 
!----------------------------------------------------------------------------
! BigBangTheory - View StringTheory Lines Queue and Optionally Split lines by Delimeter
! (c)2020 by Carl T. Barnes - Released under the MIT License 
! Download: https://github.com/CarlTBarnes/StringTheory-LoadFile-Split-Viewer
!----------------------------------------------------------------------------
    INCLUDE('StringTheory.INC'),ONCE
!StringTheoryLinkMode=>1;StringTheoryDllMode=>0  or StringTheoryLinkMode=>0;StringTheoryDllMode=>1
!----------------- BigBangTheory -----------------
BigBangTheory    CLASS,TYPE,MODULE('BigBangTheory.CLW'),LINK('BigBangTheory.CLW') !,StringTheoryLinkMode),DLL(StringTheoryDllMode)
DoNotShow           BOOL     !Class is for Developers so =True doesn't show any Windows

ValueView           PROCEDURE(StringTheory STtoViewGetValue, <STRING WindowCaption>)
StringView          PROCEDURE(STRING StrValue, <STRING WindowCaption>) 
WrapView            PROCEDURE(StringTheory STforWrap, <STRING WindowCaption>, Bool pOpenWrapped=false)

LinesViewInList     PROCEDURE(StringTheory STwithLinesSplitDone)
LinesViewSplitCSV   PROCEDURE(StringTheory STwithLinesSplitDone, BYTE pRemoveQuotes=TRUE)  
LinesViewSplitTAB   PROCEDURE(StringTheory STwithLinesSplitDone)  
LinesViewSplit      PROCEDURE(StringTheory STwithLinesSplitDone, string CsvSplitDelim,string CsvQuoteStart,string CsvQuoteEnd, BYTE pRemoveQuotes=TRUE)  

HexDump             PROCEDURE(long StringAddr, Long StringSize),STRING
     END
!-------------------------- How to Use: ------------------------------------
!
!INCLUDE('BigBangTheory.INC'),ONCE      !Global include class
!
!Bang BigBangTheory                     !Local declare BigBang object 
!ST   StringTheory
!
! Bang.DoNotShow=CHOOSE(~DebugST)   !<-- Only show BigBang if DebugST=True
!
! ST.LoadFile('EmpPos2019.csv')
!   Bang.ValueView(ST)              !<-- View ST.GetValue() of file loaded
!
! ST.Split('<13,10>')   
!   Bang.LinesViewInList(ST)               !<-- View Lines Q split by 13,10 in LIST
!   Bang.LinesViewSplitCSV(ST)             !<-- View split again as CSV
!   Bang.LinesViewSplit(ST,CHR(9),'','')   !<-- View using any split specs
!
!   Lne.SetValue(ST.GetLine(X))
!     Bang.ValueView(Lne)           !<-- View Len.GetValue() of line
!   Lne.Split(',','"','"', True) 
!     Bang.LinesViewInList(Lne)     !<-- View split lines queue in list
!
!----------------------------------------------------------------------------
    _EndOfInclude_