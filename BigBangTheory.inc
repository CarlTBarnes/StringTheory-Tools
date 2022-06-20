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
LineViewInConsolas  BOOL     !Use Consolas fixed width font for LinesView... nice for files where columns of data align   06/01/22

ValueView           PROCEDURE(StringTheory STtoViewGetValue, <STRING WindowCaption>)        
SliceView           PROCEDURE(StringTheory STtoViewSlice, Long SliceStart=1, Long SliceEnd=0, <STRING CapTxt>) !View Slice() of ST
SubView             PROCEDURE(StringTheory STtoViewSub  , Long SubStart=1,  Long SubLength=1, <STRING CapTxt>) !View Sub() of ST

StringView          PROCEDURE(*STRING StrValue, <STRING WindowCaption>) 
StringView          PROCEDURE(STRING  StrValue, <STRING WindowCaption>) 

WrapView            PROCEDURE(StringTheory STforWrap, <STRING WindowCaption>, Bool pOpenWrapped=false)

LinesViewInList     PROCEDURE(StringTheory STwithLinesSplitDone, <STRING WindowCaption>)
LinesViewSplitCSV   PROCEDURE(StringTheory STwithLinesSplitDone, BYTE RemoveQuotes=False)  
LinesViewSplitTAB   PROCEDURE(StringTheory STwithLinesSplitDone)  
LinesViewSplit      PROCEDURE(StringTheory STwithLinesSplitDone, STRING SplitDelim, <STRING QuoteStart>,<STRING QuoteEnd>, BYTE RemoveQuotes=TRUE ,|
                                                bool pClip=false, bool pLeft=false, <STRING pSeparator>, long pNested=false)

!!! <summary>Reflection of StringTheory object to show all properties. Buttons allow accessing some methods like Value, Sub, Slice, Lines. </summary>
!!! <param name="STtoReflect"     > 1. StringTheory object to show Reflection  </param>
!!! <param name="WindowCaption"   > 2. STRING Caption for Window - Optional    </param>
!!! <param name="bIgnoreDoNotShow"> 3. BOOL   Always show, ignore property .DoNotShow - Default=False  </param>
!!! <remarks>New method added 6/4/2022. If you wonder what is inside ST this will show you. </remarks>
Reflection          PROCEDURE(StringTheory STtoReflect, <STRING WindowCaption>, BOOL bIgnoreDoNotShow=False)

ReplaceView         PROCEDURE(StringTheory St4Replace, string pOldValue, string pNewValue, long pCount=0, long pStart=1, long pEnd=0, long pNoCase=0, bool pRecursive=false),Long,Proc
ParmsView           PROCEDURE(String P1,<String P2>,<String P3>,<String P4>,<String P5>,<String P6>,<String P7>,<String P8>,<String P9>),Long,Proc
ProtoView           PROCEDURE(String Prototype, String P1,<String P2>,<String P3>,<String P4>,<String P5>,<String P6>,<String P7>,<String P8>,<String P9>),Long,Proc

HexDump             PROCEDURE(StringTheory InputString, StringTheory OutHexDump)
HexDump             PROCEDURE(STRING StringToReturnHex),STRING
HexDump             PROCEDURE(*STRING StringToReturnHex),STRING 
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
!   Bang.LinesViewInList(ST,'EmpPos2019')  !<-- View Lines Q split by 13,10 in a LIST
!   Bang.LinesViewSplitCSV(ST)             !<-- View split again as CSV
!   Bang.LinesViewSplit(ST,CHR(9),'','')   !<-- View using any split specs
!
! Bang.Reflection(ST,'EmpPos2019')  !<-- View ST Object Properties with buttons for Value, Sub, Slice and Lines. Sort of Everything.
!
!   Lne.SetValue(ST.GetLine(X))
!     Bang.ValueView(Lne)           !<-- View Len.GetValue() of line
!   Lne.Split(',','"','"', True) 
!     Bang.LinesViewInList(Lne)     !<-- View split lines queue in list
!
!----------------------------------------------------------------------------
    _EndOfInclude_