    OMIT('_EndOfInclude_',_IFDef_BigBangString_)
_IFDef_BigBangString_  EQUATE(1) 
!----------------------------------------------------------------------------
! BigBangString - View Strings in a Window, see it as Hex. View Slice and Sub()
! (c)2021 by Carl T. Barnes - Released under the MIT License 
! Download: https://github.com/CarlTBarnes/StringTheory-Tools
!----------------------------------------------------------------------------
!
!----------------- BigBangString -----------------
BigBangString    CLASS,TYPE,MODULE('BigBangString.CLW'),LINK('BigBangString.CLW')
DoNotShow           BOOL     !Class is for Developers so =True doesn't show any Windows

StringView          PROCEDURE(*STRING StrValue, <STRING WindowCaption>) 
StringView          PROCEDURE(STRING  StrValue, <STRING WindowCaption>) 

SliceView           PROCEDURE(*String StrToViewSlice, Long SliceStart, Long SliceEnd, <STRING CapTxt>) !View Slice() of String
SubView             PROCEDURE(*String StrToViewSUB, Long SubStart,  Long SubLength, <STRING CapTxt>) !View Slice() of String


!StringView_ST          PROCEDURE(*STRING StrValue, <STRING WindowCaption>) 
!StringView_ST          PROCEDURE(STRING  StrValue, <STRING WindowCaption>) 

ParmsView           PROCEDURE(String P1,<String P2>,<String P3>,<String P4>,<String P5>,<String P6>,<String P7>,<String P8>,<String P9>),Long,Proc
ProtoView           PROCEDURE(String Prototype, String P1,<String P2>,<String P3>,<String P4>,<String P5>,<String P6>,<String P7>,<String P8>,<String P9>),Long,Proc

HexDump             PROCEDURE(*STRING StringToReturnHex),*STRING
     END
!-------------------------- How to Use: ------------------------------------
!
!INCLUDE('BigBangString.INC'),ONCE      !Global include class
!
!Bang BigBangString                     !Local declare BigBang object 
!
! Bang.DoNotShow=CHOOSE(~DebugST)       !<-- Only show BigBang if DebugST=True
!
! Bang.StringView(StrVariable)        !<-- View StrVar in Window, see Hex
!
!----------------------------------------------------------------------------
    _EndOfInclude_