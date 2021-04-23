!Tets BigBangString for STRING's that has features line BigBangTheory for StringTheory
  PROGRAM
  INCLUDE('BigBangString.INC'),ONCE
!  INCLUDE('BigBangSystemString.INC'),ONCE
!  INCLUDE('SystemString.INC'),ONCE
 
  MAP
Test_BangString PROCEDURE()    !No Theory here 
HaltBangTest    PROCEDURE()  
  END
  CODE
  START(HaltBangTest)
  Test_BangString()  

!-------------------
Test_BangString PROCEDURE()  
X    LONG 
    CODE 
!    Bang.DoNotShow=1      !When True None of the Bang windows show  
!    DO ParmsViewRtn ; halt() 
    DO SliceTestRtn
    DO StringViewTestRtn 
    DO HexTestRtn 
    DO ParmsViewRtn
    HALT()
    RETURN

!-------------------------------------
SliceTestRtn ROUTINE    
    DATA
S256 string(100)
BBStr BigBangString  !Carl Viewer    
    CODE 
    s256='1234567890abcdefghijklmnopqrstuvwxyz!@#$%^&*()_-+=ABCDEFGHIJKLMNOP' & all('~')
    BBStr.SliceView(s256,11,11+25,'Slice View ? a-z') 
    BBStr.SubView(s256,11,26,'Sub(11,26) View ? a-z') 
    BBStr.SliceView(s256,90,100,'Slice View 90-100') 
    BBStr.SubView(s256,90,10,'Sub(90,10) ') 
    BBStr.SliceView(s256,90,101,'Slice View 90-101') 
    BBStr.SubView(s256,90,12,'Sub(90,12) ')
  

!-------------------------------------
StringViewTestRtn ROUTINE
    DATA
BBStr BigBangString  !Carl Viewer    
Txt     STRING(400) 
    CODE 
 bbStr.StringView('While impervious to everything, they had no resistance to the microbes ' & | 
    'in our atmosphere to which we have long since become immune. After all that men ' & | 
    'could do had failed, the Trumpians were destroyed and humanity was saved by the ' & | 
    'littlest things, which God, in His wisdom, had put upon this Earth.')

 Txt = 'While impervious to everything, they had no resistance ' &|
     '<13,10>to the microbes in our atmosphere to which we ' &|
     '<13,10>have long since become immune. After all that ' &|
     '<13,10>men could do had failed, the Trumpians were destroyed ' &|
     '<13,10>and humanity was saved by the littlest things, ' &|
     '<13,10>which God, in His wisdom, had put upon this Earth.'
 bbStr.StringView(Txt)     

!-------------------------------------
HexTestRtn ROUTINE   !
    DATA 
s256    STRING(256) 
BBStr BigBangString  
    CODE
    LOOP X=1 to 255 ; s256[x]=CHR(x) ; END 
   ! BBStr.StringView('BBStr ' & s256 & s256 & s256 & s256 & '1024 Byte ','Check "HEX" Test 1024 bytes') 
    BBStr.StringView(s256 & s256 & s256 & s256 & '1024 Byte Tester','Check "HEX" Test 1024 bytes') 

    BBStr.StringView(s256,'Call *STRING (256) sig')


!------------------------------------- 

ParmsViewRtn ROUTINE 
    DATA
BBStr BigBangString
    CODE     
  !ST can have many parameters so hard to figure out if you have the right commas,,,,1 e.g. NoCase
  
  BBStr.ParmsView('humanity','America',,,,1)        !Generalized Parms counters
  BBStr.ProtoView('string pOldValue, string pNewValue, long pCount=0, long pStart=1, long pEnd=0, long pNoCase=0, bool pRecursive=false' , |
                 'humanity','America',,,,1) 
 
!========================================================================== 

!===========================================================================
HaltBangTest      PROCEDURE()
W   WINDOW('Bang String Test'),AT(1,1,86,40),GRAY,SYSTEM,FONT('Segoe UI',9)
        BUTTON('Halt Bang String'),AT(10,4,65,33),USE(?HaltBtn),ICON(ICON:Cross)
    END
    CODE
    OPEN(W)
    ACCEPT
        IF ACCEPTED() THEN BREAK.
    END
    HALT()