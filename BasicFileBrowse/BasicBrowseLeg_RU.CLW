                     MEMBER('BasicBrowseLeg.clw')

HoldPosition         STRING(512),THREAD


!--------------------------------------------------
RISnap:EmpPosCSV    PROCEDURE
  CODE

!--------------------------------------------------
RIUpdate:EmpPosCSV   FUNCTION(BYTE FromForm)
  CODE
  HoldPosition = POSITION(EmpPosCSV)
  PUT(EmpPosCSV)
  IF ERRORCODE()
    RISaveError
    IF SaveErrorCode = RecordChangedErr THEN
      IF FromForm THEN
        StandardWarning(Warn:RIFormUpdateError)
      ELSE
        StandardWarning(Warn:RIUpdateError,'Record Changed by Another Station')
      END
      WATCH(EmpPosCSV)
      REGET(EmpPosCSV,HoldPosition)
      DO RICloseFiles
      RETURN(2)
    ELSE
      StandardWarning(Warn:RIUpdateError,'EmpPosCSV')
      DO RICloseFiles
      RETURN(1)
    END
  END
  DO RICloseFiles
  RETURN(0)
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  EXIT
