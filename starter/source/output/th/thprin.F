Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
Chd|====================================================================
Chd|  THPRIN                        source/output/th/thprin.F     
Chd|-- called by -----------
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|====================================================================
      SUBROUTINE THPRIN(
     1      KEY    ,IPASU  ,NPASU ,NID   ,
     2      NVPS  ,ITHBUF ,VARE   ,NUM   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPASU,NUM,NVG,NID,NVPS,
     .        IPASU(NPASU,*),ITHBUF(*)
      CHARACTER*10 VARE(*),KEY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,JJ, I,ISU,ID,NNE,NOSYS,J10(10),NTOT,KK,IER,
     .        OK,IGS,IGRS,NSU,K,L,JREC,CONT,IAD0,IADV,NTRI,NL,
     .        IFITMP,IADFIN,NVAR,M,N,IAD2,IAD1,ISK,IPROC
      CHARACTER TITR*nchartitle,MESS*40
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,ULIST2S,LISTCNT,NINTRN,THVARC
      DATA MESS/'TH GROUP DEFINITION                     '/
C-----------------------------------------------
C
C
C-----------------------------------------------
C=======================================================================
C PRINTOUT
C=======================================================================
      IF(IPRI<1)RETURN
C
        WRITE(IOUT,'(//)')
        WRITE(IOUT,'(2A)')' TH GROUP: ',KEY
        WRITE(IOUT,'(A)') ' ---------'

        DO N=1,NUM
          NVAR=IPASU(NVPS,N)
          IF(NVAR>0)THEN
            IAD1=IPASU(NVPS+1,N)
            ID=IPASU(NID,N)
            CALL FRETITL2(TITR,IPASU(NPASU-LTITR+1,N),LTITR)
            WRITE(IOUT,'(/,3A,I10,3A,I3,A)')' TH ',KEY,':',ID,',',TRIM(TITR),',',NVAR,' VAR:'
            WRITE(IOUT,'(10A10)')(VARE(ITHBUF(J)),J=IAD1,IAD1+NVAR-1)
          ENDIF
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  THPRIN_SUB                    source/output/th/thprin.F     
Chd|-- called by -----------
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE THPRIN_SUB(
     1      KEY    ,SUBSET ,ITHFLAG ,NID   ,
     2      NVPS   ,ITHBUF ,VARE    ,NUM   )
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUM,NVG,NID,NVPS,
     .        ITHBUF(*),ITHFLAG
      CHARACTER*10 VARE(*),KEY
!
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,JJ, I,ISU,ID,NNE,NOSYS,J10(10),NTOT,KK,IER,
     .        OK,IGS,IGRS,NSU,K,L,JREC,CONT,IAD0,IADV,NTRI,NL,
     .        IFITMP,IADFIN,NVAR,M,N,IAD2,IAD1,ISK,IPROC
      CHARACTER TITR*nchartitle,MESS*40
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,ULIST2S,LISTCNT,NINTRN,THVARC
      DATA MESS/'TH GROUP DEFINITION                     '/
C-----------------------------------------------
C
C
C-----------------------------------------------
C=======================================================================
C PRINTOUT
C=======================================================================
      IF(IPRI<1)RETURN
C
        WRITE(IOUT,'(//)')
        WRITE(IOUT,'(2A)')' TH GROUP: ',KEY
        WRITE(IOUT,'(A)') ' ---------'

        DO N=1,NUM
          NVAR = SUBSET(N)%NVARTH(ITHFLAG)
          IF(NVAR>0)THEN
            IAD1=SUBSET(N)%THIAD
            ID=SUBSET(N)%ID
            TITR=SUBSET(N)%TITLE
            WRITE(IOUT,'(/,3A,I10,3A,I3,A)')' TH ',KEY,':',ID,',',TRIM(TITR),',',NVAR,' VAR:'
            WRITE(IOUT,'(10A10)')(VARE(ITHBUF(J)),J=IAD1,IAD1+NVAR-1)
          ENDIF
        ENDDO
C
      RETURN
      END


