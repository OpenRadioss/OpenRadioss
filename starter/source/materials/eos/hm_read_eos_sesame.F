Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_read_eos_sesame       ../starter/source/materials/eos/hm_read_eos_sesame.F
!||--- called by ------------------------------------------------------
!||    hm_read_eos              ../starter/source/materials/eos/hm_read_eos.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_string            ../starter/source/devtools/hm_reader/hm_get_string.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    mrdse2                   ../starter/source/materials/mat/mat026/mrdse2.F
!||    tb2si1                   ../starter/source/materials/eos/sesame_tools.F
!||    tbusr1                   ../starter/source/materials/eos/sesame_tools.F
!||--- uses       -----------------------------------------------------
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_EOS_SESAME(IOUT,PM,UNITAB,IUNIT,LSUBMODEL,IMIDEOS,IMID,TITR,BUFMAT,MFI,IDF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD  
      USE INOUTFILE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C reading parameters for
C   NOBLE-ABEL EQUATION OF STATE
C-----------------------------------------------
C   C o m m e n t s
C-----------------------------------------------
C  RHOI = PM(89)   -> provided by /MAT
C  RHOR = PM(01)   -> provided by /MAT (can be erased by EOS if present : obsolete)
C  => MU0 = RHO/RHOR-1.
C  PM(31) = P(MU0,E0) -> will be used to initialize diagonal of stress tensor SIG(1:3,*)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IIN,IOUT,IUNIT
      INTEGER,INTENT(INOUT) :: MFI, IDF
      INTEGER,INTENT(IN) :: IMID      
      my_real PM(NPROPM),BUFMAT(*)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      INTEGER,INTENT(IN) :: IMIDEOS    
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NR, NT, IDR, IDT, IDP, IDE
      my_real E0, RHO0, RHO0I, P0, T0, XNR, XNT, DPDR, RHO
      my_real DYDZ,RHOI,RHOR
      my_real MU0,DF,SSP0,G0,DPDMU
      CHARACTER FILE*(ncharline),FILE_TMP*(ncharline)
      INTEGER :: FILE_LEN
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE, IS_AVAILABLE_RHO0
      CHARACTER(LEN=NCHARTITLE) :: TITR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IS_AVAILABLE_RHO0 = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_EA', E0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('SESAME_RHO', RHO0, IS_AVAILABLE_RHO0,LSUBMODEL,UNITAB)
      CALL HM_GET_STRING('ISRTY',FILE, ncharline,IS_AVAILABLE)

      RHOR = PM(1)
      RHOI = PM(89)

      IF(RHO0 > ZERO) THEN
        RHOR = RHO0
        PM(1)= RHO0 
      ELSE
        RHO0=RHOR                   
      ENDIF

      PM(23) = E0
C----------------
C     READ TABLE
C----------------
      FILE_LEN = LEN_TRIM(FILE)
      FILE_TMP(1:FILE_LEN) = FILE(1:FILE_LEN)
      FILE = INFILE_NAME(1:INFILE_NAME_LEN)//FILE_TMP(1:FILE_LEN)
      FILE_LEN = FILE_LEN + INFILE_NAME_LEN
      OPEN(UNIT=31,FILE=FILE(1:FILE_LEN),ERR=999,STATUS='OLD',FORM='FORMATTED')
      READ(31,*)
      READ(31,'(2E15.0)')XNR,XNT
      REWIND(31)
      NR = NINT(XNR)
      NT = NINT(XNT)
      PM(33)=NR
      PM(34)=NT
      PM(35)=IDF
      IF(PM(79) == ZERO)PM(79)=THREE100
      IDR = IDF
      IDT = IDR + NR
      IDP = IDT + NT
      IDE = IDP + NR * NT
      IDF = IDE + NR * NT
      MFI = MFI + IDF - IDR
C
      BUFMAT(IDR:IDF-1) = ZERO
C
      CALL MRDSE2(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDP),BUFMAT(IDE))
      CLOSE(31)
C
      CALL TB2SI1(BUFMAT(IDR),BUFMAT(IDP),BUFMAT(IDE),NR,NT)
      CALL TBUSR1(BUFMAT(IDR),BUFMAT(IDP),BUFMAT(IDE),NR,NT,UNITAB)
C
      RHO = PM(89)

       CALL MINTP_RE(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDE),RHO,T0,E0/RHO0,DYDZ)
       CALL MINTP_RT(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDP),RHO,T0,P0,DPDR)
C
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)E0,FILE,P0,T0
        IF(IS_AVAILABLE_RHO0)WRITE(IOUT,1501)PM(1)
      ENDIF

      PM(31) = P0
      
      !SSP0
      IF(RHOI == ZERO)THEN
        MU0 = ZERO ! error 683 already displayed
      ELSE
        IF(RHOR /= ZERO)THEN
          MU0 = RHOI/RHOR-ONE
        ELSE
          MU0 = ZERO ! error 683 already displayed
        ENDIF
      ENDIF
      
      IF(RHOI /= ZERO)THEN
        DF = RHOR/RHOI
      ELSE
        DF = ZERO
      ENDIF
            
      RHO0I=PM(89)
      SSP0 = ZERO 
      G0 = PM(22)
      RHOI = PM(89) 
      DPDMU=RHO0I*DPDR
      DPDMU=MAX(ZERO,DPDMU)        
      IF(RHOR > ZERO) SSP0 = SQRT((DPDMU + TWO_THIRD*G0)/RHOR) 
      PM(27)=SSP0      
      
      RETURN
  999 CONTINUE
      CALL ANCMSG(MSGID=19,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=IMID,C1='EOS',C2='EOS',C3=TITR,C4=FILE)
      RETURN
      
 1000 FORMAT(
     & 5X,'  SESAME TABLE EOS     ',/,
     & 5X,'  ----------------     ',/)
 1500 FORMAT(
     & 5X,'INITIAL INTERNAL ENERGY PER UNIT VOLUME. =',1PG20.13/,
     & 5X,'SESAME TABLE 301 . . . . . . . . . . . . =',A70/,
     & 5X,'INITIAL PRESSURE . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'INITIAL TEMPERATURE. . . . . . . . . . . =',1PG20.13)
 1501 FORMAT(     
     & 5X,'EOS REFERENCE DENSITY . . . . . . . . . .=',1PG20.13)        
C
      RETURN
      END



