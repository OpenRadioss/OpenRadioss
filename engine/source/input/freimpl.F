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
Chd|  FREIMPL                       source/input/freimpl.F        
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ORDER_DTF                     source/input/freimpl.F        
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        NVAR                          source/input/nvar.F           
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|        IMP_KBCS                      share/modules/impbufdef_mod.F 
Chd|        IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|        IMP_SPBRM                     share/modules/impbufdef_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREIMPL(IKAD,KEY0,KIMPL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
      USE IMP_KBCS
      USE IMP_PCG_PROJ
      USE IMP_SPBRM
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKAD(0:*),KIMPL
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "buckcom.inc"
#include      "scr06_c.inc"
#include      "chara_c.inc"
#include      "fsi_ar_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NBC, K, IKEY,IM,J,NJ,KK, IOERR,E_ID
      CHARACTER  CARTE*ncharline,TITLE*72, KEY2*5, KEY3*5, KEY4*5
      my_real
     .   TMP
C----------------------------------------
      IKEY=KIMPL
      IMPL_S=0
      IDYNA=0
      ILINE=0
      ISPRB=0
      ISOLV=0
      INSOLV=0
      IDTC=0
      IM=0
      IKG=1
      KZ_TOL=ZERO
      SK_INT=ZERO
      D_TOL=ZERO
      LPRINT=0
      NPRINT=0
      IMPDEB=0
      SOLVNFO=0
      PRSTIFMAT=0
      PRSTIFMAT_TOL=ZERO
      PRSTIFMAT_NC=1
      PRSTIFMAT_IT=0
      IMPMV=1
      ISIGINI=0
      ILINTF=0
      IPREC = 0
      L_LIM = 0
      ITOL  = 0
      L_TOL =ZERO
      DT_IMP = ZERO
      DT_MIN = ZERO
      DT_MAX = ZERO
      IMP_RBY=0
      IMP_INT=0
      ISPRN = 1
C      INTP_C = 0
C -----after debuging on int24 spmd, change defaut to INTP_C=1 (INTP_C=0 suppressed good for maintenance)      
      INTP_C = 1
      L_BFGS = 0
C      IRREF = 0
      IRREF = 1
      IQSTAT = 0
      IBUCKL = 0
      ISCAU = 0
      IMP_LR=0
      IKPROJ=0
      ISMDISP = 0
      IACUSLV = 0
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
       IMPL_S=1
       NCINP=1
       N_PAT = 1
       IMP_CHK = 0
       IMP_INT7 = 0
       ITTOFF = 0
       SCAL_DTQ = ONE
       IDY_DAMP=0
       IAUTSPC = 1
       ITRMAX = 0
       MSG_LVL = 0
       B_ORDER =0
       B_MCORE =0
       IREFI = 0
       ILINE_S = 0
       NLS_LIM = 0
       LS_TOL = ZERO
       NDIVER = 0
       IKT = 0
       NDTFIX = 0
       IKPRES = 1
       N_TOLU=ZERO
       N_TOLF=ZERO
       N_TOLE=ZERO
       NCY_MAX = 0
       RF_MIN = ZERO
       RF_MAX = ZERO
       IPUPD = 0
       TOL_DIV = ZERO
       M_VS = 0
       IPRO_S0=0
       IIKGOFF = 1
       M_MSG = 0
       M_ORDER =0
       M_OCORE =0
       IRIG_M = 0
 1160  READ(IUSC1,REC=IKAD(IKEY)+K,
     .      FMT='(7X,A,1X,A,1X,A,25X,I10)',
     .      ERR=9990)KEY2,KEY3,KEY4,NBC
       K=K+1
C----------------------------
C      Dynamic implicit
C----------------------------
       IF(KEY2(1:4)=='DYNA')THEN
        IF (IDYNA==0) IDYNA=1
        IF(KEY3(1:4)=='DAMP')THEN
         IDY_DAMP=1
         CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
         READ(IUSC2,*) DAMPA_IMP,DAMPB_IMP
        ELSE IF(KEY3(1:3)=='FSI')THEN
         WRITE(6,*) "ERROR: /IMPL/DYNA/FSI IS A DEPRECATED FEATURE"
          GOTO 9990
        ELSE
         READ(KEY3,'(I2)')IM
         IDYNA=MAX(IDYNA,IM)
         IF(IDYNA==1)THEN
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*)HHT_A
         ELSEIF(IDYNA==2)THEN
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*)NEWM_A,NEWM_B
         ELSE
          HHT_A =-EM20
         ENDIF
        ENDIF
C----------------------------
C      Implicit linear
C----------------------------
       ELSEIF(KEY2(1:4)=='LINE')THEN
        ILINE=1
        IF(KEY3(1:5)=='INTER') THEN
         READ(KEY4,'(I5)')ILINTF
         ILINTF = MAX(2,ILINTF)
        ELSEIF(KEY3(1:5)=='SCAUC') THEN
         ISCAU = 1
        ENDIF
       ELSEIF(KEY2(1:5)=='MONVO')THEN
        IF(KEY3(1:3)=='OFF')IMPMV=0
       ELSEIF(KEY2(1:5)=='SPRIN')THEN
         IF(KEY3(1:4)=='NONL')THEN
          ISPRN = 1
         ELSEIF(KEY3(1:4)=='LINE')THEN
          ISPRN = 0
         ELSE
          GOTO 9990
         ENDIF
       ELSEIF(KEY2(1:5)=='PREPA')THEN
        READ(KEY3,'(I2)')N_PAT
       ELSEIF(KEY2(1:5)=='PROJV')THEN
        READ(KEY3,'(I2)') M_VS
       ELSEIF(KEY2(1:5)=='PROSI')THEN
        READ(KEY3,'(I2)') IPRO_S0
C----------------------------
C      Implicit check
C----------------------------
       ELSEIF(KEY2(1:5)=='CHECK')THEN
        IMP_CHK = 1
C----------------------------
C      Implicit quasi-static
C----------------------------
       ELSEIF(KEY2(1:5)=='QSTAT')THEN
        IQSTAT = 1
        IF(KEY3(1:5)=='DTSCA')THEN
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*)SCAL_DTQ
        ELSEIF(KEY3(1:5)=='MRIGM')THEN
          IRIG_M = 1
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*,ERR=520,END=520)E_REF(1),E_REF(2),E_REF(3)
          IF (E_REF(1)>0.AND.E_REF(2)>0.AND.E_REF(3)>0) IRIG_M = 2
 520    CONTINUE
        ELSE
          READ(KEY3,'(I2)')IM
          IQSTAT=MAX(IQSTAT,IM)
        ENDIF
C----------------------------
C      spring-back
C----------------------------
       ELSEIF(KEY2(1:4)=='SPRB')THEN
        ISPRB=1
C----------------------------
C      print-out
C----------------------------
       ELSEIF(KEY2=='PRINT')THEN
         IF(KEY3(1:4)=='LINE')THEN
          READ(KEY4,'(I5)')LPRINT
         ELSEIF(KEY3(1:4)=='NONL')THEN
          READ(KEY4,'(I5)')NPRINT
         ELSEIF(KEY3(1:4)=='STIF')THEN
          PRSTIFMAT = 1
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*)PRSTIFMAT_TOL,PRSTIFMAT_NC,PRSTIFMAT_IT
         ELSE
          GOTO 9990
         ENDIF
C----------------------------
C      Linear SOLVER
C----------------------------
       ELSEIF(KEY2(1:4)=='SOLV')THEN
        READ(KEY3,'(I2)')ISOLV
        CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
        READ(IUSC2,*)IPREC,L_LIM,ITOL,L_TOL
        IF (IMACH==3.AND.ISOLV==3) IMUMPSD=L_LIM
C----------------------------
C      BCS parameters
C----------------------------
       ELSEIF(KEY2(1:4)=='SBCS')THEN
        IF(KEY3(1:5)=='MSGLV')THEN
         READ(KEY4,'(I2)')MSG_LVL
        ELSEIF(KEY3(1:5)=='ORDER')THEN
         READ(KEY4,'(I2)')B_ORDER
C-------0 default 1 :MMD  2 :metis       
        ELSEIF(KEY3(1:5)=='OUTCO')THEN
             B_MCORE=1
        ELSE
          GOTO 9990
        ENDIF
C----------------------------
C      MUMPS parameters
C----------------------------
       ELSEIF(KEY2(1:5)=='MUMPS')THEN
        IF(KEY3(1:5)=='MSGLV')THEN
         READ(KEY4,'(I2)')M_MSG
        ELSEIF(KEY3(1:5)=='ORDER')THEN
         IF(KEY4(1:5)=='METIS')THEN
          M_ORDER = 5
         ELSEIF(KEY4(1:4)=='PORD')THEN
          M_ORDER = 4
         END IF
C-------0 default 1 :MMD  2 :metis       
        ELSEIF(KEY3(1:5)=='OUTCO')THEN
             M_OCORE=1
        ELSE
          GOTO 9990
        ENDIF
C----------------------------
C      Nonlinear SOLVER
C----------------------------
       ELSEIF(KEY2(1:4)=='NONL')THEN
        IF(KEY3(1:5)=='KTANG')THEN
         IKT = 1
        ELSEIF(KEY3(1:5)=='KTFUL')THEN
         IKT = 2
        ELSEIF(KEY3(1:5)=='KTFU8')THEN
         IKT = 3
        ELSEIF(KEY3(1:5)=='KTCON')THEN
         IKT = 4
        ELSEIF(KEY3(1:5)=='PITER')THEN
         READ(KEY4,'(I5)') IPUPD
        ELSEIF(KEY3(1:5)=='SMDIS')THEN
         ISMDISP = 1
        ELSEIF(KEY3(1:5)=='SOLVI')THEN
         SOLVNFO = 1
        ELSE
         READ(KEY3,'(I2)')INSOLV
         CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
         READ(IUSC2,'(a)')TITLE
         READ(TITLE,*)N_LIM,NITOL,N_TOL
         IF (NITOL>10) THEN
          SELECT CASE (NITOL)
           CASE(12)
            READ(TITLE,*)N_LIM,NITOL,N_TOLE,N_TOLF
           CASE(13)
            READ(TITLE,*)N_LIM,NITOL,N_TOLE,N_TOLU
           CASE(23)
            READ(TITLE,*)N_LIM,NITOL,N_TOLF,N_TOLU
           CASE(123)
            READ(TITLE,*)N_LIM,NITOL,N_TOLE,N_TOLF,N_TOLU
          END SELECT
         ENDIF !(NITOL>10)
         IF(NITOL==1.AND.IRREF==1) IRREF = 0
        ENDIF
       ELSEIF(KEY2(1:5)=='SINIT')THEN
        ISIGINI=1
       ELSEIF(KEY2(1:5)=='LBFGS')THEN
          READ(KEY3,'(I5)') L_BFGS
C----------------------------
C      Step Control
C----------------------------
       ELSEIF(KEY2=='DTINI')THEN
         CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
         READ(IUSC2,*)DT_IMP
       ELSEIF(KEY2(1:2)=='DT')THEN
         IF(KEY3(1:4)=='STOP')THEN
           CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
           READ(IUSC2,*)DT_MIN,DT_MAX
C----------------------------
C-------------fix point for time step----
C----------------------------
         ELSEIF(KEY3(1:4)=='FIXP')THEN
          KK =K
          DO I=1,NBC
           READ(IUSC1,REC=IKAD(IKEY)+KK,FMT='(A)',ERR=9990)CARTE
           CALL WRIUSC2(IKAD(IKEY)+KK,1,KEY0(IKEY))
           NJ = NVAR(CARTE)
           IF ((NDTFIX+NJ)>100) THEN
            NJ = 100-NDTFIX
            WRITE(ISTDO,*)
     .       ' ** WARNING ** : MAXMUM 100 FIX POINTS PERMITTED '
           ENDIF
           READ(IUSC2,*,ERR=9990,END=9990)(DTIMPF(NDTFIX+J),J=1,NJ)
           KK=KK+1
           NDTFIX = NDTFIX + NJ
          ENDDO
          CALL ORDER_DTF(NDTFIX,DTIMPF)
         ELSE
           READ(KEY3,'(I2)')IM
           IF (IDTC>0.AND.IM>0) GOTO 9990
           IDTC=IM
           IF(IM==1)THEN
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*)NL_DTP,SCAL_DTP,NL_DTN,SCAL_DTN
           ELSEIF(IM==2)THEN
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*)NL_DTP,ALEN0,NL_DTN,SCAL_DTN,SCAL_DTP
           ELSEIF(IM==3)THEN
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*)NL_DTP,ALEN0,NL_DTN,SCAL_DTN,SCAL_DTP,IAL_M,
     .                   SCAL_RIKS
           ELSE
            GOTO 9990
           ENDIF
         ENDIF
C----------------------------
C      NCYCLE stop
C----------------------------
       ELSEIF(KEY2=='NCYCL')THEN
         IF(KEY3(1:4)=='STOP')THEN
           CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
           READ(IUSC2,*)NCY_MAX
         ELSE
           GOTO 9990
         ENDIF
C----------------------------
C      interface Control
C----------------------------
       ELSEIF(KEY2(1:5)=='INTER')THEN
         IF(KEY3(1:5)=='TTOFF')THEN
          ITTOFF = 1
         ELSEIF(KEY3(1:5)=='SINT7')THEN
          READ(KEY4,'(I2)')IMP_INT7
C-----0 nonlinear, 1: linear 2: constant---
          IMP_INT7= MIN(2,IMP_INT7)
C---------will be suppressed in the 14.0.210          
         ELSEIF(KEY3(1:5)=='KNONL')THEN
C-----0 nonlinear, 1: linear ----
          READ(KEY4,'(I2)')IM
          INTP_C = -IM -1
C         ELSEIF(KEY3(1:5)=='KCOMP'.AND.INTP_C==0)THEN
         ELSEIF(KEY3(1:5)=='KCOMP')THEN
c          INTP_C = 1
C-----hide option to activate KG in int24 (/IMPLICIT should be defined in Starter)        
         ELSEIF(KEY3(1:4)=='KGON')THEN
          IIKGOFF = 0
         ELSE
          GOTO 9990
         ENDIF
C----------------------------
C      R_ref options
C----------------------------
       ELSEIF(KEY2(1:4)=='RREF')THEN
         IRREF = 2
         IF(KEY3(1:3)=='OFF') THEN
          IRREF = 0
         ELSEIF(KEY3(1:5)=='INTER')THEN
C-----0 agressive, 1: moyen ----2--faible  3  non--4 non sauf 1er---
          READ(KEY4,'(I2)')IM
          IREFI = IM
         ELSEIF(KEY3(1:5)=='LIMIT')THEN
           CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
           READ(IUSC2,*)RF_MIN,RF_MAX
         ENDIF
C----------------------------
C      divergence criteria
C----------------------------
       ELSEIF(KEY2(1:5)=='DIVER')THEN
         IF(KEY3(1:3)=='TOL')THEN
           CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
           READ(IUSC2,*)TOL_DIV
C-----num. of diver---
         ELSE
          READ(KEY3,'(I2)')IM
          NDIVER = IM
          IF (NDIVER ==0) NDIVER=-1
         END IF
C----------------------------
C      Geometrical stifness
C----------------------------
       ELSEIF(KEY2(1:5)=='GSTIF')THEN
        IF(KEY3(1:3)=='OFF')IKG=0
C----------------------------
C      Geometrical stifness
C----------------------------
       ELSEIF(KEY2(1:5)=='PSTIF')THEN
        IF(KEY3(1:3)=='OFF') IKPRES=0
C----------------------------
C      buckling analysis
C----------------------------
       ELSEIF(KEY2=='BUCKL')THEN
        READ(KEY3,'(I2)')IBUCKL
        IF (IBUCKL==0) THEN
         WRITE(ISTDO,*) ' ** ERROR ** : KEYWORD /IMPL/BUCKL OBSOLETE ',
     .                  'USING /IMPL/BUCKL/1 OR /IMPL/BUCKL/2'
         GOTO 9990
        ENDIF
        IBUCKL = IBUCKL-1
        CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
        READ(IUSC2,*) EMIN_B, EMAX_B, NBUCK, MSGL_B, MAXSET_B, SHIFT_B
        IF (SHIFT_B==ZERO) SHIFT_B=EM02
        SHFTBUCK = SHIFT_B
        IF (MAXSET_B==0) MAXSET_B=8
        BNITER=300
        BINCV=4
        BMAXNCV=16
c        BMAXNCV=MAX(BINCV,BMAXNCV)
        BIPRI =MSGL_B
        BISOLV=1
C
       ELSEIF(KEY2(1:5)=='AUTOS')THEN
         IF(KEY3(1:3)=='OFF')THEN
           IAUTSPC=0
         ELSEIF(KEY3(1:3)=='ALL')THEN
           IAUTSPC=2
         ENDIF
C----------------------------
C      line_search option
C--------0=3, 1:energy 2: force --3:AUTO (old)------------------
       ELSEIF(KEY2(1:5)=='LSEAR')THEN
        IF(KEY3(1:3)=='OFF')THEN
         ILINE_S = 100
        ELSE
         READ(KEY3,'(I2)')ILINE_S
         CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
         READ(IUSC2,*)NLS_LIM,LS_TOL
        ENDIF
C----------------------------
C      projection for warped shell elements
C--------0=no proj but keep explicit part 1:doing -1 :no proj, neither for explicit---
       ELSEIF(KEY2(1:5)=='SHPOF')THEN
           IKPROJ=-1
C-----------become default-after-----
       ELSEIF(KEY2(1:5)=='SHPON')THEN
           IKPROJ=1
C----------------------------
C      OLD CONTROL OPTIONS
C----------------------------
       ELSEIF(KEY2(1:5)=='CONTR')THEN
         IF(KEY3(1:2)=='DT')THEN
          IF(KEY4(1:4)=='STOP')THEN
           CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
           READ(IUSC2,*)DT_MIN,DT_MAX
          ELSE
           READ(KEY4,'(I2)')IM
           IDTC=IM
           IF(IM==1)THEN
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*)NL_DTP,SCAL_DTP,NL_DTN,SCAL_DTN
           ELSEIF(IM==2)THEN
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*)NL_DTP,ALEN0,NL_DTN,SCAL_DTN,SCAL_DTP
           ENDIF
          ENDIF
         ELSEIF(KEY3(1:4)=='SHEL')THEN
C----------------------------
C         Fictif stifness of Mzz for shell
C----------------------------
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*)KZ_TOL
         ELSEIF(KEY3(1:5)=='INTER')THEN
C----------------------------
C         stifness factor for interface
C----------------------------
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*)SK_INT
         ENDIF
C----------------------------
C      hide options
C----------------------------
       ELSEIF(KEY2(1:5)=='PRTOL')THEN
        CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
        READ(IUSC2,*)D_TOL
       ELSEIF(KEY2(1:4)=='NEXP')THEN
        READ(KEY3,'(I5)')NEXP
       ELSEIF(KEY2=='DEBUG')THEN
        IMPDEB=1
        CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
        READ(IUSC2,*)NDEB0,NDEB1
         IF(NDEB0/=0)NDEB0 = NDEB0 + 1
         NDEB1=MAX(NDEB0,NDEB1+1)
       ELSEIF(KEY2(1:3)=='DEL')THEN
         IF(KEY3(1:5)=='RBODY')THEN
           IMP_RBY=1
         ELSEIF(KEY3(1:5)=='INTER')THEN
           IMP_INT=1
         ENDIF
       ELSEIF(KEY2(1:5)=='ITRBY')THEN
C-------max iter for secnd dis calculation with finite rotation---       
        READ(KEY3,'(I3)')ITRMAX
       ELSEIF(KEY2(1:4)=='LRIG')THEN
        IMP_LR = 1
       ELSE
         GOTO 9990
       ENDIF
       K=K+NBC
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 1160
       IF (IPARIT/=0) THEN
        IPARIT=0
        IKG=IKG+5
       ENDIF
      ENDIF
C
      RETURN
C
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
Chd|====================================================================
Chd|  ORDER_DTF                     source/input/freimpl.F        
Chd|-- called by -----------
Chd|        FREIMPL                       source/input/freimpl.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ORDER_DTF(N,RC)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
      my_real
     .        RC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,NN
      my_real
     .        S(N),SMIN
C
      IF (N==0) RETURN
C-----en ordre croisante-----
      NN =0
      DO I =1,N
       IF (RC(I)>ZERO) THEN
        NN = NN +1
        S(NN)= RC(I)
       ENDIF
      ENDDO
      N= NN
      DO I =1,N
       SMIN=S(I)
       II=I
       DO J =I+1,N
        IF (S(J)<SMIN) THEN
         II=J
         SMIN = S(J)
        ENDIF
       ENDDO
       IF (II/=I) THEN
        SMIN =S(I)
        S(I)=S(II)
        S(II)=SMIN
       ENDIF
       RC(I) = S(I)
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END

