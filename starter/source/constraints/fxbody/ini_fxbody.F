Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  INI_FXBODY                    source/constraints/fxbody/ini_fxbody.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        ORTHRG                        source/constraints/fxbody/ortho_normalization.F
Chd|        ORTHSR                        source/constraints/fxbody/ortho_normalization.F
Chd|        ORTHST                        source/constraints/fxbody/ortho_normalization.F
Chd|        PRSCAL                        source/constraints/fxbody/ortho_normalization.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INI_FXBODY(FXBIPM,  FXBRPM, FXBNOD, FXBGLM,FXBCPM,   
     .                      FXBCPS,  FXBLM,  FXBFLS, FXBDLS,FXBMOD, 
     .                      ITAB, X ,MS, IN, FXB_MATRIX,
     .                      FXB_MATRIX_ADD,FXB_LAST_ADRESS,ICODE,NOM_OPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "fxbcom.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),FXBIPM(NBIPM,*),FXBNOD(*),FXB_MATRIX_ADD(4,*),FXB_LAST_ADRESS(*),ICODE(*),
     .        NOM_OPT(LNOPT1,*)
      my_real 
     .        X(3,*),MS(*),IN(*),
     .        FXBRPM(*), FXBGLM(*), FXBCPM(*), FXBCPS(*), 
     .        FXBLM(*),  FXBFLS(*), FXBDLS(*), FXBMOD(*),
     .        FXB_MATRIX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFX,ID,IDMAST,NMOD,NMST,NBNO,NME,NTR,ADRGLM,
     .        ADRCP,ADRLM,ADRFLS,ADRDLS,ADRVAR,ADRRPM,ADRMCD,ADRMOD,IMOD,INO,I,LEN,
     .        NLIG,NRES,ILIG,ADRCP2,IR,ADRNOD,NUMNO(10),ISHELL,
     .        IC,J,INFO,IANIM, IMIN, IMAX, REF, ERR, K, L, IDN, I1,I2, J2, REF2,J1,SIZE_MAX,IDUM1,IDUM2,IDUM3,
     .        IDUM4,FLAG,IC1,IC2,BCS(6),SIZE_STIFF,ADR_STIFF,IL1,IL2,IDOF1,IDOF2,NMOD_MAX,NMST_F,NMR,
     .        ADR_MASS,SIZE_MASS,RELOOP,ADRMOD0,ADRLM0,ADRFLS0,LENCP0,LENLM0,LENFLS0,LENDLS0,LENVAR0,
     .        NSAV_MODES,NSAV_MODESN,NN
      my_real 
     .        FREQ,BETA,OMEGA,DTC1,DTC2,BID,XM(3),ZZ,RHO_INVMK,UNSN,NORM,FAC,FAC2,MSTOT,TOLE,
     .        KT,KR,RR,SKEW(9),RDUM1,MIN_DIAG_STIF,FAC2X,FAC2Y,FAC2Z,DT_MINI,DT_CST,OMEGA_MIN,
     .        INMIN,INMAX,OMEGA_MAX,MAX_FREQ,ALPHA,FREQ_RANGE,MIN_CUT_OFF_FREQ
      CHARACTER TITR*nchartitle,MESS*40,MESS1*40,NWLINE*100,
     .          FXBFILE*100
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TABSL,ITAG_DOF
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX,LISTN
      INTEGER IFLAGI1,IFLAGDBL,IRB,CPT,REST,NLIGNE,M,INFO2,LWORK,NBDYN,NDDL,IM,NMOD0,NMST0
C
      INTEGER RPM_L,ADRMODE_L,ADRLM_L,ADRFLS_L,ADRGLM_L,ADRCP_L,ADRCP2_L,NSNGLR
C
      INTEGER NBNOD,IROT,IDAMP,IBLO,IFILE,REDUX,PRINTOUT
      my_real, DIMENSION(:),ALLOCATABLE :: VT,VBID
      my_real, DIMENSION(:,:),ALLOCATABLE :: MASS,STIF,MODES,MODEST,MODES_RB,MODES_RBT,STIFT,
     .                                       MTEMP,MTEMP2,MTEMP3,TT,T,VECTR,MODES_SAV
C
      DOUBLE PRECISION  WORK1(1000)
      DOUBLE PRECISION, DIMENSION(:),  ALLOCATABLE :: WORK,WR,WI
      DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE :: EIG_R,EIG_L,INVMK

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME

C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      my_real 
     .        PRSCAL
      DATA MESS/'FLEXIBLE BODY : NODES                   '/
      DATA MESS1/'FLEXIBLE BODY DEFINITION                '/
C=====================================================================  
C
      PRINTOUT = 0
      ADRMOD = FXB_LAST_ADRESS(1)
      ADRGLM = FXB_LAST_ADRESS(2)
      ADRCP  = FXB_LAST_ADRESS(3)
      ADRCP2 = FXB_LAST_ADRESS(3)
      ADRLM  = FXB_LAST_ADRESS(4)
      ADRFLS = FXB_LAST_ADRESS(5)
      ADRDLS = FXB_LAST_ADRESS(6)
      ADRVAR = FXB_LAST_ADRESS(7)
      ADRRPM = FXB_LAST_ADRESS(8)
      ADRMCD = FXB_LAST_ADRESS(9)
C
      FREQ_RANGE = HUNDRED    
C     Default min cut off frequency 1000Hz
      MIN_CUT_OFF_FREQ = 1000.0 / FAC_TIME 
C
      DO NFX=1,NFXBODY
C
        ADRNOD = FXBIPM(6,NFX)  
C
        IF (FXBIPM(41,NFX)==2) THEN
C
          IF (PRINTOUT == 0) WRITE(IOUT,1000)
          PRINTOUT = 1
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,NFX),LTITR)
C
          ID = FXBIPM(1,NFX)
          NBNOD =  FXBIPM(3,NFX)
          NMOD = FXBIPM(4,NFX)
          NMST = FXBIPM(5,NFX)
          ADRNOD = FXBIPM(6,NFX)
          ISHELL = FXBIPM(16,NFX)
          NME = FXBIPM(17,NFX)
          IBLO = FXBIPM(28,NFX)
          IFILE = FXBIPM(29,NFX)
          IANIM = FXBIPM(36,NFX)
          SIZE_STIFF = FXBIPM(42,NFX)
          SIZE_MASS = FXBIPM(43,NFX)
          ADR_STIFF = FXBIPM(44,NFX)
          ADR_MASS = FXBIPM(45,NFX)    
          NDDL = 6*NBNOD  
          SIZE_MAX = MAX(NMOD,NME)
C
          ALLOCATE(ITAG_DOF(6,NBNOD))
          ALLOCATE(STIF(NDDL,NDDL),MASS(NDDL,NDDL))
          ALLOCATE(MODES_RB(NDDL,NME),MODES_RBT(NME,NDDL))
          ALLOCATE(MODEST(NMOD,NDDL),MODES(NDDL,NMOD))
          ALLOCATE(VECTR(NDDL,6),MTEMP(NDDL,SIZE_MAX),MTEMP2(SIZE_MAX,SIZE_MAX))
          ALLOCATE(MTEMP3(SIZE_MAX,SIZE_MAX),VT(NDDL))
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                         INITIAL SKEW                              CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          FXBIPM(14,NFX) = ADRRPM                  
C 
          SKEW(1:9) = ZERO
          SKEW(1) = ONE
          SKEW(5) = ONE
          SKEW(9) = ONE
          DO I=1,9
            FXBRPM(ADRRPM+I) = SKEW(I)
          ENDDO
C
          ADRRPM=ADRRPM+12
C
C--       Information of boundary nodes is not used for now
          DO I=1,NBNOD
            IF (FXBNOD(ADRNOD+I-1) < 0) THEN
              FXBNOD(ADRNOD+I-1) = ABS(FXBNOD(ADRNOD+I-1))
            ENDIF 
          ENDDO
          FXBIPM(18,NFX) = NBNOD
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                       STIFFNESS MATRIX                            CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          MIN_DIAG_STIF = EP30
          STIF(1:NDDL,1:NDDL) = ZERO
          DO I=1,SIZE_STIFF
            I1 = FXB_MATRIX_ADD(1,ADR_STIFF+I-1) 
            I2 = FXB_MATRIX_ADD(2,ADR_STIFF+I-1)
            IDOF1 = FXB_MATRIX_ADD(3,ADR_STIFF+I-1)
            IDOF2 = FXB_MATRIX_ADD(4,ADR_STIFF+I-1)
            STIF(6*(I1-1)+IDOF1,6*(I2-1)+IDOF2) = FXB_MATRIX(ADR_STIFF+I-1)
            STIF(6*(I2-1)+IDOF2,6*(I1-1)+IDOF1) = FXB_MATRIX(ADR_STIFF+I-1)
            IF ((6*(I2-1)+IDOF2)==(6*(I1-1)+IDOF1)) MIN_DIAG_STIF = MIN(MIN_DIAG_STIF,FXB_MATRIX(ADR_STIFF+I-1))
          ENDDO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                         MASS MATRIX                               CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          MASS(1:NDDL,1:NDDL) = ZERO
C
          IF (SIZE_MASS == 0) THEN
C-- Mass matrix automatically generated from nodal masses and inertia
            DO I=1,NBNOD
              IDN = FXBNOD(ADRNOD+I-1)
              FXBNOD(ADRNOD+I-1) = IDN
              MASS(6*(I-1)+1,6*(I-1)+1) = MS(IDN)
              MASS(6*(I-1)+2,6*(I-1)+2) = MS(IDN)
              MASS(6*(I-1)+3,6*(I-1)+3) = MS(IDN)
              MASS(6*(I-1)+4,6*(I-1)+4) = IN(IDN)
              MASS(6*(I-1)+5,6*(I-1)+5) = IN(IDN)
              MASS(6*(I-1)+6,6*(I-1)+6) = IN(IDN)
            ENDDO
          ELSE
            DO I=1,SIZE_MASS
              I1 = FXB_MATRIX_ADD(1,ADR_MASS+I-1) 
              I2 = FXB_MATRIX_ADD(2,ADR_MASS+I-1)
              IDOF1 = FXB_MATRIX_ADD(3,ADR_MASS+I-1)
              IDOF2 = FXB_MATRIX_ADD(4,ADR_MASS+I-1)
              MASS(6*(I1-1)+IDOF1,6*(I2-1)+IDOF2) = FXB_MATRIX(ADR_MASS+I-1)
              MASS(6*(I2-1)+IDOF2,6*(I1-1)+IDOF1) = FXB_MATRIX(ADR_MASS+I-1)
            ENDDO
C-- Mass of connections nodes is added to the mass matrix
            DO I=1,NBNOD
              IDN = FXBNOD(ADRNOD+I-1)
              FXBNOD(ADRNOD+I-1) = IDN
              MASS(6*(I-1)+1,6*(I-1)+1) = MASS(6*(I-1)+1,6*(I-1)+1)+MS(IDN)
              MASS(6*(I-1)+2,6*(I-1)+2) = MASS(6*(I-1)+2,6*(I-1)+2)+MS(IDN)
              MASS(6*(I-1)+3,6*(I-1)+3) = MASS(6*(I-1)+3,6*(I-1)+3)+MS(IDN)
              MASS(6*(I-1)+4,6*(I-1)+4) = MASS(6*(I-1)+4,6*(I-1)+4)+IN(IDN)
              MASS(6*(I-1)+5,6*(I-1)+5) = MASS(6*(I-1)+5,6*(I-1)+5)+IN(IDN)
              MASS(6*(I-1)+6,6*(I-1)+6) = MASS(6*(I-1)+6,6*(I-1)+6)+IN(IDN)
            ENDDO
          ENDIF

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                         MODES RBODY                               CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          FXBIPM(7,NFX) = ADRMOD
C
          IF (IBLO == 0) THEN
C
            XM = ZERO
            MSTOT = ZERO
            DO I=1,NBNOD
              IDN = FXBNOD(ADRNOD+I-1)
              XM(1) = XM(1) + X(1,IDN)*MS(IDN)
              XM(2) = XM(2) + X(2,IDN)*MS(IDN)
              XM(3) = XM(3) + X(3,IDN)*MS(IDN)
              MSTOT = MSTOT + MS(IDN)
            ENDDO
            DO I=1,3
              XM(I) = XM(I)/MAX(EM20,MSTOT)
            ENDDO
C
            MODES_RB(1:NDDL,1:NME) = ZERO
            MODES_RBT(1:NME,1:NDDL) = ZERO
C
C---------- Modes 1/.../9
            DO I=1,3
              DO J=1,3
                DO K=1,NBNOD
                  IDN = FXBNOD(ADRNOD+K-1)
                  MODES_RB(6*(K-1)+J,3*(I-1)+J) = X(I,IDN) - XM(I)
                ENDDO           
              ENDDO
            ENDDO
C---------- Modes 10/11/12
            DO I=1,3
              DO K=1,NBNOD
                IDN = FXBNOD(ADRNOD+K-1)
                MODES_RB(6*(K-1)+I,9+I) = 1-(X(1,IDN)-XM(1))-(X(2,IDN)-XM(2))-(X(3,IDN)-XM(3))       
              ENDDO
            ENDDO
C---------- Modes rotation
            IF (NME > 12) THEN
              DO I=1,3
                DO K=1,NBNOD
                  MODES_RB(6*(K-1)+3+I,12+I) = ONE       
                ENDDO
              ENDDO
            ENDIF
C --------- Matrice modes RB transposee
            DO I=1,NME
              DO J=1,NDDL
                MODES_RBT(I,J) = MODES_RB(J,I)      
              ENDDO
            ENDDO
C
            DO I=1,NME
              DO J=1,NDDL
                FXBMOD(ADRMOD) = MODES_RB(J,I)
                ADRMOD=ADRMOD+1
              ENDDO
            ENDDO
C
C---------- SCALE FACTOR ON LOCAL MODES FOR MASS CONDITIONING
            FAC2X=PRSCAL(MODES_RB(1,1), MODES_RB(1,1), NDDL, MASS)
            FAC2Y=PRSCAL(MODES_RB(1,4), MODES_RB(1,4), NDDL, MASS)
            FAC2Z=PRSCAL(MODES_RB(1,7), MODES_RB(1,7), NDDL, MASS)
            FAC2 = MAX(FAC2X,FAC2Y,FAC2Z)  
            FAC=SQRT(FAC2)
C
          ELSE
C        
            FAC2 = ONE
            FAC=SQRT(FAC2)
C
          ENDIF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                         STATIC MODES                              CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
          MODES(1:NDDL,1:NMOD) = ZERO
          MODEST(1:NMOD,1:NDDL) = ZERO
          ITAG_DOF(1:6,1:NBNOD) = 0
C
C---      Tag of connected DOF ---
          DO I=1,SIZE_STIFF
              IL1 = FXB_MATRIX_ADD(1,ADR_STIFF+I-1)
            IL2 = FXB_MATRIX_ADD(2,ADR_STIFF+I-1)
            IDOF1 = FXB_MATRIX_ADD(3,ADR_STIFF+I-1)
            IDOF2 = FXB_MATRIX_ADD(4,ADR_STIFF+I-1)
C --        Craig-bampton with only boundary nodes - one static mode per dof in connected
            ITAG_DOF(IDOF1,IL1) = 1
            ITAG_DOF(IDOF2,IL2) = 1                  
          ENDDO
C
          NMST = 0
          DO I=1,NBNOD
            IDN = FXBNOD(ADRNOD+I-1)
C---        Tag BCS ---
            IC = ICODE(IDN)
            IC1=IC/512
            IC2=(IC-512*IC1)/64
            BCS(1) = IC1/4
            BCS(2) = (IC1-4*BCS(1))/2
            BCS(3) = IC1-4*BCS(1)-2*BCS(2)
            BCS(4) = IC2/4
            BCS(5) = (IC2-4*BCS(4))/2
            BCS(6) = IC2-4*BCS(4)-2*BCS(5)

C---        On static mode generated per connected DOF  ---
            DO K=1,6
              IF ((BCS(K)==0).AND.(ITAG_DOF(K,I)>0)) THEN
                NMST = NMST + 1
                MODES(6*(I-1)+K,NMST) = ONE
              ENDIF
            ENDDO      
          ENDDO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC   COMPLEMENT MODES - used for othogonalisation with RB modes      CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          NMR = 0
          IF (IBLO == 0) THEN
C
            CPT = 0
            DO I=1,NBNOD
              IDN = FXBNOD(ADRNOD+I-1)
              DO J=1,6
                DO IM=1,6
                   VECTR(CPT+J,IM)=ZERO
                ENDDO
                SELECT CASE (J)
                CASE (1)
                  VECTR(CPT+J,1)=ONE
                  VECTR(CPT+J,5)=X(3,IDN)-XM(3)
                  VECTR(CPT+J,6)=-(X(2,IDN)-XM(2))
                CASE (2)
                  VECTR(CPT+J,2)=ONE
                  VECTR(CPT+J,4)=-(X(3,IDN)-XM(3))
                  VECTR(CPT+J,6)=X(1,IDN)-XM(1)
                CASE (3)
                  VECTR(CPT+J,3)=ONE
                  VECTR(CPT+J,4)=X(2,IDN)-XM(2)
                  VECTR(CPT+J,5)=-(X(1,IDN)-XM(1))
                CASE (4)
                  VECTR(CPT+J,4)=ONE
                CASE (5)
                  VECTR(CPT+J,5)=ONE
                CASE (6)
                  VECTR(CPT+J,6)=ONE
                END SELECT
              ENDDO
              CPT=CPT+6
            ENDDO
C
C---      Orthonormalisation of VECTR with mass matrix ---
            NMR = 6
            IF (NMST > 0) CALL ORTHRG(VECTR,MASS,NDDL,NMR)
C
          ENDIF 
c
          IF ((IBLO==0).AND.(NMST > 0)) THEN
C---      Orthonormalisation of VECTR and vector of static modes with mass matrix ---
            CALL ORTHSR(MODES,VECTR,MASS,NDDL,NMST,NMR)
          ENDIF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCC   LOOP -> reloop = 0 - all the modes       CCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCC           reloop = 1 - reduced modal bases CCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          RELOOP = 0
          NSAV_MODES = 0
          NMST0 = NMST
          NMOD0 = NMOD
          ADRMOD0 = ADRMOD
          ADRLM0  = ADRLM
          ADRFLS0 = ADRFLS
          LENCP0  =LENCP
          LENLM0  =LENLM
          LENFLS0 =LENFLS
          LENDLS0 =LENDLS
          LENVAR0 =LENVAR
C
          DO RELOOP = 0,1
C
            IF (RELOOP > 0) THEN
              NMST = NSAV_MODES
              NMOD = NSAV_MODES
              ADRMOD = ADRMOD0
              ADRLM  = ADRLM0
              ADRFLS = ADRFLS0
              LENCP  =LENCP0
              LENLM  =LENLM0
              LENFLS =LENFLS0
              LENDLS =LENDLS0
              LENVAR =LENVAR0
              MODES(1:NDDL,1:NMOD) = MODES_SAV(1:NDDL,1:NMOD)
            ENDIF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC               STATIC MODES - ORTHONORMALIZATION                   CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
            IF (NMST > 0) THEN
              NMST_F = 0
C             TOLE = MIN_DIAG_STIF*MIN_DIAG_STIF*EM5*EM5
              TOLE = EM04
              CALL ORTHST(MODES,MASS,NDDL,NMST,NMST_F,TOLE)
              NMST = NMST_F
            ENDIF

C--         Number of static modes is updated
            NMOD = NMST
            FXBIPM(4,NFX) = NMOD
            FXBIPM(5,NFX) = NMST
C    
            NTR = 9
            LENCP =LENCP -NTR*NMOD0*NME+NTR*NMOD*NME
            LENLM =LENLM -NMOD0+NMOD
            LENFLS=LENFLS-NMST0*(2*NMOD0-NMST0+1)/2+NMST*(2*NMOD-NMST+1)/2
            LENDLS=LENDLS-NMOD0+NMST0+NMOD+NMST
            LENVAR=LENVAR-NMOD0+NMOD
            FXBIPM(38,NFX)=MIN(NMOD,FXBIPM(38,NFX))
C
            DO I=1,NDDL
              DO J=1,NMOD
                MODEST(J,I)=MODES(I,J)
              ENDDO
            ENDDO        
C
            DO I=1,NMST
              DO J=1,NDDL
                FXBMOD(ADRMOD) = FAC*MODES(J,I)
                ADRMOD=ADRMOD+1
              ENDDO
            ENDDO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                   REDUCED DIAG MASS MATRIX                        CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
            FXBIPM(10,NFX) = ADRLM
C
            MTEMP(1:NDDL,1:NMOD) = MATMUL(MASS(1:NDDL,1:NDDL),MODES(1:NDDL,1:NMOD))
            MTEMP2(1:NMOD,1:NMOD) = MATMUL(MODEST(1:NMOD,1:NDDL),MTEMP(1:NDDL,1:NMOD))
C
            DO I=1,NMOD
              FXBLM(ADRLM) = FAC2*MTEMP2(I,I)
              ADRLM=ADRLM+1
            ENDDO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                   REDUCED FULL STIFF MATRIX                       CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
            FXBIPM(11,NFX) = ADRFLS
C
            MTEMP(1:NDDL,1:NMST) = MATMUL(STIF(1:NDDL,1:NDDL),MODES(1:NDDL,1:NMST))
            MTEMP3(1:NMST,1:NMST) = MATMUL(MODEST(1:NMST,1:NDDL),MTEMP(1:NDDL,1:NMST))
C
            DO I=1,NMST
              DO J=1,NMST
CC-- Mat sym trian sup
                IF (J >= I) THEN
                  FXBFLS(ADRFLS) = FAC2*MTEMP3(I,J)
                  ADRFLS=ADRFLS+1
                ENDIF
              ENDDO
            ENDDO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                   COMPUTATION OF MAX FREQUENCY                    CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
            ALLOCATE(WI(NMST),WR(NMST),EIG_R(NMST,NMST),EIG_L(NMST,NMST),INVMK(NMST,NMST))
C
            DO I=1,NMST
              DO J=1,NMST
                IF (I==J) THEN
                  MTEMP2(I,J) = ONE/MAX(EM20,MTEMP2(I,J))
                ELSE
                  MTEMP2(I,J) = ZERO
                ENDIF
              ENDDO
            ENDDO
C
            INVMK(1:NMST,1:NMST) = MATMUL(MTEMP2(1:NMST,1:NMST),MTEMP3(1:NMST,1:NMST))
C
#ifndef WITHOUT_LINALG
            LWORK = -1
            CALL DGEEV( 'N','V',NMST,INVMK,NMST,WR,WI,EIG_L,NMST,EIG_R,NMST,WORK1,LWORK,INFO2)
            LWORK = MAX( 1000, INT( WORK1( 1 ) ) )
            ALLOCATE(WORK(LWORK))
C
            CALL DGEEV( 'N','V',NMST,INVMK,NMST,WR,WI,EIG_L,NMST,EIG_R,NMST,WORK,LWORK,INFO2)
#else
            INFO2 = 1
#endif
C
            IF( INFO2>0 ) THEN
              WRITE(*,*)'The algorithm failed to compute eigenvalues.'
              STOP
            END IF
C
            RHO_INVMK = ZERO
            OMEGA_MIN = EP30
            DO I=1,NMST
              IF (WI(I)==ZERO) THEN
                OMEGA_MIN = MIN(OMEGA_MIN,SQRT(WR(I)))
              ENDIF
            ENDDO
C
            OMEGA_MAX = MAX(FREQ_RANGE*OMEGA_MIN,TWO*PI*MIN_CUT_OFF_FREQ)
            DT_MINI = TWO/OMEGA_MAX
            NSAV_MODESN = 0
            DO I=1,NMST
              IF (WI(I)==ZERO) THEN
                RHO_INVMK = MAX(RHO_INVMK,ABS(WR(I)))
                IF ((TWO/SQRT(ABS(WR(I))) > DT_MINI)) THEN
                  NSAV_MODESN = NSAV_MODESN+1
                ENDIF
              ENDIF
            ENDDO
C
            IF ((NMST - NSAV_MODESN > 0).AND.(RELOOP==0)) THEN
              NSAV_MODES = NSAV_MODESN
              ALLOCATE(MODES_SAV(NDDL,NSAV_MODES))
              MODES_SAV(1:NDDL,1:NSAV_MODES) = ZERO
              NN = 0
              DO I=1,NMST
                IF ((TWO/SQRT(ABS(WR(I))) > DT_MINI).AND.((WI(I)==ZERO))) THEN
                  NN = NN + 1 
                  DO J=1,NDDL
                    DO K=1,NMST
                      MODES_SAV(J,NN)=MODES_SAV(J,NN)+MODES(J,K)*EIG_R(K,I)
                    ENDDO
                  ENDDO  
                ENDIF
              ENDDO
            ENDIF
C
            DEALLOCATE(WI,WR,EIG_R,EIG_L,WORK,INVMK)
C
            FXBRPM(FXBIPM(14,NFX))=ZEP9*TWO/SQRT(RHO_INVMK)
C
            IF (NMST - NSAV_MODESN == 0) EXIT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       
          ENDDO
C
          IF (ALLOCATED(MODES_SAV))  DEALLOCATE (MODES_SAV)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                           DAMPING                                 CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          IDAMP = 0
C
          IF (IDAMP == 0) THEN
            FXBRPM(ADRRPM)=ZERO
            FXBRPM(ADRRPM+1)=ZERO
            ADRRPM=ADRRPM+2
          ENDIF
          FXBRPM(ADRRPM)=ZERO
          FXBRPM(ADRRPM+1)=ZERO
          ADRRPM=ADRRPM+2   
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                   MASS MATRIX PROJ ON RB MODES                    CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          FXBIPM(8,NFX) = ADRGLM
C
          MTEMP(1:NDDL,1:NME) = MATMUL(MASS(1:NDDL,1:NDDL),MODES_RB(1:NDDL,1:NME))
          MTEMP2(1:NME,1:NME) = MATMUL(MODES_RBT(1:NME,1:NDDL),MTEMP(1:NDDL,1:NME))
C
          DO I=1,NME
            DO J=1,NME
CC-- Mat sym trian sup
              IF (J >= I) THEN
                FXBGLM(ADRGLM) = MTEMP2(I,J)
                ADRGLM=ADRGLM+1
              ENDIF
            ENDDO
          ENDDO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                   MASS MATRIX COUPLED PROJECTION                  CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          FXBIPM(9,NFX) = ADRCP
C
          DO I = 1,3
            DO J = 1,3
C
              DO K=1,NMOD
                VT(1:NDDL) = ZERO
                CPT = 0
                DO M = 1,NBNOD
                  VT(CPT+I) = MODES(CPT+J,K)
                  VT(CPT+3+I) = MODES(CPT+3+J,K)
                  CPT = CPT + 6
                ENDDO
                DO L=1,NME
                  MTEMP(L,K)=FAC*PRSCAL(VT,MODES_RB(1,L),NDDL,MASS)
                ENDDO
              ENDDO
C
              DO L=1,NME
                DO K=1,NMOD
                  FXBCPM(ADRCP) = MTEMP(L,K)
                  ADRCP=ADRCP+1
                ENDDO
              ENDDO
C
            ENDDO
          ENDDO  
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                   STIFF MATRIX COUPLED PROJECTION                  CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          FXBIPM(9,NFX) = ADRCP2
C
          DO I = 1,3
            DO J = 1,3
C
              DO K=1,NME
                VT(1:NDDL) = ZERO
                CPT = 0
                DO M = 1,NBNOD
                  VT(CPT+I) = MODES_RB(CPT+J,K)
                  VT(CPT+3+I) = MODES_RB(CPT+3+J,K)
                  CPT = CPT + 6
                ENDDO
                DO L=1,NMOD
                  MTEMP(K,L)=FAC*PRSCAL(MODES(1,L),VT,NDDL,STIF)
                ENDDO
              ENDDO
C
              DO L=1,NME
                DO K=1,NMOD
                  FXBCPS(ADRCP2) = MTEMP(L,K)
                  ADRCP2=ADRCP2+1
                ENDDO
              ENDDO
C
            ENDDO
          ENDDO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC                       ADDITIONAL ADDRESSES                        CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          FXBIPM(13,NFX) = ADRVAR 
          ADRVAR=ADRVAR+NMOD+NME
          FXBIPM(15,NFX) = ADRMCD 
          ADRMCD=ADRMCD+NME*NME
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC                         PRINTOUT CONTROL                            CCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          WRITE(IOUT,1100) ID,TRIM(TITR),NMST,NME,NBNOD,FXBRPM(FXBIPM(14,NFX))
          WRITE(IOUT,1200)
          WRITE(IOUT,FMT=FMW_10I)(ITAB(FXBNOD(ADRNOD+I-1)),I=1,NBNOD)
C
          IF (IPRI >= 5) THEN
C
C--       FXB file generation for full output
C
            RPM_L = FXBIPM(14,NFX)
            ADRMODE_L = FXBIPM(7,NFX)
            ADRLM_L = FXBIPM(10,NFX)
            ADRFLS_L = FXBIPM(11,NFX)
            ADRGLM_L = FXBIPM(8,NFX)
            ADRCP_L = FXBIPM(9,NFX)
            ADRCP2_L = FXBIPM(9,NFX)         
C
            REF= 11000
            IF (NFX < 10) THEN
              WRITE(TMP_NAME,'(A,I1,A)') OUTFILE_NAME(1:OUTFILE_NAME_LEN)//"FXB_CONTROL",NFX,"_0000.fxb"
              LEN_TMP_NAME = OUTFILE_NAME_LEN + 21
            ELSE
              WRITE(TMP_NAME,'(A,I2,A)') OUTFILE_NAME(1:OUTFILE_NAME_LEN)//"FXB_CONTROL",NFX,"_0000.fxb"
              LEN_TMP_NAME = OUTFILE_NAME_LEN + 22
            ENDIF
C
            OPEN(UNIT=REF,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN')  
C
            WRITE(REF,'(A)') '# FLEXIBLE BODY DATA'
            WRITE(REF,'(A)') '#--1---|---2---|---3---|---4---|---5---|---6---|---7---|---8---|---9---|--10---|'
            WRITE(REF,'(A)') '#  Param'     
            WRITE(REF,'(A)') "#  Nbmod  Nbstat   Nbnod    Irot   Idamp    Iblo   Ifile  Intype"
C
            WRITE(REF,'(7I8)') NMOD,NMST,NBNOD,ISHELL,IDAMP,IBLO,IFILE
            WRITE(REF,'(A)') '#  Nodes'
C
            CPT = 0
            ZZ = NBNOD / 10
            NLIGNE = CEILING(ZZ)
            DO I=1,NLIGNE
              WRITE(REF,'(10I8)') (ITAB(FXBNOD(ADRNOD+CPT+K-1)),K=1,10)
              CPT = CPT + 10          
            ENDDO
            REST = NBNOD-10*NLIGNE
            IF (REST > 0) WRITE(REF,'(10I8)') (ITAB(FXBNOD(ADRNOD+CPT+K-1)),K=1,REST)
C
            MAX_FREQ = HALF*OMEGA_MAX/PI
            WRITE(REF,'(A)') '#         Mrot11          Mrot12          Mrot13          Mrot21          Mrot22'
            WRITE(REF,'(5(1PE16.9))') FXBRPM(RPM_L+1),FXBRPM(RPM_L+2),FXBRPM(RPM_L+3),FXBRPM(RPM_L+4),FXBRPM(RPM_L+5)
            WRITE(REF,'(A)') '#         Mrot23          Mrot31          Mrot32          Mrot33            Freq'
            WRITE(REF,'(5(1PE16.9))') FXBRPM(RPM_L+6),FXBRPM(RPM_L+7),FXBRPM(RPM_L+8),FXBRPM(RPM_L+9),MAX_FREQ
C
C --- block for damping
C
            IF (IDAMP == 1) THEN
              WRITE(REF,'(A)') '#   RAYLEIGH DAMPING' 
              WRITE(REF,'(A)') ' '
              WRITE(REF,'(A)') ' '
              WRITE(REF,'(2(1PE16.9))') FXBRPM(ADRRPM-4),FXBRPM(ADRRPM-3)           
            ENDIF
C
            WRITE(REF,'(A)') '#   BLOCK5  - RB MODES' 
C
C --- block5 - Modes RBODY
C
            IF (IBLO == 0) THEN
C
              DO I=1,NME
                WRITE(REF,'(A)') '#             X               Y               Z              XX              YY'
                WRITE(REF,'(A)') '#             ZZ'
                DO J=1,NBNOD
                  WRITE(REF,'(5(1PE16.9))') (FXBMOD(ADRMODE_L+K-1),K=1,5)
                  WRITE(REF,'((1PE16.9))') FXBMOD(ADRMODE_L+6-1)
                  ADRMODE_L = ADRMODE_L + 6      
                ENDDO
              ENDDO
C
            ENDIF
C
            WRITE(REF,'(A)') '#   BLOCK6  - REDUCED MODES ROTATION ' 
            WRITE(REF,'(A)') '#             X               Y               Z              XX              YY'
            WRITE(REF,'(A)') '#             ZZ'
C
C --- block7 - Reduced Modes
C
            WRITE(REF,'(A)') '#   BLOCK7  - REDUCED MODES' 
C  
            DO I=1,NMST
              WRITE(REF,'(A)') '#             X               Y               Z              XX              YY'
              WRITE(REF,'(A)') '#             ZZ'
              DO J=1,NBNOD
                WRITE(REF,'(5(1PE16.9))') (FXBMOD(ADRMODE_L+K-1),K=1,5)
                WRITE(REF,'((1PE16.9))') FXBMOD(ADRMODE_L+6-1)
                ADRMODE_L = ADRMODE_L + 6     
              ENDDO
            ENDDO
C
C --- block8 - Diag Mass matrix
C
            WRITE(REF,'(A)') '#   BLOCK8  - REDUCED DIAG MASS MATRIX' 
            WRITE(REF,'(A)') '#             X               Y               Z              XX              YY'
            WRITE(REF,'(A)') '#             ZZ'
C
            ZZ = NMOD / 5
            NLIGNE = CEILING(ZZ)
            DO I=1,NLIGNE
              WRITE(REF,'(5(1PE16.9))') (FXBLM(ADRLM_L+K-1),K=1,5)
              ADRLM_L = ADRLM_L + 5          
            ENDDO
            REST = NMOD-5*NLIGNE
            IF (REST > 0) THEN
              WRITE(REF,'(5(1PE16.9))') (FXBLM(ADRLM_L+K-1),K=1,REST)
              ADRLM_L = ADRLM_L + 1
            ENDIF
C
C --- block9 - Full Stiff matrix
C
            WRITE(REF,'(A)') '#   BLOCK9  - REDUCED FULL STIFF MATRIX' 
            WRITE(REF,'(A)') '#             X               Y               Z              XX              YY'
            WRITE(REF,'(A)') '#             ZZ'
C
            SIZE_MAX = ((NMST+1)*NMST)/2
            ZZ = SIZE_MAX / 5
            NLIGNE = CEILING(ZZ)
            DO I=1,NLIGNE
              WRITE(REF,'(5(1PE16.9))') (FXBFLS(ADRFLS_L+K-1),K=1,5)
              ADRFLS_L = ADRFLS_L + 5         
            ENDDO
            REST = SIZE_MAX-5*NLIGNE
            IF (REST > 0) THEN
              WRITE(REF,'(5(1PE16.9))') (FXBFLS(ADRFLS_L+K-1),K=1,REST)
              ADRFLS_L = ADRFLS_L + 1
            ENDIF
C
C --- block11 - Reduced Mass matrix on rbody modes - diag  
C
            WRITE(REF,'(A)') '#   BLOCK11  - RB MODES RB MATRIX' 
            WRITE(REF,'(A)') '#             X               Y               Z              XX              YY'
            WRITE(REF,'(A)') '#             ZZ'
C
            SIZE_MAX = ((NME+1)*NME)/2
            ZZ = SIZE_MAX / 5
            NLIGNE = CEILING(ZZ)
            DO I=1,NLIGNE
              WRITE(REF,'(5(1PE16.9))') (FXBGLM(ADRGLM_L+K-1),K=1,5)
              ADRGLM_L = ADRGLM_L + 5         
            ENDDO
            REST = SIZE_MAX-5*NLIGNE
            IF (REST > 0) THEN
              WRITE(REF,'(5(1PE16.9))') (FXBGLM(ADRGLM_L+K-1),K=1,REST)
              ADRGLM_L = ADRGLM_L + 1
            ENDIF
C
C --- block12 - Coupled mass projection
C
            WRITE(REF,'(A)') '#   BLOCK12  - COUPLE MASS PROJECTION' 
            WRITE(REF,'(A)') '#             X               Y               Z              XX              YY'
            WRITE(REF,'(A)') '#             ZZ'
C
            DO L=1,9
              WRITE(REF,'(A,7I8)') '#',L
              SIZE_MAX = NME*NMOD
              ZZ = SIZE_MAX / 5
              NLIGNE = CEILING(ZZ)
              DO I=1,NLIGNE
                WRITE(REF,'(5(1PE16.9))') (FXBCPM(ADRCP_L+K-1),K=1,5)
                ADRCP_L = ADRCP_L + 5         
              ENDDO
              REST = SIZE_MAX-5*NLIGNE
              IF (REST > 0) THEN
                WRITE(REF,'(5(1PE16.9))') (FXBCPM(ADRCP_L+K-1),K=1,REST)
                ADRCP_L = ADRCP_L + 1
              ENDIF
            ENDDO
C  
C --- block13 - Coupled stiff projection
C
            WRITE(REF,'(A)') '#   BLOCK13  - COUPLE STIFF PROJECTION' 
            WRITE(REF,'(A)') '#             X               Y               Z              XX              YY'
            WRITE(REF,'(A)') '#             ZZ'
C
            DO L=1,9
              WRITE(REF,'(A,7I8)') '#',L
              SIZE_MAX = NME*NMOD
              ZZ = SIZE_MAX / 5
              NLIGNE = CEILING(ZZ)
              DO I=1,NLIGNE
                WRITE(REF,'(5(1PE16.9))') (FXBCPS(ADRCP2_L+K-1),K=1,5)
                ADRCP2_L = ADRCP2_L + 5         
              ENDDO
              REST = SIZE_MAX-5*NLIGNE
              IF (REST > 0) THEN
                WRITE(REF,'(5(1PE16.9))') (FXBCPS(ADRCP2_L+K-1),K=1,REST)
                ADRCP2_L = ADRCP2_L + 1
              ENDIF
            ENDDO
C
            WRITE(REF,'(A)') '#--1---|---2---|---3---|---4---|---5---|---6---|---7---|---8---|---9---|--10---|'
C
            CLOSE(UNIT=REF,STATUS='KEEP')
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC          FIN  PRINTOUT CONTROL CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
          ENDIF
C
          DEALLOCATE(ITAG_DOF,STIF,MASS,MODES_RB,MODES_RBT)
          DEALLOCATE(MODEST,MODES,VECTR,MTEMP,MTEMP2,MTEMP3,VT)
C
        ENDIF
C
      ENDDO
C
      RETURN
9999  CALL ANCMSG(MSGID=566,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            C1=TITR,
     .            C2=FXBFILE,
     .            C3=NWLINE)
C
      RETURN
C      
1000  FORMAT(/
     . '      FLEXIBLE BODY INITIALIZATION FROM DMIG'/
     . '      ---------------------- ')
C
1100  FORMAT( /6X,'FLEXIBLE BODY ID ',I10,1X,A
     .       /10X,'NUMBER OF MODES                         ',I10
     .       /10X,'NUMBER OF RIGID BODY MODES              ',I10
     .       /10X,'NUMBER OF NODES                         ',I10
     .       /10X,'STABILITY TIME STEP                     ',1PE10.3)
C
1200  FORMAT(/
     . '          LIST OF NODES')
C
      END SUBROUTINE INI_FXBODY
         
         
