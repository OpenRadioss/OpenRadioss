Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    allocbuf_auto   ../starter/source/elements/elbuf_init/allocbuf_auto.F
!||--- called by ------------------------------------------------------
!||    elbuf_ini       ../starter/source/elements/elbuf_init/elbuf_ini.F
!||--- calls      -----------------------------------------------------
!||    ancmsg          ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    message_mod     ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE ALLOCBUF_AUTO(ELBUF_TAB,NLAY,NPTR,NPTS,NPTT,
     .                         NINTLAY,NEL,NPT,NG,NGROUP,ITY,
     .                         IGTYP,NUMMAT,MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MAT_ELEM_MOD            
      USE MESSAGE_MOD
      USE CONSTANT_MOD , ONLY : EP21
C-----------------------------------------------
C   I n c l u d e
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NUMMAT
      INTEGER NLAY,NINTLAY,NPTR,NPTS,NPTT,NEL,NPT,NG,NGROUP,ITY,IGTYP
      TYPE(ELBUF_STRUCT_),TARGET ,DIMENSION(NGROUP)     :: ELBUF_TAB
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IR,IS,IT,Stat,NPG,ILAYER,IPT,IMAT,LEN1,LEN2,LEN_PLAPT,LEN_SIGPT,NPTTOT,IDRAPE
      TYPE(BUF_LAY_)     , POINTER :: BUFLY
      TYPE(BUF_INTLAY_)  , POINTER :: INTLAY
      TYPE(G_BUFEL_)     , POINTER :: GBUF
      TYPE(L_BUFEL_)     , POINTER :: LBUF
      TYPE(BUF_INTLOC_)  , POINTER :: ILBUF
      TYPE (L_BUFEL_DIR_), POINTER :: LBUF_DIR
C=======================================================================
      Stat = 0
      IF (ITY == 1 .OR. ITY == 2 .OR. ITY == 51 .OR. ITY == 101) THEN
        NPG = NPTR*NPTS*NPTT
        NPTTOT = NPT
      ELSEIF (ITY == 3 .OR. ITY == 7) THEN
        NPG = NPTR*NPTS
        NPTTOT  = 0
        DO I=1,NLAY
          NPTTOT = NPTTOT + NPG*ELBUF_TAB(NG)%BUFLY(I)%NPTT
        ENDDO
        IF (NPT == 0) NPTTOT = NPT
      ELSE !IF (ITY == 4 .OR. ITY == 5 .OR. ITY == 6  .OR. ITY == 100)THEN
        NPG = 0
        NPTTOT = 0
      ENDIF
      IDRAPE = ELBUF_TAB(NG)%IDRAPE
c-------------------------------------------------
C     layer buffer allocation
c-------------------------------------------------
      DO I = 1, NLAY                                                        
        BUFLY => ELBUF_TAB(NG)%BUFLY(I)                                     
c
        CALL MY_ALLOC(BUFLY%DMG,NEL*BUFLY%LY_DMG,"BUFLY%DMG")
          BUFLY%DMG = ZERO                                                  
        CALL MY_ALLOC(BUFLY%GAMA,NEL*BUFLY%LY_GAMA,"BUFLY%GAMA")
          BUFLY%GAMA = ZERO                     
       IF(IDRAPE == 0 .OR. (IDRAPE >0 .AND. IGTYP == 17)) THEN
         CALL MY_ALLOC(BUFLY%DIRA,NEL*BUFLY%LY_DIRA,"BUFLY%DIRA")
          BUFLY%DIRA = ZERO                                                 
         CALL MY_ALLOC(BUFLY%DIRB,NEL*BUFLY%LY_DIRB,"BUFLY%DIRB")
           BUFLY%DIRB = ZERO   
       ENDIF         
        CALL MY_ALLOC(BUFLY%CRKDIR,NEL*BUFLY%LY_CRKDIR,"BUFLY%CRKDIR")
          BUFLY%CRKDIR = ZERO        
        CALL MY_ALLOC(BUFLY%OFFPG,NEL*BUFLY%LY_OFFPG,"BUFLY%OFFPG")
          BUFLY%OFFPG = 1
        CALL MY_ALLOC(BUFLY%OFF,NEL*BUFLY%LY_OFF,"BUFLY%OFF")
          BUFLY%OFF = 1
c
        IF (NPG > 1) THEN
          IF (NLAY > 1) THEN
            LEN_PLAPT = NEL*BUFLY%LY_PLAPT
            LEN_SIGPT = NEL*BUFLY%LY_SIGPT
          ELSE
            IF (IGTYP /= 51 .AND. IGTYP /= 52 ) THEN
              LEN_PLAPT = NEL*BUFLY%LY_PLAPT*NPT
              LEN_SIGPT = NEL*BUFLY%LY_SIGPT*NPT
            ELSE
              LEN_PLAPT = NEL*BUFLY%LY_PLAPT*BUFLY%NPTT
              LEN_SIGPT = NEL*BUFLY%LY_SIGPT*BUFLY%NPTT
            ENDIF
          ENDIF
          CALL MY_ALLOC(BUFLY%PLAPT,LEN_PLAPT,"BUFLY%PLAPT")
            BUFLY%PLAPT = ZERO                 
          CALL MY_ALLOC(BUFLY%SIGPT,LEN_SIGPT,"BUFLY%SIGPT")
            BUFLY%SIGPT = ZERO 
        ELSEIF (NPG == 1) THEN
C
C      PLAPT & SIGPT will point to LBUF%PLA and LBUF%SIG after allocation
C
        ENDIF
C
        CALL MY_ALLOC(BUFLY%HOURG,NEL*BUFLY%LY_HOURG,"BUFLY%HOURG")
          BUFLY%HOURG = ZERO
        CALL MY_ALLOC(BUFLY%UELR,NEL*BUFLY%LY_UELR,"BUFLY%UELR")
          BUFLY%UELR = ZERO
        CALL MY_ALLOC(BUFLY%UELR1,NEL*BUFLY%LY_UELR1,"BUFLY%UELR1")
          BUFLY%UELR1 = ZERO
      ENDDO                                                                 
c---
c-------------------------------------------------
C     LAYER Local variables per integration point
c-------------------------------------------------
          IDRAPE = ELBUF_TAB(NG)%IDRAPE
          DO I = 1, NLAY
           BUFLY => ELBUF_TAB(NG)%BUFLY(I)
           IMAT = ELBUF_TAB(NG)%BUFLY(I)%IMAT
           IF (IGTYP == 51 .OR. IGTYP == 52) THEN
             NPTT = BUFLY%NPTT
           ELSE
             NPTT = ELBUF_TAB(NG)%NPTT
           ENDIF
           DO IR = 1,NPTR                                       
            DO IS = 1,NPTS                                      
              DO IT = 1,NPTT  
                LBUF => ELBUF_TAB(NG)%BUFLY(I)%LBUF(IR,IS,IT) 
c
              CALL MY_ALLOC(LBUF%OFF,NEL*BUFLY%L_OFF,"LBUF%OFF")
                  LBUF%OFF = ONE
              CALL MY_ALLOC(LBUF%GAMA,NEL*BUFLY%L_GAMA,"LBUF%GAMA")
                  LBUF%GAMA = ZERO
              CALL MY_ALLOC(LBUF%STRA,NEL*BUFLY%L_STRA,"LBUF%STRA")
                  LBUF%STRA = ZERO
              CALL MY_ALLOC(LBUF%FRAC,NEL*BUFLY%L_FRAC,"LBUF%FRAC")
                  LBUF%FRAC = ZERO
              CALL MY_ALLOC(LBUF%BFRAC,NEL*BUFLY%L_BFRAC,"LBUF%BFRAC")
                  LBUF%BFRAC = ZERO
              CALL MY_ALLOC(LBUF%EINT,NEL*BUFLY%L_EINT,"LBUF%EINT")
                  LBUF%EINT = ZERO
              CALL MY_ALLOC(LBUF%EINS,NEL*BUFLY%L_EINS,"LBUF%EINS")
                  LBUF%EINS = ZERO
              CALL MY_ALLOC(LBUF%RHO,NEL*BUFLY%L_RHO,"LBUF%RHO")
                  LBUF%RHO  = ZERO
              CALL MY_ALLOC(LBUF%DP_DRHO,NEL,"LBUF%DP_DRHO")
                  LBUF%DP_DRHO = ZERO
              CALL MY_ALLOC(LBUF%QVIS,NEL*BUFLY%L_QVIS,"LBUF%QVIS")
                  LBUF%QVIS = ZERO
              CALL MY_ALLOC(LBUF%DELTAX,NEL*BUFLY%L_DELTAX,"LBUF%DELTAX")
                  LBUF%DELTAX = ZERO
              CALL MY_ALLOC(LBUF%VOL,NEL*BUFLY%L_VOL,"LBUF%VOL")
                  LBUF%VOL = ZERO
              CALL MY_ALLOC(LBUF%EPSA,NEL*BUFLY%L_EPSA,"LBUF%EPSA")
                  LBUF%EPSA = ZERO
              CALL MY_ALLOC(LBUF%EPSD,NEL*BUFLY%L_EPSD,"LBUF%EPSD")
                  LBUF%EPSD = ZERO
              CALL MY_ALLOC(LBUF%EPSQ,NEL*BUFLY%L_EPSQ,"LBUF%EPSQ")
                  LBUF%EPSQ = ZERO
              CALL MY_ALLOC(LBUF%EPSF,NEL*BUFLY%L_EPSF,"LBUF%EPSF")
                  LBUF%EPSF = ZERO
              CALL MY_ALLOC(LBUF%PLA,NEL*BUFLY%L_PLA,"LBUF%PLA")
                  LBUF%PLA = ZERO
             CALL MY_ALLOC(LBUF%WPLA,NEL*BUFLY%L_WPLA,"LBUF%WPLA")
                  LBUF%WPLA = ZERO                  
              CALL MY_ALLOC(LBUF%TEMP,NEL*BUFLY%L_TEMP,"LBUF%TEMP")
                  LBUF%TEMP = MIN(BUFLY%L_TEMP,1)*MAT_PARAM(IMAT)%THERM%TINI
              CALL MY_ALLOC(LBUF%TB,NEL*BUFLY%L_TB,"LBUF%TB")
                  LBUF%TB = -EP21
              CALL MY_ALLOC(LBUF%RK,NEL*BUFLY%L_RK,"LBUF%RK")
                  LBUF%RK = ZERO
              CALL MY_ALLOC(LBUF%RE,NEL*BUFLY%L_RE,"LBUF%RE")
                  LBUF%RE = ZERO
              CALL MY_ALLOC(LBUF%VK,NEL*BUFLY%L_VK,"LBUF%VK")
                  LBUF%VK = ZERO
              CALL MY_ALLOC(LBUF%SF,NEL*BUFLY%L_SF,"LBUF%SF")
                  LBUF%SF = ZERO
              CALL MY_ALLOC(LBUF%ROB,NEL*BUFLY%L_ROB,"LBUF%ROB")
                  LBUF%ROB = ZERO
              CALL MY_ALLOC(LBUF%DAM,NEL*BUFLY%L_DAM,"LBUF%DAM")
                  LBUF%DAM = ZERO
              CALL MY_ALLOC(LBUF%DSUM,NEL*BUFLY%L_DSUM,"LBUF%DSUM")
                  LBUF%DSUM = ZERO
              CALL MY_ALLOC(LBUF%DGLO,NEL*BUFLY%L_DGLO,"LBUF%DGLO")
                  LBUF%DGLO = ZERO
              CALL MY_ALLOC(LBUF%CRAK,NEL*BUFLY%L_CRAK,"LBUF%CRAK")
                  LBUF%CRAK = ZERO
              CALL MY_ALLOC(LBUF%EPE,NEL*BUFLY%L_EPE,"LBUF%EPE")
                  LBUF%EPE = ZERO
              CALL MY_ALLOC(LBUF%ANG,NEL*BUFLY%L_ANG,"LBUF%ANG")
                  LBUF%ANG = ZERO
              CALL MY_ALLOC(LBUF%EPC,NEL*BUFLY%L_EPC,"LBUF%EPC")
                  LBUF%EPC = ZERO
              CALL MY_ALLOC(LBUF%XST,NEL*BUFLY%L_XST,"LBUF%XST")
                  LBUF%XST = ZERO
              CALL MY_ALLOC(LBUF%SSP,NEL*BUFLY%L_SSP,"LBUF%SSP")
                  LBUF%SSP = ZERO
              CALL MY_ALLOC(LBUF%Z,NEL*BUFLY%L_Z,"LBUF%Z")
                  LBUF%Z = ZERO
              CALL MY_ALLOC(LBUF%VISC,NEL*BUFLY%L_VISC,"LBUF%VISC")
                  LBUF%VISC = ZERO
              CALL MY_ALLOC(LBUF%SIGL,NEL*BUFLY%L_SIGL,"LBUF%SIGL")
                  LBUF%SIGL = ZERO
              CALL MY_ALLOC(LBUF%SIGV,NEL*BUFLY%L_SIGV,"LBUF%SIGV")
                  LBUF%SIGV = ZERO
              CALL MY_ALLOC(LBUF%SIGA,NEL*BUFLY%L_SIGA,"LBUF%SIGA")
                  LBUF%SIGA = ZERO
              CALL MY_ALLOC(LBUF%SIGB,NEL*BUFLY%L_SIGB,"LBUF%SIGB")
                  LBUF%SIGB = ZERO
              CALL MY_ALLOC(LBUF%SIGC,NEL*BUFLY%L_SIGC,"LBUF%SIGC")
                  LBUF%SIGC = ZERO
              CALL MY_ALLOC(LBUF%SIGD,NEL*BUFLY%L_SIGD,"LBUF%SIGD")
                  LBUF%SIGD = ZERO
              CALL MY_ALLOC(LBUF%SIGF,NEL*BUFLY%L_SIGF,"LBUF%SIGF")
                  LBUF%SIGF = ZERO
              CALL MY_ALLOC(LBUF%SIG,NEL*BUFLY%L_SIG,"LBUF%SIG")
                  LBUF%SIG = ZERO
              CALL MY_ALLOC(LBUF%SIGPLY,NEL*BUFLY%L_SIGPLY,"LBUF%SIGPLY")
                  LBUF%SIGPLY = ZERO
              CALL MY_ALLOC(LBUF%FOR,NEL*BUFLY%L_FOR,"LBUF%FOR")
                  LBUF%FOR = ZERO
              CALL MY_ALLOC(LBUF%MOM,NEL*BUFLY%L_MOM,"LBUF%MOM")
                  LBUF%MOM = ZERO
              CALL MY_ALLOC(LBUF%THK,NEL*BUFLY%L_THK,"LBUF%THK")
                  LBUF%THK = ZERO
              CALL MY_ALLOC(LBUF%SMSTR,NEL*BUFLY%L_SMSTR,"LBUF%SMSTR")
                  LBUF%SMSTR = ZERO
              CALL MY_ALLOC(LBUF%DMG,NEL*BUFLY%L_DMG,"LBUF%DMG")
                  LBUF%DMG = ZERO
              CALL MY_ALLOC(LBUF%FORTH,NEL*BUFLY%L_FORTH,"LBUF%FORTH")
                  LBUF%FORTH = ZERO
              CALL MY_ALLOC(LBUF%EINTTH,NEL*BUFLY%L_EINTTH,"LBUF%EINTTH")
                  LBUF%EINTTH = ZERO
              CALL MY_ALLOC(LBUF%SEQ,NEL*BUFLY%L_SEQ,"LBUF%SEQ")
                  LBUF%SEQ = ZERO
              CALL MY_ALLOC(LBUF%JAC_I,NEL*BUFLY%L_JAC_I,"LBUF%JAC_I")
                  LBUF%JAC_I = ZERO
              CALL MY_ALLOC(LBUF%FAC_YLD,NEL*BUFLY%L_FAC_YLD,"LBUF%FAC_YLD")
                  LBUF%FAC_YLD = ONE
              CALL MY_ALLOC(LBUF%ABURN,NEL*BUFLY%L_ABURN,"LBUF%ABURN")
                  LBUF%ABURN = ZERO
              CALL MY_ALLOC(LBUF%MU,NEL*BUFLY%L_MU,"LBUF%MU")
                  LBUF%MU = ZERO
              CALL MY_ALLOC(LBUF%PLANL,NEL*BUFLY%L_PLANL,"LBUF%PLANL")
                  LBUF%PLANL = ZERO
              CALL MY_ALLOC(LBUF%EPSDNL,NEL*BUFLY%L_EPSDNL,"LBUF%EPSDNL")
                  LBUF%EPSDNL = ZERO
              CALL MY_ALLOC(LBUF%DMGSCL,NEL*BUFLY%L_DMGSCL,"LBUF%DMGSCL")
                  LBUF%DMGSCL = ONE
              CALL MY_ALLOC(LBUF%TSAIWU,NEL*BUFLY%L_TSAIWU,"LBUF%TSAIWU")
                  LBUF%TSAIWU = ZERO

              CALL MY_ALLOC(LBUF%PIJ,NEL*BUFLY%L_PIJ,"LBUF%PIJ")
                  LBUF%PIJ = ZERO
C-------------initialization even in DP in Starter only---                  
              CALL MY_ALLOC(LBUF%VOL0DP,NEL,"LBUF%VOL0DP")
                  LBUF%VOL0DP = ZERO
              ENDDO                                                           
            ENDDO                                                          
           ENDDO 
           IF ( IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
             DO IT= 1,NPTT 
               LBUF_DIR => ELBUF_TAB(NG)%BUFLY(I)%LBUF_DIR(IT)
               CALL MY_ALLOC(LBUF_DIR%DIRA,NEL*BUFLY%LY_DIRA,"LBUF_DIR%DIRA")
               LBUF_DIR%DIRA = ZERO                       
               CALL MY_ALLOC(LBUF_DIR%DIRB,NEL*BUFLY%LY_DIRB,"LBUF_DIR%DIRB")
               LBUF_DIR%DIRB = ZERO  
             ENDDO          
           ENDIF
          ENDDO ! NLAY
c---
C
C layer mean variable (NPG = 1)
C
      DO I = 1, NLAY                                                        
        BUFLY => ELBUF_TAB(NG)%BUFLY(I)                                     
        IF (NPG == 1) THEN
          ILAYER = I
          IF (NLAY > 1) THEN
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = BUFLY%NPTT
              DO IT=1,NPTT
                LBUF => ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(1,1,IT)
                BUFLY%PLAPT(1:NEL*BUFLY%LY_PLAPT) => LBUF%PLA
                BUFLY%SIGPT(1:NEL*BUFLY%LY_SIGPT) => LBUF%SIG
              ENDDO
            ELSE
              LBUF => ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(1,1,1)
              BUFLY%PLAPT(1:NEL*BUFLY%LY_PLAPT) => LBUF%PLA
              BUFLY%SIGPT(1:NEL*BUFLY%LY_SIGPT) => LBUF%SIG
            ENDIF
          ELSE
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = BUFLY%NPTT
              DO IPT = 1,NPTT
                LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,IPT)
                LEN1 = 1+(IPT-1)*NEL*BUFLY%LY_PLAPT
                LEN2 = IPT*NEL*BUFLY%LY_PLAPT
                BUFLY%PLAPT(LEN1:LEN2) => LBUF%PLA
                LEN1 = 1+(IPT-1)*NEL*BUFLY%LY_SIGPT
                LEN2 = IPT*NEL*BUFLY%LY_SIGPT
                BUFLY%SIGPT(LEN1:LEN2) => LBUF%SIG
              ENDDO
            ELSE
              DO IPT = 1,NPT
                LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,IPT)
                LEN1 = 1+(IPT-1)*NEL*BUFLY%LY_PLAPT
                LEN2 = IPT*NEL*BUFLY%LY_PLAPT
                BUFLY%PLAPT(LEN1:LEN2) => LBUF%PLA(1:NEL*BUFLY%L_PLA)
                LEN1 = 1+(IPT-1)*NEL*BUFLY%LY_SIGPT
                LEN2 = IPT*NEL*BUFLY%LY_SIGPT
                BUFLY%SIGPT(LEN1:LEN2) => LBUF%SIG
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDDO
c-------------------------------------------------
C     Inter layers (ply xfem)
c-------------------------------------------------
      DO I = 1, NINTLAY                                                    
        INTLAY => ELBUF_TAB(NG)%INTLAY(I)                                  
        CALL MY_ALLOC(ELBUF_TAB(NG)%INTLAY(I)%EINT,NEL,"ELBUF_TAB(NG)%INTLAY(I)%EINT")
           ELBUF_TAB(NG)%INTLAY(I)%EINT  = ZERO                                         
        CALL MY_ALLOC(ELBUF_TAB(NG)%INTLAY(I)%COUNT,NEL,"ELBUF_TAB(NG)%INTLAY(I)%COUNT")
           ELBUF_TAB(NG)%INTLAY(I)%COUNT = ZERO                                         
c
        DO IR = 1,NPTR                                                   
          DO IS = 1,NPTS                                                 
            ILBUF => ELBUF_TAB(NG)%INTLAY(I)%ILBUF(IR,IS)              
            CALL MY_ALLOC(ILBUF%EPS,NEL*3,"ILBUF%EPS")
              ILBUF%EPS = ZERO                                           
            CALL MY_ALLOC(ILBUF%SIG,NEL*3,"ILBUF%SIG")
              ILBUF%SIG = ZERO                                         
          ENDDO                                                            
        ENDDO                                                            
      ENDDO
c-------------------------------------------------
C       global buffer allocation
c-------------------------------------------------
      GBUF => ELBUF_TAB(NG)%GBUF
      CALL MY_ALLOC(GBUF%NOFF,NEL*GBUF%G_NOFF,"GBUF%NOFF")
         ELBUF_TAB(NG)%GBUF%NOFF = 0
      CALL MY_ALLOC(GBUF%IERR,NEL*GBUF%G_IERR,"GBUF%IERR")
        ELBUF_TAB(NG)%GBUF%IERR = 0                                                   
      CALL MY_ALLOC(GBUF%GAMA,NEL*GBUF%G_GAMA,"GBUF%GAMA")
        ELBUF_TAB(NG)%GBUF%GAMA = ZERO
      CALL MY_ALLOC(GBUF%SMSTR,NEL*GBUF%G_SMSTR,"GBUF%SMSTR")
        ELBUF_TAB(NG)%GBUF%SMSTR = ZERO
      CALL MY_ALLOC(GBUF%HOURG,NEL*GBUF%G_HOURG,"GBUF%HOURG")
        ELBUF_TAB(NG)%GBUF%HOURG = ZERO
      CALL MY_ALLOC(GBUF%TAG22,NEL*GBUF%G_TAG22,"GBUF%TAG22")
        ELBUF_TAB(NG)%GBUF%TAG22 = ZERO                                                                                  
      CALL MY_ALLOC(GBUF%STRA,NEL*GBUF%G_STRA,"GBUF%STRA")
        ELBUF_TAB(NG)%GBUF%STRA = ZERO                                                    
      CALL MY_ALLOC(GBUF%STRW,NEL*GBUF%G_STRW,"GBUF%STRW")
        ELBUF_TAB(NG)%GBUF%STRW = ZERO
      CALL MY_ALLOC(GBUF%SIGI,NEL*GBUF%G_SIGI,"GBUF%SIGI")
        ELBUF_TAB(NG)%GBUF%SIGI = ZERO                                                    
      CALL MY_ALLOC(GBUF%DMG,NEL*GBUF%G_DMG,"GBUF%DMG")
        ELBUF_TAB(NG)%GBUF%DMG = ZERO                                                     
      CALL MY_ALLOC(GBUF%UELR,NEL*GBUF%G_UELR,"GBUF%UELR")
        ELBUF_TAB(NG)%GBUF%UELR = ZERO
      CALL MY_ALLOC(GBUF%UELR1,NEL*GBUF%G_UELR1,"GBUF%UELR1")
        ELBUF_TAB(NG)%GBUF%UELR1 = ZERO
      CALL MY_ALLOC(GBUF%DAMDL,NEL*GBUF%G_DAMDL,"GBUF%DAMDL")
        ELBUF_TAB(NG)%GBUF%DAMDL = ZERO                                                     
      CALL MY_ALLOC(GBUF%FOR,NEL*GBUF%G_FOR,"GBUF%FOR")
        ELBUF_TAB(NG)%GBUF%FOR = ZERO                                          
      CALL MY_ALLOC(GBUF%MOM,NEL*GBUF%G_MOM,"GBUF%MOM")
        ELBUF_TAB(NG)%GBUF%MOM = ZERO                                          
      CALL MY_ALLOC(GBUF%THK,NEL*GBUF%G_THK,"GBUF%THK")
        ELBUF_TAB(NG)%GBUF%THK = ZERO     
      CALL MY_ALLOC(GBUF%THK_I,NEL*GBUF%G_THK_I,"GBUF%THK_I")
        ELBUF_TAB(NG)%GBUF%THK_I = ZERO                                    
      CALL MY_ALLOC(GBUF%JAC_I,NEL*GBUF%G_JAC_I,"GBUF%JAC_I")
        ELBUF_TAB(NG)%GBUF%JAC_I = ZERO                                    
      CALL MY_ALLOC(GBUF%DT,NEL*GBUF%G_DT,"GBUF%DT")
        ELBUF_TAB(NG)%GBUF%DT = ZERO
      CALL MY_ALLOC(GBUF%ISMS,NEL*GBUF%G_ISMS,"GBUF%ISMS")
        ELBUF_TAB(NG)%GBUF%ISMS = 0
      CALL MY_ALLOC(GBUF%STRHG,NEL*GBUF%G_STRHG,"GBUF%STRHG")
        ELBUF_TAB(NG)%GBUF%STRHG = ZERO  
      CALL MY_ALLOC(GBUF%BPRELD,NEL*GBUF%G_BPRELD,"GBUF%BPRELD")
        ELBUF_TAB(NG)%GBUF%BPRELD = ZERO  
      CALL MY_ALLOC(GBUF%COR_NF,NEL*GBUF%G_COR_NF,"GBUF%COR_NF")
        ELBUF_TAB(NG)%GBUF%COR_NF = ZERO
      CALL MY_ALLOC(GBUF%COR_FR,NEL*GBUF%G_COR_FR,"GBUF%COR_FR")
        ELBUF_TAB(NG)%GBUF%COR_FR = ZERO
      CALL MY_ALLOC(GBUF%COR_XR,NEL*GBUF%G_COR_XR,"GBUF%COR_XR")
        ELBUF_TAB(NG)%GBUF%COR_XR = ZERO
      CALL MY_ALLOC(GBUF%MAXFRAC,NEL*GBUF%G_MAXFRAC,"GBUF%MAXFRAC")
        ELBUF_TAB(NG)%GBUF%MAXFRAC = ZERO
      CALL MY_ALLOC(GBUF%MAXEPS,NEL*GBUF%G_MAXEPS,"GBUF%MAXEPS")
        ELBUF_TAB(NG)%GBUF%MAXEPS = ZERO
      CALL MY_ALLOC(GBUF%GAMA_R,NEL*GBUF%G_GAMA_R,"GBUF%GAMA_R")
        ELBUF_TAB(NG)%GBUF%GAMA_R = ZERO
C           Allocate Orthotropic Beta angle (dyna input format)
      CALL MY_ALLOC(GBUF%BETAORTH,NEL*GBUF%G_BETAORTH,"GBUF%BETAORTH")
        ELBUF_TAB(NG)%GBUF%BETAORTH= ZERO
      CALL MY_ALLOC(GBUF%AMU,NEL*GBUF%G_AMU,"GBUF%AMU")
        ELBUF_TAB(NG)%GBUF%AMU = ZERO
      CALL MY_ALLOC(GBUF%SH_IOFFSET,NEL*GBUF%G_SH_IOFFSET,"GBUF%SH_IOFFSET")
        ELBUF_TAB(NG)%GBUF%SH_IOFFSET = 0
      CALL MY_ALLOC(GBUF%EINT_DISTOR,NEL*GBUF%G_EINT_DISTOR,"GBUF%EINT_DISTOR")
        ELBUF_TAB(NG)%GBUF%EINT_DISTOR = ZERO
      CALL MY_ALLOC(GBUF%FOR_G,NEL*GBUF%G_FOR_G,"GBUF%FOR_G")
        ELBUF_TAB(NG)%GBUF%FOR_G = ZERO

c
      IF (NPG == 1) THEN
        ELBUF_TAB(NG)%GBUF%FORPG => GBUF%FOR
        ELBUF_TAB(NG)%GBUF%MOMPG => GBUF%MOM
C
C        ELBUF_TAB(NG)%GBUF%FORPGPINCH => GBUF%FORPINCH ! [PM] add later
C        ELBUF_TAB(NG)%GBUF%MOMPGPINCH => GBUF%MOMPINCH ! [PM] add later
C        ELBUF_TAB(NG)%GBUF%EPGPINCHXZ => GBUF%EPGPINCHXZ ! [PM] add later
C        ELBUF_TAB(NG)%GBUF%EPGPINCHYZ => GBUF%EPGPINCHYZ ! [PM] add later
C        ELBUF_TAB(NG)%GBUF%EPGPINCHZZ => GBUF%EPGPINCHZZ ! [PM] add later
C
        ELBUF_TAB(NG)%GBUF%STRWPG=> GBUF%STRW
        IF (GBUF%G_STRPG<=GBUF%G_STRA) THEN
          ELBUF_TAB(NG)%GBUF%STRPG => GBUF%STRA
        ELSE
          CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%STRPG,NEL*GBUF%G_STRPG,"ELBUF_TAB(NG)%GBUF%STRPG")
          ELBUF_TAB(NG)%GBUF%STRPG = ZERO
        END IF
        ELBUF_TAB(NG)%GBUF%FORPG_G=> GBUF%FOR_G
      ELSEIF (NPG > 1) THEN
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%FORPG,NEL*GBUF%G_FORPG,"ELBUF_TAB(NG)%GBUF%FORPG")
           ELBUF_TAB(NG)%GBUF%FORPG = ZERO
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%MOMPG,NEL*GBUF%G_MOMPG,"ELBUF_TAB(NG)%GBUF%MOMPG")
          ELBUF_TAB(NG)%GBUF%MOMPG = ZERO
C
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%FORPGPINCH,NEL*GBUF%G_FORPGPINCH,"ELBUF_TAB(NG)%GBUF%FORPGPINCH")
           ELBUF_TAB(NG)%GBUF%FORPGPINCH = ZERO
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%MOMPGPINCH,NEL*GBUF%G_MOMPGPINCH,"ELBUF_TAB(NG)%GBUF%MOMPGPINCH")
          ELBUF_TAB(NG)%GBUF%MOMPGPINCH = ZERO
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%EPGPINCHXZ,NEL*GBUF%G_EPGPINCHXZ,"ELBUF_TAB(NG)%GBUF%EPGPINCHXZ")
          ELBUF_TAB(NG)%GBUF%EPGPINCHXZ = ZERO
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%EPGPINCHYZ,NEL*GBUF%G_EPGPINCHYZ,"ELBUF_TAB(NG)%GBUF%EPGPINCHYZ")
          ELBUF_TAB(NG)%GBUF%EPGPINCHYZ = ZERO
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%EPGPINCHZZ,NEL*GBUF%G_EPGPINCHZZ,"ELBUF_TAB(NG)%GBUF%EPGPINCHZZ")
          ELBUF_TAB(NG)%GBUF%EPGPINCHZZ = ZERO
C
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%STRPG,NEL*GBUF%G_STRPG,"ELBUF_TAB(NG)%GBUF%STRPG")
          ELBUF_TAB(NG)%GBUF%STRPG = ZERO
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%STRWPG,NEL*GBUF%G_STRWPG,"ELBUF_TAB(NG)%GBUF%STRWPG")
          ELBUF_TAB(NG)%GBUF%STRWPG = ZERO  
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%FORPG_G,NEL*GBUF%G_FORPG_G,"ELBUF_TAB(NG)%GBUF%FORPG_G")
          ELBUF_TAB(NG)%GBUF%FORPG_G = ZERO  
      ENDIF
c
c-------------------------------------------------
C      IF (NPT == 1) THEN  ! NPT = 1 ! global variables are pointing to local ones    
      IF (NPTTOT == 1) THEN  ! NPTTOT = 1 ! global variables are pointing to local ones, BEAM3 not included    
        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)                                   
c
        ELBUF_TAB(NG)%GBUF%BFRAC  => LBUF%BFRAC                                      
        ELBUF_TAB(NG)%GBUF%OFF    => LBUF%OFF                                        
        ELBUF_TAB(NG)%GBUF%EINT   => LBUF%EINT                                       
        ELBUF_TAB(NG)%GBUF%EINS   => LBUF%EINS                                       
        ELBUF_TAB(NG)%GBUF%RHO    => LBUF%RHO                                        
        ELBUF_TAB(NG)%GBUF%QVIS   => LBUF%QVIS                                       
        ELBUF_TAB(NG)%GBUF%DELTAX => LBUF%DELTAX                                     
        ELBUF_TAB(NG)%GBUF%VOL    => LBUF%VOL                                        
        ELBUF_TAB(NG)%GBUF%EPSD   => LBUF%EPSD                                       
        ELBUF_TAB(NG)%GBUF%EPSQ   => LBUF%EPSQ                                       
        ELBUF_TAB(NG)%GBUF%PLA    => LBUF%PLA            
        ELBUF_TAB(NG)%GBUF%WPLA   => LBUF%WPLA                                      
        ELBUF_TAB(NG)%GBUF%TEMP   => LBUF%TEMP                                       
        ELBUF_TAB(NG)%GBUF%TB     => LBUF%TB                                         
        ELBUF_TAB(NG)%GBUF%RK     => LBUF%RK                                         
        ELBUF_TAB(NG)%GBUF%RE     => LBUF%RE                                         
        ELBUF_TAB(NG)%GBUF%SIG    => LBUF%SIG                                        
        ELBUF_TAB(NG)%GBUF%FORTH  => LBUF%FORTH                                      
        ELBUF_TAB(NG)%GBUF%EINTTH => LBUF%EINTTH
        ELBUF_TAB(NG)%GBUF%SEQ    => LBUF%SEQ
        ELBUF_TAB(NG)%GBUF%ABURN  => LBUF%ABURN   
        ELBUF_TAB(NG)%GBUF%MU     => LBUF%MU                                    
      ELSE  ! NPT > 1 OR BEAM3                                                              
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%BFRAC,NEL*GBUF%G_BFRAC,"ELBUF_TAB(NG)%GBUF%BFRAC")
              ELBUF_TAB(NG)%GBUF%BFRAC = ZERO     
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%OFF,NEL*GBUF%G_OFF,"ELBUF_TAB(NG)%GBUF%OFF")
              ELBUF_TAB(NG)%GBUF%OFF = ONE               
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%EINT,NEL*GBUF%G_EINT,"ELBUF_TAB(NG)%GBUF%EINT")
              ELBUF_TAB(NG)%GBUF%EINT = ZERO               
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%EINS,NEL*GBUF%G_EINS,"ELBUF_TAB(NG)%GBUF%EINS")
              ELBUF_TAB(NG)%GBUF%EINS = ZERO               
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%RHO,NEL*GBUF%G_RHO,"ELBUF_TAB(NG)%GBUF%RHO")
              ELBUF_TAB(NG)%GBUF%RHO = ZERO               
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%QVIS,NEL*GBUF%G_QVIS,"ELBUF_TAB(NG)%GBUF%QVIS")
              ELBUF_TAB(NG)%GBUF%QVIS = ZERO               
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%DELTAX,NEL*GBUF%G_DELTAX,"ELBUF_TAB(NG)%GBUF%DELTAX")
              ELBUF_TAB(NG)%GBUF%DELTAX = ZERO             
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%VOL,NEL*GBUF%G_VOL,"ELBUF_TAB(NG)%GBUF%VOL")
              ELBUF_TAB(NG)%GBUF%VOL = ZERO               
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%EPSD,NEL*GBUF%G_EPSD,"ELBUF_TAB(NG)%GBUF%EPSD")
              ELBUF_TAB(NG)%GBUF%EPSD = ZERO               
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%EPSQ,NEL*GBUF%G_EPSQ,"ELBUF_TAB(NG)%GBUF%EPSQ")
              ELBUF_TAB(NG)%GBUF%EPSQ = ZERO 
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%PLA,NEL*GBUF%G_PLA,"ELBUF_TAB(NG)%GBUF%PLA")
              ELBUF_TAB(NG)%GBUF%PLA = ZERO    
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%WPLA,NEL*GBUF%G_WPLA,"ELBUF_TAB(NG)%GBUF%WPLA")
              ELBUF_TAB(NG)%GBUF%WPLA = ZERO                             
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%TEMP,NEL*GBUF%G_TEMP,"ELBUF_TAB(NG)%GBUF%TEMP")
              ELBUF_TAB(NG)%GBUF%TEMP = ZERO               
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%TB,NEL*GBUF%G_TB,"ELBUF_TAB(NG)%GBUF%TB")
              ELBUF_TAB(NG)%GBUF%TB = -EP21
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%RK,NEL*GBUF%G_RK,"ELBUF_TAB(NG)%GBUF%RK")
              ELBUF_TAB(NG)%GBUF%RK = ZERO               
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%RE,NEL*GBUF%G_RE,"ELBUF_TAB(NG)%GBUF%RE")
              ELBUF_TAB(NG)%GBUF%RE = ZERO               
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%SIG,NEL*GBUF%G_SIG,"ELBUF_TAB(NG)%GBUF%SIG")
              ELBUF_TAB(NG)%GBUF%SIG = ZERO               
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%FORTH,NEL*GBUF%G_FORTH,"ELBUF_TAB(NG)%GBUF%FORTH")
              ELBUF_TAB(NG)%GBUF%FORTH = ZERO  
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%EINTTH,NEL*GBUF%G_EINTTH,"ELBUF_TAB(NG)%GBUF%EINTTH")
              ELBUF_TAB(NG)%GBUF%EINTTH = ZERO               
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%SEQ,NEL*GBUF%G_SEQ,"ELBUF_TAB(NG)%GBUF%SEQ")
              ELBUF_TAB(NG)%GBUF%SEQ = ZERO
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%ABURN,NEL*GBUF%G_ABURN,"ELBUF_TAB(NG)%GBUF%ABURN")
              ELBUF_TAB(NG)%GBUF%ABURN = ZERO 
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%MU,NEL*GBUF%G_MU,"ELBUF_TAB(NG)%GBUF%MU")
              ELBUF_TAB(NG)%GBUF%MU = ZERO 
      ENDIF  ! NPT
c---
      call my_alloc(ELBUF_TAB(NG)%GBUF%FILL, NEL*GBUF%G_FILL, "ELBUF_TAB(NG)%GBUF%FILL", stat=Stat)
        ELBUF_TAB(NG)%GBUF%FILL = ONE    
                                                   
c---
C 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
      IF(IGTYP == 3 .and. ITY == 5) THEN 
        call my_alloc(GBUF%DMGSCL, NEL*GBUF%G_DMGSCL, "GBUF%DMGSCL", stat=Stat)
          GBUF%DMGSCL = ONE
      ENDIF 
      call my_alloc(GBUF%AREA, NEL*GBUF%G_AREA, "GBUF%AREA", stat=Stat)
        GBUF%AREA = ZERO
      call my_alloc(GBUF%SKEW, NEL*GBUF%G_SKEW, "GBUF%SKEW", stat=Stat)
        GBUF%SKEW = ZERO
      call my_alloc(GBUF%LENGTH, NEL*GBUF%G_LENGTH, "GBUF%LENGTH", stat=Stat)
        GBUF%LENGTH = ZERO
      call my_alloc(GBUF%TOTDEPL, NEL*GBUF%G_TOTDEPL, "GBUF%TOTDEPL", stat=Stat)
        GBUF%TOTDEPL = ZERO
      call my_alloc(GBUF%TOTROT, NEL*GBUF%G_TOTROT, "GBUF%TOTROT", stat=Stat)
        GBUF%TOTROT = ZERO
      call my_alloc(GBUF%FOREP, NEL*GBUF%G_FOREP, "GBUF%FOREP", stat=Stat)
        GBUF%FOREP = ZERO
      call my_alloc(GBUF%MOMEP, NEL*GBUF%G_MOMEP, "GBUF%MOMEP", stat=Stat)
        GBUF%MOMEP = ZERO
      call my_alloc(GBUF%DEP_IN_TENS, NEL*GBUF%G_DEP_IN_TENS, "GBUF%DEP_IN_TENS", stat=Stat)
        GBUF%DEP_IN_TENS = ZERO
      call my_alloc(GBUF%DEP_IN_COMP, NEL*GBUF%G_DEP_IN_COMP, "GBUF%DEP_IN_COMP", stat=Stat)
        GBUF%DEP_IN_COMP = ZERO
      call my_alloc(GBUF%ROT_IN_TENS, NEL*GBUF%G_ROT_IN_TENS, "GBUF%ROT_IN_TENS", stat=Stat)
        GBUF%ROT_IN_TENS = ZERO
      call my_alloc(GBUF%ROT_IN_COMP, NEL*GBUF%G_ROT_IN_COMP, "GBUF%ROT_IN_COMP", stat=Stat)
        GBUF%ROT_IN_COMP = ZERO
      call my_alloc(GBUF%POSX, NEL*GBUF%G_POSX, "GBUF%POSX", stat=Stat)
        GBUF%POSX = ZERO
      call my_alloc(GBUF%POSY, NEL*GBUF%G_POSY, "GBUF%POSY", stat=Stat)
        GBUF%POSY = ZERO
      call my_alloc(GBUF%POSZ, NEL*GBUF%G_POSZ, "GBUF%POSZ", stat=Stat)
        GBUF%POSZ = ZERO
      call my_alloc(GBUF%POSXX, NEL*GBUF%G_POSXX, "GBUF%POSXX", stat=Stat)
        GBUF%POSXX = ZERO
      call my_alloc(GBUF%POSYY, NEL*GBUF%G_POSYY, "GBUF%POSYY", stat=Stat)
        GBUF%POSYY = ZERO
      call my_alloc(GBUF%POSZZ, NEL*GBUF%G_POSZZ, "GBUF%POSZZ", stat=Stat)
        GBUF%POSZZ = ZERO
      call my_alloc(GBUF%YIELD, NEL*GBUF%G_YIELD, "GBUF%YIELD", stat=Stat)
        GBUF%YIELD = ZERO
      call my_alloc(GBUF%LENGTH_ERR, NEL*GBUF%G_LENGTH_ERR, "GBUF%LENGTH_ERR", stat=Stat)
        GBUF%LENGTH_ERR = ZERO
      call my_alloc(GBUF%DV, NEL*GBUF%G_DV, "GBUF%DV", stat=Stat)
        GBUF%DV = ZERO
      call my_alloc(GBUF%DFS, NEL*GBUF%G_DFS, "GBUF%DFS", stat=Stat)
        GBUF%DFS = ZERO
      call my_alloc(GBUF%SKEW_ERR, NEL*GBUF%G_SKEW_ERR, "GBUF%SKEW_ERR", stat=Stat)
        GBUF%SKEW_ERR = ZERO
      call my_alloc(GBUF%E6, NEL*GBUF%G_E6, "GBUF%E6", stat=Stat)
        GBUF%E6 = ZERO
      call my_alloc(GBUF%RUPTCRIT, NEL*GBUF%G_RUPTCRIT, "GBUF%RUPTCRIT", stat=Stat)
        GBUF%RUPTCRIT = ZERO
      call my_alloc(GBUF%MASS, NEL*GBUF%G_MASS, "GBUF%MASS", stat=Stat)
        GBUF%MASS = ZERO
      call my_alloc(GBUF%V_REPCVT, NEL*GBUF%G_V_REPCVT, "GBUF%V_REPCVT", stat=Stat)
        GBUF%V_REPCVT = ZERO
      call my_alloc(GBUF%VR_REPCVT, NEL*GBUF%G_VR_REPCVT, "GBUF%VR_REPCVT", stat=Stat)
        GBUF%VR_REPCVT = ZERO
      call my_alloc(GBUF%VAR, NEL*GBUF%G_NUVAR, "GBUF%VAR", stat=Stat)
        GBUF%VAR = ZERO
      call my_alloc(GBUF%VARN, NEL*GBUF%G_NUVARN, "GBUF%VARN", stat=Stat)
        GBUF%VARN = ZERO
      call my_alloc(GBUF%DEFINI, NEL*GBUF%G_DEFINI, "GBUF%DEFINI", stat=Stat)
        GBUF%DEFINI = ZERO
      call my_alloc(GBUF%FORINI, NEL*GBUF%G_FORINI, "GBUF%FORINI", stat=Stat)
        GBUF%FORINI = ZERO
      call my_alloc(GBUF%INIFRIC, NEL*GBUF%G_INIFRIC, "GBUF%INIFRIC", stat=Stat)
        GBUF%INIFRIC = ZERO
c---
      call my_alloc(GBUF%DT_PITER, NEL*GBUF%G_DT_PITER, "GBUF%DT_PITER", stat=Stat)
        GBUF%DT_PITER = ZERO
      call my_alloc(GBUF%IDT_TSH, NEL*GBUF%G_IDT_TSH, "GBUF%IDT_TSH", stat=Stat)
        GBUF%IDT_TSH = 1
C --- needed for batoz shell and /heat/mat 
      IF (NPG == 1) THEN
        ELBUF_TAB(NG)%GBUF%TEMPG => GBUF%TEMP
      ELSEIF (NPG > 1) THEN
        CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%TEMPG,NEL*GBUF%G_TEMPG,"ELBUF_TAB(NG)%GBUF%TEMPG")
        ELBUF_TAB(NG)%GBUF%TEMPG = ZERO  
      ENDIF
C
      call my_alloc(GBUF%SKEW_ID, NEL*GBUF%G_SKEW_ID, "GBUF%SKEW_ID", stat=Stat)
        GBUF%SKEW_ID = 0
      call my_alloc(GBUF%YIELD_IN_COMP, NEL*GBUF%G_YIELD_IN_COMP, "GBUF%YIELD_IN_COMP", stat=Stat)
        GBUF%YIELD_IN_COMP = ZERO
      call my_alloc(GBUF%XXOLD_IN_COMP, NEL*GBUF%G_XXOLD_IN_COMP, "GBUF%XXOLD_IN_COMP", stat=Stat)
        GBUF%XXOLD_IN_COMP = ZERO
c---
C seatbelt elements
c---
      call my_alloc(GBUF%SLIPRING_ID, NEL*GBUF%G_SLIPRING_ID, "GBUF%SLIPRING_ID", stat=Stat)
        GBUF%SLIPRING_ID = 0
      call my_alloc(GBUF%SLIPRING_FRAM_ID, NEL*GBUF%G_SLIPRING_FRAM_ID, "GBUF%SLIPRING_FRAM_ID", stat=Stat)
        GBUF%SLIPRING_FRAM_ID = 0
      call my_alloc(GBUF%SLIPRING_STRAND, NEL*GBUF%G_SLIPRING_STRAND, "GBUF%SLIPRING_STRAND", stat=Stat)
        GBUF%SLIPRING_STRAND = 0
      call my_alloc(GBUF%RETRACTOR_ID, NEL*GBUF%G_RETRACTOR_ID, "GBUF%RETRACTOR_ID", stat=Stat)
        GBUF%RETRACTOR_ID = 0
      call my_alloc(GBUF%RINGSLIP, NEL*GBUF%G_RINGSLIP, "GBUF%RINGSLIP", stat=Stat)
        GBUF%RINGSLIP = ZERO
      call my_alloc(GBUF%ADD_NODE, NEL*GBUF%G_ADD_NODE, "GBUF%ADD_NODE", stat=Stat)
        GBUF%ADD_NODE = 0
      call my_alloc(GBUF%UPDATE, NEL*GBUF%G_UPDATE, "GBUF%UPDATE", stat=Stat)
        GBUF%UPDATE = 0
      call my_alloc(GBUF%FRAM_FACTOR, NEL*GBUF%G_FRAM_FACTOR, "GBUF%FRAM_FACTOR", stat=Stat)
        GBUF%FRAM_FACTOR = ZERO  
      call my_alloc(GBUF%INTVAR, NEL*GBUF%G_INTVAR, "GBUF%INTVAR", stat=Stat)
        GBUF%INTVAR = ZERO          
c---
C ---H3D/TMAX in BUFLEN         
c---
      call my_alloc(GBUF%TM_YIELD, NEL*GBUF%G_TM_YIELD, "GBUF%TM_YIELD", stat=Stat)
                GBUF%TM_YIELD = ZERO
      call my_alloc(GBUF%TM_SEQ, NEL*GBUF%G_TM_SEQ, "GBUF%TM_SEQ", stat=Stat)
                GBUF%TM_SEQ = ZERO
      call my_alloc(GBUF%TM_EINT, NEL*GBUF%G_TM_EINT, "GBUF%TM_EINT", stat=Stat)
                GBUF%TM_EINT = ZERO
      call my_alloc(GBUF%TM_DMG, NEL*GBUF%G_TM_DMG, "GBUF%TM_DMG", stat=Stat)
                GBUF%TM_DMG = ZERO
      call my_alloc(GBUF%TM_SIG1, NEL*GBUF%G_TM_SIG, "GBUF%TM_SIG1", stat=Stat)
                GBUF%TM_SIG1 = ZERO  
      call my_alloc(GBUF%TM_STRA1, NEL*GBUF%G_TM_STRA, "GBUF%TM_STRA1", stat=Stat)
                GBUF%TM_STRA1 = ZERO
      call my_alloc(GBUF%TM_SIG3, NEL*GBUF%G_TM_SIG, "GBUF%TM_SIG3", stat=Stat)
                GBUF%TM_SIG3 = ZERO  
      call my_alloc(GBUF%TM_STRA3, NEL*GBUF%G_TM_STRA, "GBUF%TM_STRA3", stat=Stat)
                GBUF%TM_STRA3 = ZERO
                
      IF (STAT /= 0) THEN                         
            CALL ANCMSG(MSGID=727,MSGTYPE=MSGERROR,ANMODE=ANSTOP,C1='ELEMENT BUFFER')              
      ENDIF                                       


C-----------------------------------------------
      RETURN
      END SUBROUTINE ALLOCBUF_AUTO
