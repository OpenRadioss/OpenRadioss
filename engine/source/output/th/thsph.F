Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  THSPH                         source/output/th/thsph.F      
Chd|-- called by -----------
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE THSPH(ELBUF_TAB, NTHGRP2, ITHGRP, IPARG, ITHBUF,
     1                 SPBUF    ,    KXSP, NOD2SP,    PM,  WA    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "sphcom.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITHBUF(*),KXSP(NISP,*),NOD2SP(*)
      INTEGER, INTENT(in) :: NTHGRP2
      INTEGER, DIMENSION(NITHGR,*), INTENT(in) :: ITHGRP

      my_real
     .        WA(*),SPBUF(NSPBUF,*),PM(NPROPM,*)

      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: 
     .        ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,JJ, I, J, N, IH, NG, MTE, 
     .        K, IST, IP, L, LWA, NEL,KK(6)
      INTEGER :: NITER,IADR,NN,IADV,NVAR,ITYP,IJK

      my_real
     .        WWA(100)

      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C-----------------------------------------------
C   D e s c r  i p t i o n
C-----------------------------------------------
C Time History output for SPH cells.
C
C-------------------------
C           SMOOTH PARTICLES (CELLS).
C-------------------------
C VAR(OLD) KEY    DESCRIPTION   [MAT LAW]
C
C    1     OFF
C    2     SX     SIGX
C    3     SY     SIGY
C    4     SZ     SIGZ
C    5     SXY    SIGXY
C    6     SYZ    SIGYZ
C    7     SXZ    SIGZX
C    8     IE     INTERNAL ENERGIE / VOLUME0
C    9     DENS   DENSITY
C   10     WFVIS  ARTIFICIAL VISCOSITY FORCES WORK
C   11     VOL    VOLUME (ALE) OR INITIAL VOLUME (LAG)
C   12     PLAS   EPS PLASTIQUE [2,3,4,7,8,9,16,22,23,26,33-38] 
C   13     TEMP   TEMPERATURE   [4,6,7,8,9,11,16,17,26,33-38] 
C   14     PLSR   STRAIN RATE   [4,7,8,9,16,26,33-38] 
C   15     DAMA1  DAMAGE 1      [14] 
C   16     DAMA2  DAMAGE 2      [14] 
C   17     DAMA3  DAMAGE 3      [14] 
C   18     DAMA4  DAMAGE 4      [14] 
C   19     DAMA   DAMAGE        [24] 
C   20(14) SA1    STRESS RE1    [24] 
C   21(15) SA2    STRESS RE2    [24] 
C   22(16) SA3    STRESS RE3    [24] 
C   23(17) CR     CRACKS VOL    [24] 
C   24(18) CAP    CAP PARAM     [24] 
C   25(13) K0     HARD. PARAM   [24] 
C   26(12) RK     TURBUL. ENER. [6,11,17] 
C   27(14) TD     TURBUL. DISS. [6,11,17] 
C   28(14) EFIB   FIBER STRAIN  [14] 
C   29(16) ISTA   PHASE STATE   [16] 
C   30(12) VPLA   VOL. EPS PLA. [10,21] 
C   31(12) BFRAC  BURN FRACTION [5,51] 
C   32(12) WPLA   PLAS. WORK    [14] 
C   35     LSX      SIGMA-X IN LOCAL SYSTEM (ONLY BRICKS)
C   36     LSY      SIGMA-Y IN LOCAL SYSTEM (ONLY BRICKS)
C   37     LSZ      SIGMA-Z IN LOCAL SYSTEM (ONLY BRICKS)
C   38     LSXY     SIGMA-XY IN LOCAL SYSTEM (ONLY BRICKS)
C   39     LSYZ     SIGMA-YZ IN LOCAL SYSTEM (ONLY BRICKS)
C   40     LSXZ     SIGMA-XZ IN LOCAL SYSTEM (ONLY BRICKS)
C   41     DIAMETER PARTICLE DIAMETER
C======================================================================|


        IJK = 0
        DO NITER=1,NTHGRP2
            ITYP=ITHGRP(2,NITER)
            NN  =ITHGRP(4,NITER)
            IADR =ITHGRP(5,NITER)
            NVAR=ITHGRP(6,NITER)
            IADV=ITHGRP(7,NITER)
            II=0
            IF(ITYP==51)THEN
!   -------------------------------
                II=0
                IH=IADR
                           
                DO WHILE((ITHBUF(IH+NN)/=ISPMD).AND.(IH<IADR+NN))               
                    IH = IH + 1                           
                ENDDO                                   
                IF (IH>=IADR+NN) GOTO 666               
                DO NG=1,NGROUP
                    ITY=IPARG(5,NG)

                    IF(ITY==51.OR.ITY==52) THEN
                        CALL INITBUF(IPARG    ,NG      ,                    
     2                                MTE     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3                                NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4                                JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5                                NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6                                IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7                                ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
                        GBUF => ELBUF_TAB(NG)%GBUF
                        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)

                        DO I=1,6
                            KK(I) = NEL*(I-1)
                        ENDDO

                        DO I=1,NEL
                            N=I+NFT
                            JJ = 6*(I-1)
                            K=ITHBUF(IH)
                            IP=ITHBUF(IH+NN)
      
                            IF (K==N)THEN
                                IH=IH+1
                                !---spmd specific treatment---!
                                IF (IMACH==3) THEN
                                    !---search for ii---!
                                    II = ((IH-1) - IADR)*NVAR
                                    DO WHILE((ITHBUF(IH+NN)/=ISPMD).AND.(IH<IADR+NN))
                                        IH = IH + 1
                                    ENDDO
                                ENDIF

                                IF(IH>IADR+NN) GOTO 666
                                !---PROP type VOID ---!
                                DO L=1,100
                                    WWA(L)=ZERO
                                ENDDO
                                !-----------
                                WWA(1) = GBUF%OFF(I)
                                WWA(8) = GBUF%EINT(I)
                                WWA(9) = GBUF%RHO(I)
                                !artificial viscosity forces work.
                                WWA(10)= SPBUF(10,K)
                                WWA(11)= GBUF%VOL(I)
                                WWA(2 )= GBUF%SIG(KK(1)+I)
                                WWA(3 )= GBUF%SIG(KK(2)+I)
                                WWA(4 )= GBUF%SIG(KK(3)+I)
                                WWA(5 )= GBUF%SIG(KK(4)+I)
                                WWA(6 )= GBUF%SIG(KK(5)+I)
                                WWA(7 )= GBUF%SIG(KK(6)+I)
                                !-----------
                                GO TO (150,102,102,104,105,106,104,104,104,110,
     .                                 106,150,150,114,150,104,106,118,150,120,
     .                                 110,102,102,124,150,104,150,150,104,104,
     .                                                   104,104,104,104,104,104,104,104,104,104,
     .               104,104,104,104,104,104,104,104,104,104),MTE
                                GO TO 150
  102                           WWA(12)=GBUF%PLA(I)
                                GO TO 150
  104                           CONTINUE
                                IF (GBUF%G_PLA/=0) WWA(12)=GBUF%PLA(I)
                                IF (GBUF%G_EPSD/=0)WWA(14)=GBUF%EPSD(I)
                                IF (JTHE /= 0) WWA(13)=GBUF%TEMP(I)
                                GOTO 150
  105                           WWA(31)=GBUF%BFRAC(I)
                                GOTO 150
  106                           IF (JTHE /= 0) WWA(13)=LBUF%TEMP(I)
                                WWA(26)=LBUF%RK(I)
                                WWA(27)=LBUF%RE(I)
                                GOTO 150
  110                           WWA(30)=GBUF%PLA(I)
                                GO TO 150
  114                           WWA(32)=LBUF%PLA(I)      !N1
                                WWA(33)=LBUF%SIGF(I)     !N2
                                WWA(28)=LBUF%EPSF(I)     !N3
                                WWA(15)=LBUF%DAM(KK(1)+I)   !N4
                                WWA(16)=LBUF%DAM(KK(2)+I)
                                WWA(17)=LBUF%DAM(KK(3)+I)
                                WWA(18)=LBUF%DAM(KK(4)+I)
                                WWA(34)=LBUF%DAM(KK(5)+I)
                                GOTO 150              
  118                           IF (JTHE /= 0) WWA(13)= LBUF%TEMP(I)
                                GOTO 150
  120                           WWA(12)=ZERO
                                WWA(13)=ZERO
                                GOTO 150
  124                           WWA(19)=LBUF%DAM(KK(1)+I)+LBUF%DAM(KK(2)+I)+LBUF%DAM(KK(3)+I)
                                WWA(20)=LBUF%SIGA(KK(1)+I)
                                WWA(21)=LBUF%SIGA(KK(2)+I)
                                WWA(22)=LBUF%SIGA(KK(3)+I)
                                WWA(23)=LBUF%CRAK(KK(1)+I)+LBUF%CRAK(KK(2)+I)+LBUF%CRAK(KK(3)+I)
                                WWA(24)=LBUF%DSUM(I)
                                WWA(25)=LBUF%VK(I)
  150                           CONTINUE
                                !---diameter---!
                                WWA(41)=SPBUF(1,K)

                                DO L=IADV,IADV+NVAR-1
                                    K=ITHBUF(L)
                                    IJK=IJK+1
                                    WA(IJK)=WWA(K)
                                ENDDO
                                IJK = IJK + 1
                                WA(IJK)= II
                            ENDIF
                        ENDDO
                    ENDIF
                ENDDO
 666    continue
!   -------------------------------
            ENDIF
        ENDDO

C-----------
      RETURN
      END SUBROUTINE THSPH
