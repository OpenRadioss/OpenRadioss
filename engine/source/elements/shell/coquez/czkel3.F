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
Chd|  CZKEL3                        source/elements/shell/coquez/czkel3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        CMATC3                        source/elements/shell/coqueba/cmatc3.F
Chd|        CZBE3                         source/elements/shell/coquez/czbe3.F
Chd|        CZBER3                        source/elements/shell/coquez/czbe3.F
Chd|        CZCOORK3                      source/elements/shell/coquez/czcoork3.F
Chd|        CZLKEC3                       source/elements/shell/coquez/czlkec3.F
Chd|        CZLKECG3                      source/elements/shell/coquez/czlkecg3.F
Chd|        CZLKECR3                      source/elements/shell/coquez/czlkec3.F
Chd|        CZLKECT3                      source/elements/shell/coquez/czlkect3.F
Chd|        CZLKEN3                       source/elements/shell/coquez/czlken3.F
Chd|        CZLKENR3                      source/elements/shell/coquez/czlken3.F
Chd|        CZSUMG3                       source/elements/shell/coquez/czsumg3.F
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
       SUBROUTINE CZKEL3 (
     1            JFT    ,JLT    ,NFT    ,NPT    ,MTN    ,
     2            ITHK   ,ISTRAIN,IPLA   ,PM     ,
     3            GEO    ,IXC    ,ELBUF_STR   ,BUFMAT ,
     4            OFFSET ,INDXOF ,      
     6            IHBE   ,THKE   ,ISMSTR ,X      ,IKGEO  ,
     7            IPM    ,IGEO   ,IEXPAN ,IPARG  ,
     8            KE11   ,KE22   ,KE33   ,KE44   ,   
     9            KE12   ,KE13   ,KE14   ,KE23   ,   
     A            KE24   ,KE34   ,UI     ,RI     ,DRAPE_SH4N,
     B            INDX_DRAPE ,SEDRAPE,NUMEL_DRAPE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
      USE DRAPE_MOD 
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT     ,JLT    ,NFT    ,NPT    ,
     .        MTN     ,ITHK   ,
     .        ISTRAIN ,IPLA   ,OFFSET,IHBE    ,ISMSTR,IKGEO,IEXPAN
      INTEGER ,    INTENT(IN)     ::        SEDRAPE,NUMEL_DRAPE
      INTEGER IXC(NIXC,*),IGEO(NPROPGI,*),IPM(*),IPARG(*)
      INTEGER INDXOF(MVSIZ)
      INTEGER, DIMENSION(SEDRAPE) :: INDX_DRAPE
C     REAL OU REAL*8
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*), BUFMAT(*),   X(3,*),THKE(*)
      my_real    
     .   OFF(MVSIZ),UI(3,4,*),RI(3,4,*)
      my_real    
     .   KE11(6,6,MVSIZ),KE22(6,6,MVSIZ),KE33(6,6,MVSIZ),
     .   KE44(6,6,MVSIZ),KE12(6,6,MVSIZ),KE13(6,6,MVSIZ),
     .   KE14(6,6,MVSIZ),KE23(6,6,MVSIZ),
     .   KE24(6,6,MVSIZ),KE34(6,6,MVSIZ)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (DRAPE_) :: DRAPE_SH4N(NUMELC_DRAPE)
C=======================================================================
c FUNCTION: [K] stiffness Matrix of QEPH element
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   JFT,JLT,NFT   - local element id limit and first id(global)of this element group
c                       NEL=JLT-JFT+1
c  I   NPT,MTN       - num. of integrating point in thickness and material type id
c  I   ITHK          - flag of thickness updating (if >0)
c  I   NCYCLE        - cycle(increment) number
c  I   ISTRAIN       - total strain output flag
c  I   IPLA          -  radial return plasticity compute option
c  I   PM ,GEO       - Material and geometrical property data
c  I   IXC(NIXC,NEL) - connectivity and mid,pid integer data
c  I   BUFMAT()      - internal material data
c  I   INDXOF(NEL)   - deleted element flag (not used in this subroutine)
c  I   ETAG(NEL)     - activating element flag for Eigenvalue analysis
c  I   IDDL(NUMNOD)  - DOF id of node N =IDDL(N)+1,NDOF
c  I   NDOF(NUMNOD)  - Num of DOF (nodal)
c  IO  K_DIAG(NDDL)  - Diagnale terms of assembled [K] NDDL: total number of model DOF 
c  IO  K_LT(NNZK)    - terms of strick triagular of assembled [K] NNZK: number of no-zero terms
c  I   IADK(NDDL),JDIK(NNZK)  - Indice integer tables of Compress format of [K]
c  I   IHBE          - Shell formulation flag (Ishell)
c  I   THKE          - initial thickness
c  I   ISMSTR        - small strain flag
c  I   IKGEO  ,      - geometrical stiffness matrix including (if >0)
c  I   X(3,NUMNOD)         co-ordinate 
c  I   IPM ,IGEO     - Material and geometrical property integer data 
c  I   IEXPAN        - thermo flag used in materials
c  I   IPARG(NG)           element group data
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C   L O C A L   V A R I A B L E S
C--------------------------------
      INTEGER 
     .   I, J,J1,J2, NEL, NPLAT,NLAY,IPLAT(MVSIZ),   
     .   IREP,IBID,EP,IDRIL,IBID1,L_DIRA,L_DIRB,
     .   M,MI,MJ,ISUBSTACK
      INTEGER  MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ),IORTH,IGTYP
      my_real 
     .  X13(MVSIZ),  X24(MVSIZ),  Y13(MVSIZ),  Y24(MVSIZ),
     .  MX13(MVSIZ), MX23(MVSIZ), MX34(MVSIZ),
     .  MY13(MVSIZ), MY23(MVSIZ), MY34(MVSIZ), Z1(MVSIZ),
     .  PX1(MVSIZ), PX2(MVSIZ), PY1(MVSIZ),PY2(MVSIZ),
     .  SX(MVSIZ), SY(MVSIZ), RX(MVSIZ),RY(MVSIZ),
     .  SX2(MVSIZ), SY2(MVSIZ), RX2(MVSIZ),RY2(MVSIZ),
     .  RHX(MVSIZ,4),RHY(MVSIZ,4),SHX(MVSIZ,4),SHY(MVSIZ,4),
     .  PH1(MVSIZ),PH2(MVSIZ),HXX(MVSIZ),HYY(MVSIZ),HXY(MVSIZ)
      my_real 
     .   VQ(MVSIZ,9),AREA(MVSIZ), VQN(MVSIZ,12),THK0(MVSIZ),VOL(MVSIZ),
     .   A_I(MVSIZ), THK2(MVSIZ),HM(MVSIZ,4),HF(MVSIZ,4),HC(MVSIZ,2),
     .   HZ(MVSIZ),DHZ(MVSIZ),HMOR(MVSIZ,2),HFOR(MVSIZ,2),
     .   GS(MVSIZ),HMFOR(MVSIZ,6),TMP,TM11,BID
      my_real 
     .   CORELV(MVSIZ,2,4)
      MY_REAL 
     .    K11(3,3,MVSIZ),K12(3,3,MVSIZ),K13(3,3,MVSIZ),K14(3,3,MVSIZ),
     .    K22(3,3,MVSIZ),K23(3,3,MVSIZ),K24(3,3,MVSIZ),K33(3,3,MVSIZ),
     .    M11(3,3,MVSIZ),M12(3,3,MVSIZ),M13(3,3,MVSIZ),M14(3,3,MVSIZ),
     .    M22(3,3,MVSIZ),M23(3,3,MVSIZ),M24(3,3,MVSIZ),M33(3,3,MVSIZ),
     .    MF11(3,3,MVSIZ),MF12(3,3,MVSIZ),MF13(3,3,MVSIZ),
     .    MF22(3,3,MVSIZ),MF23(3,3,MVSIZ),MF24(3,3,MVSIZ),
     .    FM12(3,3,MVSIZ),FM13(3,3,MVSIZ),FM14(3,3,MVSIZ),
     .    FM23(3,3,MVSIZ),FM24(3,3,MVSIZ),FM34(3,3,MVSIZ),
     .    K34(3,3,MVSIZ),K44(3,3,MVSIZ),M34(3,3,MVSIZ),M44(3,3,MVSIZ),
     .    MF34(3,3,MVSIZ),MF44(3,3,MVSIZ),MF14(3,3,MVSIZ),
     .    MF33(3,3,MVSIZ)
      my_real 
     .   PRX(4,MVSIZ),PRY(4,MVSIZ),PRXY(4,MVSIZ),PHKRX(4,MVSIZ),
     .   PHKRY(4,MVSIZ),PHKRXY(4,MVSIZ),PHERX(4,MVSIZ),PHERY(4,MVSIZ),
     .   PHERXY(4,MVSIZ),PRZ(4,MVSIZ),PHKRZ(4,MVSIZ),PHERZ(4,MVSIZ),
     .   PHKX(MVSIZ),PHKY(MVSIZ),PHEX(MVSIZ),PHEY(MVSIZ) 
      my_real 
     .   EXX(MVSIZ) ,EYY(MVSIZ) ,EXY(MVSIZ)   ,EXZ(MVSIZ) ,EYZ(MVSIZ),
     .   KXX(MVSIZ) ,KYY(MVSIZ) ,KXY(MVSIZ)   ,MOM(3,MVSIZ)
C-----------------------------------------------
      my_real,
     .  DIMENSION(:) ,POINTER  :: DIR_A, DIR_B
      my_real, 
     .    ALLOCATABLE, DIMENSION(:), TARGET :: DIRA,DIRB
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C------------|---------|------------------------------------------------------
C------------|---------|------------------------------------------------------
C--------------------------
C     INITIALISATION
C--------------------------
C      OPEN(UNIT=17,FILE='DEBZ.TMP',STATUS='UNKNOWN',FORM='FORMATTED')
c
      NEL=JLT-JFT+1
      IDRIL = IPARG(41)
      GBUF => ELBUF_STR%GBUF
      IGTYP = IGEO(11,IXC(6,1))
      IREP  = IGEO(6 ,IXC(6,1))
      ISUBSTACK = 0
      BID = ZERO
      NLAY  = ELBUF_STR%NLAY
      L_DIRA = ELBUF_STR%BUFLY(1)%LY_DIRA
      L_DIRB = ELBUF_STR%BUFLY(1)%LY_DIRB
      ALLOCATE(DIRA(NLAY*NEL*L_DIRA))
      ALLOCATE(DIRB(NLAY*NEL*L_DIRB))
      DIRA = ZERO
      DIRB = ZERO
      DIR_A => DIRA(1:NLAY*NEL*L_DIRA)
      DIR_B => DIRB(1:NLAY*NEL*L_DIRB)
      IF (IREP == 0) THEN
        DO J=1,NLAY
          J1 = 1+(J-1)*L_DIRA*NEL
          J2 = J*L_DIRA*NEL
          DIRA(J1:J2) = ELBUF_STR%BUFLY(J)%DIRA(1:NEL*L_DIRA)
        ENDDO
      ENDIF
C
C
       CALL CZCOORK3(JFT ,JLT ,X   ,IXC ,PM  ,   
     1               OFF,AREA,A_I,VQN  ,VQ    ,
     2               X13 ,X24 ,Y13  ,Y24 ,MX13,
     3               MX23,MX34  ,MY13 ,MY23  ,MY34,
     4               Z1  , GEO  , 
     5               ELBUF_STR,GBUF%SMSTR,NLAY,
     6               IREP,NPT,ISMSTR,
     7               DIR_A,DIR_B,PID,MAT,NGL,NPLAT,IPLAT ,
     8               CORELV,OFF,THKE,NEL)
       IF (IKPROJ>0.OR.IDRIL>0) THEN
        DO I=1,3 
        DO J=1,3 
        DO EP=JFT,JLT 
         M11(I,J,EP) =ZERO
         M22(I,J,EP) =ZERO
         M33(I,J,EP) =ZERO
         M44(I,J,EP) =ZERO
         M12(I,J,EP) =ZERO
         M13(I,J,EP) =ZERO
         M14(I,J,EP) =ZERO
         M23(I,J,EP) =ZERO
         M24(I,J,EP) =ZERO
         M34(I,J,EP) =ZERO
         MF11(I,J,EP) =ZERO
         MF22(I,J,EP) =ZERO
         MF33(I,J,EP) =ZERO
         MF44(I,J,EP) =ZERO
         MF12(I,J,EP) =ZERO
         MF13(I,J,EP) =ZERO
         MF14(I,J,EP) =ZERO
         MF23(I,J,EP) =ZERO
         MF24(I,J,EP) =ZERO
         MF34(I,J,EP) =ZERO
         FM12(I,J,EP) =ZERO
         FM13(I,J,EP) =ZERO
         FM14(I,J,EP) =ZERO
         FM23(I,J,EP) =ZERO
         FM24(I,J,EP) =ZERO
         FM34(I,J,EP) =ZERO
        ENDDO
        ENDDO
        ENDDO
       ENDIF
C
      IF (IREP>0) THEN
       CALL CMATC3(JFT    ,JLT       ,PM     ,MAT     ,GEO      ,
     1             PID    ,AREA      ,THK0   ,THK2    ,GBUF%THK ,
     2             THKE   ,VOL       ,MTN    ,NPT     ,ITHK     ,
     3             HM     ,HF        ,HC     ,HZ      ,IGTYP    ,
     4             IORTH  ,HMOR      ,HFOR   ,DIR_A   ,IGEO     ,
     5             IDRIL  ,IHBE      ,HMFOR  ,GS      ,ISUBSTACK,
     6             BID    ,ELBUF_STR ,NLAY   ,DRAPE_SH4N ,NFT   , 
     7             NEL    ,INDX_DRAPE,SEDRAPE,NUMEL_DRAPE)
      ELSE
C
       CALL CMATC3(JFT    ,JLT       ,PM     ,MAT     ,GEO      ,
     1             PID    ,AREA      ,THK0   ,THK2    ,GBUF%THK ,
     2             THKE   ,VOL       ,MTN    ,NPT     ,ITHK     ,
     3             HM     ,HF        ,HC     ,HZ      ,IGTYP    ,
     4             IORTH  ,HMOR      ,HFOR   ,DIRA    ,IGEO     ,
     5             IDRIL  ,IHBE      ,HMFOR  ,GS      ,ISUBSTACK,
     6             BID    ,ELBUF_STR ,NLAY   ,DRAPE_SH4N,  NFT   ,
     7             NEL    ,INDX_DRAPE,SEDRAPE,NUMEL_DRAPE)
      ENDIF
C-----------------------------------------------
C     MATRICE [B]---index changed from here JFT-NPLAT (plat els)+,JLT(warped)
C-----------------------------------------------
        CALL CZBE3(JFT ,JLT  ,AREA ,A_I  ,X13  ,
     2                  X24  ,Y13  ,Y24  ,MX13 ,MX23 ,
     3                  MX34 ,MY13 ,MY23 ,MY34 ,Z1   ,
     4                  PX1  ,PX2  ,PY1  ,PY2  ,RX   ,
     5                  RY   ,SX   ,SY   ,RX2  ,RY2  ,
     6                  SX2  ,SY2  ,RHX  ,RHY  ,SHX  ,
     7                  SHY  ,PH1  ,PH2  ,HXX  ,HYY  ,
     8                  HXY  ,NPLAT,IPLAT)
        DO EP=JFT,JLT 
c         EXX(EP) =PX1(EP)*UI(1,1,EP)+PX2(EP)*UI(1,2,EP)
c     1            -PX1(EP)*UI(1,3,EP)-PX2(EP)*UI(1,4,EP)
c         EYY(EP) =PY1(EP)*UI(2,1,EP)+PY2(EP)*UI(2,2,EP)
c     1            -PY1(EP)*UI(2,3,EP)-PY2(EP)*UI(2,4,EP)
c         EXY(EP) =PX1(EP)*UI(2,1,EP)+PX2(EP)*UI(2,2,EP)
c     1            -PX1(EP)*UI(2,3,EP)-PX2(EP)*UI(2,4,EP)
c     1            +PY1(EP)*UI(1,1,EP)+PY2(EP)*UI(1,2,EP)
c     1            -PY1(EP)*UI(1,3,EP)-PY2(EP)*UI(1,4,EP)
C--------px1'=py2;px2'=py1;py1'=-px2;py2'=-px1;-------------
         TMP=Z1(EP)*FOUR*A_I(EP)
         KXX(EP)=PX1(EP)*RI(2,1,EP)+PX2(EP)*RI(2,2,EP)
     1                  -PX1(EP)*RI(2,3,EP)-PX2(EP)*RI(2,4,EP)
         KYY(EP)=-PY1(EP)*RI(1,1,EP)-PY2(EP)*RI(1,2,EP)
     1                  +PY1(EP)*RI(1,3,EP)+PY2(EP)*RI(1,4,EP)
         KXY(EP)=-PX1(EP)*RI(1,1,EP)-PX2(EP)*RI(1,2,EP)
     1                  +PX1(EP)*RI(1,3,EP)+PX2(EP)*RI(1,4,EP)
     1                  +PY1(EP)*RI(2,1,EP)+PY2(EP)*RI(2,2,EP)
     1                  -PY1(EP)*RI(2,3,EP)-PY2(EP)*RI(2,4,EP)
       print *,'PX1(EP),RY13,PX2(EP),RY24=',PX1(EP),
     1         (RI(2,1,EP)-RI(2,3,EP)),PX2(EP),
     1         (RI(2,2,EP)-RI(2,4,EP))
       print *,'K 0 KXX,KYY,KXY'
       print *,KXX(1),KYY(1),KXY(1)
         KXX(EP) =KXX(EP)+
     1           TMP*(PY2(EP)*UI(1,1,EP)+PY1(EP)*UI(1,2,EP)
     1           -PY2(EP)*UI(1,3,EP)-PY1(EP)*UI(1,4,EP))
         KYY(EP) =KYY(EP)+
     1            TMP*(-PX2(EP)*UI(2,1,EP)-PX1(EP)*UI(2,2,EP)
     1            +PX2(EP)*UI(2,3,EP)+PX1(EP)*UI(2,4,EP))
         KXY(EP) =KXY(EP)+
     1            TMP*(PY2(EP)*UI(2,1,EP)+PY1(EP)*UI(2,2,EP)
     1           -PY2(EP)*UI(2,3,EP)-PY1(EP)*UI(2,4,EP)
     1           -PX2(EP)*UI(1,1,EP)-PX1(EP)*UI(1,2,EP)
     1           +PX2(EP)*UI(1,3,EP)+PX1(EP)*UI(1,4,EP))
        ENDDO
       print *,'K 1 KXX,KYY,KXY'
c       print *,EXX(1),EYY(1),EXY(1)
       print *,KXX(1),KYY(1),KXY(1)
        DO EP=JFT,JLT 
         MOM(1,EP)=HF(EP,1)*KXX(EP)+HF(EP,3)*KYY(EP)+HFOR(EP,1)*KXY(EP)
         MOM(2,EP)=HF(EP,3)*KXX(EP)+HF(EP,2)*KYY(EP)+HFOR(EP,2)*KXY(EP)
         MOM(3,EP)=HFOR(EP,1)*KXX(EP)+HFOR(EP,2)*KYY(EP)+
     1             HF(EP,4)*KXY(EP)
         TMP=VOL(EP)*THK2(EP)
         TM11= -TMP*(PY1(EP)*MOM(2,EP)+PX1(EP)*MOM(3,EP))
        ENDDO
C----------------------------------
C     SOUS-MATRICE DE RIGIDITE [K]
C----------------------------------
C--------------------------
C     1. PARTIE CONSTANTE
C--------------------------
       CALL CZLKEC3(JFT ,JLT   ,VOL  ,THK0 ,THK2 ,
     2              HM  ,HF    ,HZ   ,A_I  ,Z1   ,
     3              PX1 ,PX2   ,PY1  ,PY2  ,NPLAT,
     4              IPLAT,DHZ  ,
     4              K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     5              M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     6              MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     7              MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     8              IORTH,HMOR,HFOR,HMFOR)
C--------------------------
C     2. Cisaillement Transversale (const+hourglass): 
C--------------------------
        DO EP=JFT,JLT 
         GS(EP) =ZERO
        ENDDO
       CALL CZLKECT3(JFT  ,JLT   ,VOL  ,HC   ,RX   ,
     4               RY   ,SX    ,SY   ,RX2  ,RY2  ,
     5               SX2  ,SY2   ,RHX  ,RHY  ,SHX  ,
     6               SHY  ,GS    ,NPLAT ,IPLAT,
     9               K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     A               M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     B               MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     C               MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34) 
       IF (IDRIL>0) THEN
         CALL CZBER3(JFT ,JLT  ,AREA ,A_I  ,X13  ,
     1               X24  ,Y13  ,Y24  ,MX13 ,MX23 ,
     2               MX34 ,MY13 ,MY23 ,MY34 ,Z1   ,
     3               RX   ,RY   ,SX   ,SY   ,PRX  ,
     4               PRY  ,PRXY ,PRZ  ,PHKRX,PHKRY,
     5               PHKRXY,PHERX,PHERY,PHERXY,
     6               PHKRZ,PHERZ ,PHKX ,PHKY ,PHEX ,
     7               PHEY ,IPLAT)
         CALL CZLKECR3(JFT ,JLT   ,VOL  ,THK0 ,THK2 ,
     2                 HM  ,HF    ,HZ   ,A_I  ,Z1   ,
     3                 PX1 ,PX2   ,PY1  ,PY2  ,
     6                 K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     7                 M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     8                 MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     9                 MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     A                 IORTH,HMOR,HFOR ,IPLAT,DHZ  ,
     4                 PRX  ,PRY  ,PRXY ,PRZ ,HMFOR,NPLAT)
       ENDIF
C--------------------------
C     3. PARTIE HOURGLASS
C--------------------------
      GOTO 100
      IF (MTN==19.OR.MTN==15.OR.MTN==25) THEN
C-----------keep isotropic for hourglass part (36 to avoid A12=0 for LAW25)------------
       IBID=36      
       CALL CMATC3(JFT    ,JLT       ,PM     ,MAT     ,GEO      ,
     1             PID    ,AREA      ,THK0   ,THK2    ,GBUF%THK ,
     2             THKE   ,VOL       ,IBID   ,NPT     ,ITHK     ,
     3             HM     ,HF        ,HC     ,HZ      ,IGTYP    ,
     4             IBID1  ,HMOR      ,HFOR   ,DIRA    ,IGEO     ,
     5             IDRIL  ,IHBE      ,HMFOR  ,GS      ,ISUBSTACK,
     6             BID    ,ELBUF_STR ,NLAY   ,DRAPE_SH4N   ,NFT      ,
     7             NEL    ,INDX_DRAPE ,SEDRAPE,NUMEL_DRAPE)
      ENDIF
       CALL CZLKEN3(JFT ,JLT  ,VOL  ,THK0 ,THK2 ,
     2              HM  ,HZ   ,A_I  ,PX1  ,PX2  ,
     3              PY1  ,PY2 ,HXX  ,HYY  ,HXY  ,
     4              PH1  ,PH2  ,Z1  ,NPLAT,IPLAT,DHZ  ,
     5           K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     6           M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     7           MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     8           MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     9           IDRIL )
       IF (IDRIL>0) THEN
         CALL CZLKENR3(JFT ,JLT  ,VOL  ,THK0 ,THK2 ,
     2                 HM  ,HZ   ,A_I  ,PX1  ,PX2  ,
     3                 PY1  ,PY2  ,HXX  ,HYY  ,HXY  ,
     4                 PH1  ,PH2  ,Z1   ,NPLAT,IPLAT,DHZ  ,
     5           K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     6           M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     7           MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     8           MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     9           PHKRX,PHKRY,PHKRXY,PHERX,PHERY,PHERXY,
     A           PHKRZ,PHERZ,PHKX ,PHKY ,PHEX ,PHEY  ) 
       ENDIF
       IF (IKGEO ==1) 
     .  CALL CZLKECG3(JFT ,JLT   ,VOL  ,THK0 ,THK2 ,
     1               PX1  ,PX2  ,PY1   ,PY2  ,RX   ,
     2               RY   ,SX    ,SY   ,RX2  ,RY2  ,
     3               SX2  ,SY2   ,RHX  ,RHY  ,SHX  ,
     4               SHY  ,NPLAT ,IPLAT,GBUF%FOR,GBUF%MOM,
     5               K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     6               M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     7               MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     8               MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     9               IDRIL,IORTH ,NEL)
C--------------------------
C     ASSEMBLE+LOCAL->GLOBAL
C--------------------------
        CALL CZSUMG3(
     1               JFT    ,JLT    ,VQN    ,VQ     ,NPLAT,   
     2               IPLAT  ,
     3               K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4               M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5               MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     6               MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     7               KE11,KE22,KE33,KE44,KE12,KE13,KE14,KE23,
     8               KE24,KE34,CORELV,Z1   ,IDRIL,IORTH)
 100  CONTINUE
C---------constant part in local system
C---------------------------------------
C   ASSEMBLAGE
C---------------------------------------
C---------KII -------- 
       DO I=1,3 
        MI=I+3
        DO J=I,3 
         MJ=J+3
#include "vectorize.inc"
         DO M=JFT,JLT
          EP=IPLAT(M)
          KE11(I,J,EP)=K11(I,J,M)
          KE11(MI,MJ,EP)=M11(I,J,M)
          KE22(I,J,EP)=K22(I,J,M)
          KE22(MI,MJ,EP)=M22(I,J,M)
          KE33(I,J,EP)=K33(I,J,M)
          KE33(MI,MJ,EP)=M33(I,J,M)
          KE44(I,J,EP)=K44(I,J,M)
          KE44(MI,MJ,EP)=M44(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,3 
        DO J=1,3 
         MJ=J+3
#include "vectorize.inc"
         DO M=JFT,JLT
          EP=IPLAT(M)
          KE11(I,MJ,EP)=MF11(I,J,M)
          KE22(I,MJ,EP)=MF22(I,J,M)
          KE33(I,MJ,EP)=MF33(I,J,M)
          KE44(I,MJ,EP)=MF44(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C
C---------KIJ -------- 
       DO I=1,3 
        MI=I+3
        DO J=1,3 
         MJ=J+3
#include "vectorize.inc"
         DO M=JFT,JLT
          EP=IPLAT(M)
          KE12(I,J,EP)=K12(I,J,M)
          KE12(I,MJ,EP)=MF12(I,J,M)
          KE12(MI,J,EP)=FM12(I,J,M)
          KE12(MI,MJ,EP)=M12(I,J,M)
          KE13(I,J,EP)=K13(I,J,M)
          KE13(I,MJ,EP)=MF13(I,J,M)
          KE13(MI,J,EP)=FM13(I,J,M)
          KE13(MI,MJ,EP)=M13(I,J,M)
          KE14(I,J,EP)=K14(I,J,M)
          KE14(I,MJ,EP)=MF14(I,J,M)
          KE14(MI,J,EP)=FM14(I,J,M)
          KE14(MI,MJ,EP)=M14(I,J,M)
          KE23(I,J,EP)=K23(I,J,M)
          KE23(I,MJ,EP)=MF23(I,J,M)
          KE23(MI,J,EP)=FM23(I,J,M)
          KE23(MI,MJ,EP)=M23(I,J,M)
          KE24(I,J,EP)=K24(I,J,M)
          KE24(I,MJ,EP)=MF24(I,J,M)
          KE24(MI,J,EP)=FM24(I,J,M)
          KE24(MI,MJ,EP)=M24(I,J,M)
          KE34(I,J,EP)=K34(I,J,M)
          KE34(I,MJ,EP)=MF34(I,J,M)
          KE34(MI,J,EP)=FM34(I,J,M)
          KE34(MI,MJ,EP)=M34(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,6 
        DO J=I+1,6 
         DO EP=JFT,JLT
          KE11(J,I,EP)=KE11(I,J,EP)
          KE22(J,I,EP)=KE22(I,J,EP)
          KE33(J,I,EP)=KE33(I,J,EP)
          KE44(J,I,EP)=KE44(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
      RETURN
      END
