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
Chd|  CZKE3                         source/elements/shell/coquez/czke3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_C4                      source/implicit/assem_c4.F    
Chd|        C4EOFF                        source/elements/shell/coque/c4eoff.F
Chd|        CMATC3                        source/elements/shell/coqueba/cmatc3.F
Chd|        CMATCH3                       source/elements/shell/coqueba/cmatc3.F
Chd|        CMATIP3                       source/elements/shell/coqueba/cmatc3.F
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
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
       SUBROUTINE CZKE3 (
     1            JFT    ,JLT    ,NFT    ,NPT    ,MTN    ,
     2            ITHK   ,NCYCLE ,ISTRAIN,IPLA   ,PM     ,
     3            GEO    ,IXC    ,ELBUF_STR   ,BUFMAT ,
     4            OFFSET ,INDXOF ,ETAG   , IDDL  ,NDOF   ,
     5            K_DIAG ,K_LT  , IADK  ,JDIK  ,     
     6            IHBE   ,THKE   ,ISMSTR ,X      ,IKGEO  ,
     7            IPM    ,IGEO   ,IEXPAN ,IPARG  ,ISUBSTACK,
     8            STACK  ,DRAPE_SH4N , INDX_DRAPE,SEDRAPE,NUMEL_DRAPE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD      
      USE STACK_MOD
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT     ,JLT    ,NFT    ,NPT    , 
     .        MTN     ,ITHK   ,NCYCLE,ISUBSTACK,
     .        ISTRAIN ,IPLA   ,OFFSET,IHBE    ,ISMSTR,IKGEO,IEXPAN
      INTEGER ,    INTENT(IN)     ::        SEDRAPE,NUMEL_DRAPE
      INTEGER IXC(NIXC,*),IGEO(NPROPGI,*),IPM(*),IPARG(*)
      INTEGER INDXOF(MVSIZ),
     .         ETAG(*),IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*)
      INTEGER, DIMENSION(SEDRAPE) :: INDX_DRAPE
     
C     REAL OU REAL*8
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*),BUFMAT(*),X(3,*),THKE(*),
     .   OFF(MVSIZ),K_DIAG(*) ,K_LT(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
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
     .   I, J,J1,J2, NEL, NPLAT,IPLAT(MVSIZ), NLAY,L_DIRA,L_DIRB,
     .   IREP,IBID,EP,IDRIL,IBID1
      INTEGER  MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ),IORTH,IGTYP,IUN
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
     .   GS(MVSIZ),HMFOR(MVSIZ,6)
      my_real 
     .   CORELV(MVSIZ,2,4)
      MY_REAL 
     .    K11(9,MVSIZ),K12(9,MVSIZ),K13(9,MVSIZ),K14(9,MVSIZ),
     .    K22(9,MVSIZ),K23(9,MVSIZ),K24(9,MVSIZ),K33(9,MVSIZ),
     .    M11(9,MVSIZ),M12(9,MVSIZ),M13(9,MVSIZ),M14(9,MVSIZ),
     .    M22(9,MVSIZ),M23(9,MVSIZ),M24(9,MVSIZ),M33(9,MVSIZ),
     .    MF11(9,MVSIZ),MF12(9,MVSIZ),MF13(9,MVSIZ),MF14(9,MVSIZ),
     .    MF22(9,MVSIZ),MF23(9,MVSIZ),MF24(9,MVSIZ),MF33(9,MVSIZ),
     .    FM12(9,MVSIZ),FM13(9,MVSIZ),FM14(9,MVSIZ),
     .    FM23(9,MVSIZ),FM24(9,MVSIZ),FM34(9,MVSIZ),
     .    K34(9,MVSIZ),K44(9,MVSIZ),M34(9,MVSIZ),M44(9,MVSIZ),
     .    MF34(9,MVSIZ),MF44(9,MVSIZ)
      my_real 
     .   PRX(4,MVSIZ),PRY(4,MVSIZ),PRXY(4,MVSIZ),PHKRX(4,MVSIZ),
     .   PHKRY(4,MVSIZ),PHKRXY(4,MVSIZ),PHERX(4,MVSIZ),PHERY(4,MVSIZ),
     .   PHERXY(4,MVSIZ),PRZ(4,MVSIZ),PHKRZ(4,MVSIZ),PHERZ(4,MVSIZ),
     .   PHKX(MVSIZ),PHKY(MVSIZ),PHEX(MVSIZ),PHEY(MVSIZ) 
      my_real    
     .   KE11(36,MVSIZ),KE22(36,MVSIZ),KE33(36,MVSIZ),KE44(36,MVSIZ),
     .   KE12(36,MVSIZ),KE13(36,MVSIZ),KE14(36,MVSIZ),KE23(36,MVSIZ),
     .   KE24(36,MVSIZ),KE34(36,MVSIZ)
C-----------------------------------------------
      my_real,
     .  DIMENSION(:) ,POINTER  :: DIR_A, DIR_B
      my_real, 
     .    ALLOCATABLE, DIMENSION(:), TARGET :: DIRA,DIRB
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C------------|---------|------------------------------------------------------
C--------------------------
C     INITIALISATION
C--------------------------
C      OPEN(UNIT=17,FILE='DEBZ.TMP',STATUS='UNKNOWN',FORM='FORMATTED')
c
      GBUF => ELBUF_STR%GBUF
      NEL=JLT-JFT+1
      IDRIL = IPARG(41)
C
      IGTYP = IGEO(11,IXC(6,1))
      IREP  = IGEO(6 ,IXC(6,1))
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
       CALL CZCOORK3(JFT ,JLT ,X   ,IXC ,PM  ,   
     1               GBUF%OFF,AREA,A_I,VQN  ,VQ    ,
     2               X13 ,X24 ,Y13  ,Y24 ,MX13,
     3               MX23,MX34  ,MY13 ,MY23  ,MY34,
     4               Z1  , GEO  ,
     5               ELBUF_STR,GBUF%SMSTR,NLAY,
     6               IREP,NPT,ISMSTR,
     7               DIR_A,DIR_B,PID,MAT,NGL,NPLAT,IPLAT ,
     8               CORELV,OFF,THKE,NEL)
       IF (IKPROJ>0.OR.IDRIL>0) THEN
        DO I=1,9 
        DO EP=JFT,JLT 
         M11(I,EP) =ZERO
         M22(I,EP) =ZERO
         M33(I,EP) =ZERO
         M44(I,EP) =ZERO
         M12(I,EP) =ZERO
         M13(I,EP) =ZERO
         M14(I,EP) =ZERO
         M23(I,EP) =ZERO
         M24(I,EP) =ZERO
         M34(I,EP) =ZERO
         MF11(I,EP) =ZERO
         MF22(I,EP) =ZERO
         MF33(I,EP) =ZERO
         MF44(I,EP) =ZERO
         MF12(I,EP) =ZERO
         MF13(I,EP) =ZERO
         MF14(I,EP) =ZERO
         MF23(I,EP) =ZERO
         MF24(I,EP) =ZERO
         MF34(I,EP) =ZERO
         FM12(I,EP) =ZERO
         FM13(I,EP) =ZERO
         FM14(I,EP) =ZERO
         FM23(I,EP) =ZERO
         FM24(I,EP) =ZERO
         FM34(I,EP) =ZERO
        ENDDO
        ENDDO
       ENDIF
      IF (IREP>0) THEN
       CALL CMATC3(JFT    ,JLT       ,PM     ,MAT     ,GEO      ,
     1             PID    ,AREA      ,THK0   ,THK2    ,GBUF%THK ,
     2             THKE   ,VOL       ,MTN    ,NPT     ,ITHK     ,
     3             HM     ,HF        ,HC     ,HZ      ,IGTYP    ,
     4             IORTH  ,HMOR      ,HFOR   ,DIR_A   ,IGEO     ,
     5             IDRIL  ,IHBE      ,HMFOR  ,GS      ,ISUBSTACK,
     6             STACK  ,ELBUF_STR ,NLAY   ,DRAPE_SH4N   ,NFT      ,
     7             NEL    ,INDX_DRAPE,SEDRAPE,NUMEL_DRAPE)
      ELSE
       CALL CMATC3(JFT    ,JLT       ,PM     ,MAT     ,GEO      ,
     1             PID    ,AREA      ,THK0   ,THK2    ,GBUF%THK ,
     2             THKE   ,VOL       ,MTN    ,NPT     ,ITHK     ,
     3             HM     ,HF        ,HC     ,HZ      ,IGTYP    ,
     4             IORTH  ,HMOR      ,HFOR   ,DIRA    ,IGEO     ,
     5             IDRIL  ,IHBE      ,HMFOR  ,GS      ,ISUBSTACK,
     6             STACK  ,ELBUF_STR ,NLAY   ,DRAPE_SH4N   ,NFT      ,
     7             NEL    ,INDX_DRAPE,SEDRAPE,NUMEL_DRAPE)
      ENDIF
C-----------------------------------------------
C     IF [KT]
C-----------------------------------------------
       IUN = 1
        CALL CMATIP3(JFT    ,JLT     ,PM     ,MAT     ,PID    ,
     1               MTN    ,NPT     ,HM     ,HF      ,IORTH  ,
     2               HMOR   ,HFOR    ,HMFOR  ,IUN      )
C      
      IF (IORTH >0 .AND.IKPROJ<=0 .AND.IDRIL==0 ) THEN
        DO I=1,9 
        DO EP=JFT,JLT 
         MF11(I,EP) =ZERO
         MF22(I,EP) =ZERO
         MF33(I,EP) =ZERO
         MF44(I,EP) =ZERO
         MF12(I,EP) =ZERO
         MF13(I,EP) =ZERO
         MF14(I,EP) =ZERO
         MF23(I,EP) =ZERO
         MF24(I,EP) =ZERO
         MF34(I,EP) =ZERO
         FM12(I,EP) =ZERO
         FM13(I,EP) =ZERO
         FM14(I,EP) =ZERO
         FM23(I,EP) =ZERO
         FM24(I,EP) =ZERO
         FM34(I,EP) =ZERO
        ENDDO
        ENDDO
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
C--------------modif here!!! with IKT>0---add ET in HM,HF
      IF ( IORTH >0 .OR. MTN == 27) THEN
c      IF (MTN==19.OR.MTN==15.OR.MTN==25) THEN
C-----------keep elastic-isotropic for hourglass part ------------
       CALL CMATCH3(JFT    ,JLT     ,PM     ,MAT     ,GEO   ,
     1             PID     ,MTN     ,IDRIL  ,IGEO    ,HM    ,
     2             HF      ,HZ      )
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
     8               KE24,KE34,CORELV,Z1   ,IDRIL ,IORTH)
C
             IF (NEIG>0) CALL C4EOFF(
     1                   JFT, JLT, IXC, ETAG, OFF)
C
             CALL ASSEM_C4(
     1         IXC       ,NEL       ,IDDL      ,NDOF      ,K_DIAG    ,
     2         K_LT      ,IADK      ,JDIK      ,KE11      ,KE12      ,
     3         KE13      ,KE14      ,KE22      ,KE23      ,KE24      ,     
     5         KE33      ,KE34      ,KE44      ,OFF       )
C
      RETURN
      END
Chd|====================================================================
Chd|  CZFKEU3                       source/elements/shell/coquez/czke3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        CZFIKIJUJ                     source/elements/shell/coquez/czke3.F
Chd|====================================================================
       SUBROUTINE CZFKEU3 (
     1            JFT    ,JLT    ,   
     8            KE11   ,KE22   ,KE33   ,KE44   ,   
     8            KE12   ,KE13   ,KE14   ,KE23   ,   
     8            KE24   ,KE34   ,UI     ,RI     ,
     9            FI     ,MI     )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT     ,JLT 
      my_real    
     .   UI(3,4,MVSIZ),RI(3,4,MVSIZ),FI(3,4,MVSIZ),MI(3,4,MVSIZ)
      my_real    
     .   KE11(36,MVSIZ),KE22(36,MVSIZ),KE33(36,MVSIZ),KE44(36,MVSIZ),
     .   KE12(36,MVSIZ),KE13(36,MVSIZ),KE14(36,MVSIZ),KE23(36,MVSIZ),
     .   KE24(36,MVSIZ),KE34(36,MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C   L O C A L   V A R I A B L E S
C--------------------------------
      INTEGER 
     .   I, J, M ,EP,ITRAN0,ITRAN1
      my_real    
     .   FJ(3),MJ(3),UJ(3),RJ(3)
C
      ITRAN0=0
      ITRAN1=1
      DO I=1,3
      DO J=1,4
      DO EP=JFT ,JLT 
       FI(I,J,EP)=ZERO
       MI(I,J,EP)=ZERO
      END DO
      END DO
      END DO
C
      DO EP=JFT ,JLT 
       J=1
       DO I=1,3
        UJ(I)=UI(I,J,EP)
        RJ(I)=RI(I,J,EP)
       END DO
       print *,'KE(1,4),KE(1,5),KE(1,6)='
       print *,KE11(19,1),KE11(25,1),KE11(31,1)
       print *,KE11(19,1)*RJ(1),KE11(25,1)*RJ(2),KE11(31,1)*RJ(3)
       CALL CZFIKIJUJ (KE11(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN0  )
       DO I=1,3
        FI(I,1,EP)=FI(I,1,EP)+FJ(I)
        MI(I,1,EP)=MI(I,1,EP)+MJ(I)
       END DO
       print *,'KE12(1,4),KE(1,5),KE(1,6)='
       print *,KE12(19,1),KE12(25,1),KE12(31,1)
       print *,KE12(19,1)*RJ(1),KE12(25,1)*RJ(2),KE12(31,1)*RJ(3)
       CALL CZFIKIJUJ (KE12(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN1  )
       DO I=1,3
        FI(I,2,EP)=FI(I,2,EP)+FJ(I)
        MI(I,2,EP)=MI(I,2,EP)+MJ(I)
       END DO
       print *,'KE(1,4),KE(1,5),KE(1,6)='
       print *,KE13(19,1),KE13(25,1),KE13(31,1)
       print *,KE13(19,1)*RJ(1),KE13(25,1)*RJ(2),KE13(31,1)*RJ(3)
       CALL CZFIKIJUJ (KE13(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN1  )
       DO I=1,3
        FI(I,3,EP)=FI(I,3,EP)+FJ(I)
        MI(I,3,EP)=MI(I,3,EP)+MJ(I)
       END DO
       print *,'KE14(1,4),KE(1,5),KE(1,6)='
       print *,KE14(19,1),KE14(25,1),KE14(31,1)
       print *,KE14(19,1)*RJ(1),KE14(25,1)*RJ(2),KE14(31,1)*RJ(3)
       CALL CZFIKIJUJ (KE14(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN1  )
       DO I=1,3
        FI(I,4,EP)=FI(I,4,EP)+FJ(I)
        MI(I,4,EP)=MI(I,4,EP)+MJ(I)
       END DO
       J=2
       DO I=1,3
        UJ(I)=UI(I,J,EP)
        RJ(I)=RI(I,J,EP)
       END DO
       CALL CZFIKIJUJ (KE12(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN0  )
       DO I=1,3
        FI(I,1,EP)=FI(I,1,EP)+FJ(I)
        MI(I,1,EP)=MI(I,1,EP)+MJ(I)
       END DO
       CALL CZFIKIJUJ (KE22(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN0  )
       DO I=1,3
        FI(I,2,EP)=FI(I,2,EP)+FJ(I)
        MI(I,2,EP)=MI(I,2,EP)+MJ(I)
       END DO
       CALL CZFIKIJUJ (KE23(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN1  )
       DO I=1,3
        FI(I,3,EP)=FI(I,3,EP)+FJ(I)
        MI(I,3,EP)=MI(I,3,EP)+MJ(I)
       END DO
       CALL CZFIKIJUJ (KE24(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN1  )
       DO I=1,3
        FI(I,4,EP)=FI(I,4,EP)+FJ(I)
        MI(I,4,EP)=MI(I,4,EP)+MJ(I)
       END DO
       J=3
       DO I=1,3
        UJ(I)=UI(I,J,EP)
        RJ(I)=RI(I,J,EP)
       END DO
       CALL CZFIKIJUJ (KE13(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN0  )
       DO I=1,3
        FI(I,1,EP)=FI(I,1,EP)+FJ(I)
        MI(I,1,EP)=MI(I,1,EP)+MJ(I)
       END DO
       CALL CZFIKIJUJ (KE23(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN0  )
       DO I=1,3
        FI(I,2,EP)=FI(I,2,EP)+FJ(I)
        MI(I,2,EP)=MI(I,2,EP)+MJ(I)
       END DO
       CALL CZFIKIJUJ (KE33(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN0  )
       DO I=1,3
        FI(I,3,EP)=FI(I,3,EP)+FJ(I)
        MI(I,3,EP)=MI(I,3,EP)+MJ(I)
       END DO
       CALL CZFIKIJUJ (KE34(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN1  )
       DO I=1,3
        FI(I,4,EP)=FI(I,4,EP)+FJ(I)
        MI(I,4,EP)=MI(I,4,EP)+MJ(I)
       END DO
       J=4
       DO I=1,3
        UJ(I)=UI(I,J,EP)
        RJ(I)=RI(I,J,EP)
       END DO
       CALL CZFIKIJUJ (KE14(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN0  )
       DO I=1,3
        FI(I,1,EP)=FI(I,1,EP)+FJ(I)
        MI(I,1,EP)=MI(I,1,EP)+MJ(I)
       END DO
       CALL CZFIKIJUJ (KE24(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN0  )
       DO I=1,3
        FI(I,2,EP)=FI(I,2,EP)+FJ(I)
        MI(I,2,EP)=MI(I,2,EP)+MJ(I)
       END DO
       CALL CZFIKIJUJ (KE34(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN0  )
       DO I=1,3
        FI(I,3,EP)=FI(I,3,EP)+FJ(I)
        MI(I,3,EP)=MI(I,3,EP)+MJ(I)
       END DO
       CALL CZFIKIJUJ (KE44(1,EP),UJ     ,RJ     ,FJ     ,MJ     ,
     .                ITRAN0  )
       DO I=1,3
        FI(I,4,EP)=FI(I,4,EP)+FJ(I)
        MI(I,4,EP)=MI(I,4,EP)+MJ(I)
       END DO
      END DO
C      
      RETURN
      END
Chd|====================================================================
Chd|  CZFIKIJUJ                     source/elements/shell/coquez/czke3.F
Chd|-- called by -----------
Chd|        CZFKEU3                       source/elements/shell/coquez/czke3.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE CZFIKIJUJ (KEIJ   ,UJ     ,RJ     ,FI     ,MI     ,
     .                      ITRAN   )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER ITRAN
      my_real    
     .   UJ(3),RJ(3),FI(3),MI(3)
      my_real    
     .   KEIJ(6,6)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER 
     .   I, J, M  
C
        DO I=1,3
         FI(I)=ZERO
         MI(I)=ZERO
        END DO
C        
       IF (ITRAN == 0) THEN
        DO I=1,3
         DO J=1,3
          FI(I)=FI(I)+KEIJ(I,J)*UJ(J)+KEIJ(I,J+3)*RJ(J)
          MI(I)=MI(I)+KEIJ(I+3,J)*UJ(J)+KEIJ(I+3,J+3)*RJ(J)
         END DO
        END DO
       ELSE
        DO I=1,3
         DO J=1,3
          FI(I)=FI(I)+KEIJ(J,I)*UJ(J)+KEIJ(J+3,I)*RJ(J)
          MI(I)=MI(I)+KEIJ(J,I+3)*UJ(J)+KEIJ(J+3,I+3)*RJ(J)
         END DO
        END DO
       END IF
      RETURN
      END
