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
Chd|  CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_C4                      source/implicit/assem_c4.F    
Chd|        C4EOFF                        source/elements/shell/coque/c4eoff.F
Chd|        CBABE3                        source/elements/shell/coqueba/cbabe3.F
Chd|        CBABEC3                       source/elements/shell/coqueba/cbabe3.F
Chd|        CBABER3                       source/elements/shell/coqueba/cbabe3.F
Chd|        CBACOORK                      source/elements/shell/coqueba/cbacoork.F
Chd|        CBADERIRZ                     source/elements/shell/coqueba/cbadef.F
Chd|        CBAINI3                       source/elements/shell/coqueba/cbake3.F
Chd|        CBALKE3                       source/elements/shell/coqueba/cbalke3.F
Chd|        CBALKEC3                      source/elements/shell/coqueba/cbalke3.F
Chd|        CBALKERZ                      source/elements/shell/coqueba/cbalke3.F
Chd|        CBASUMG3                      source/elements/shell/coqueba/cbasumg3.F
Chd|        CMATC3                        source/elements/shell/coqueba/cmatc3.F
Chd|        CMATIP3                       source/elements/shell/coqueba/cmatc3.F
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
       SUBROUTINE CBAKE3 (
     1            JFT    ,JLT    ,NFT    ,NPT    ,MTN    ,
     2            ITHK   ,NCYCLE ,
     3            ISTRAIN,IPLA   ,PM     ,GEO    ,IXC    ,
     4            ELBUF_STR ,BUFMAT ,OFFSET ,INDXOF ,
     5            ETAG  , IDDL   ,NDOF  ,K_DIAG ,K_LT  , IADK  ,JDIK  ,
     6            IHBE   ,THKE   ,ISMSTR ,X      ,IKGEO  ,
     7            IPM    ,IGEO   ,IEXPAN ,IPARG  ,ISUBSTACK ,STACK ,
     8            DRAPE )
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
#include      "scr01_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT     ,JLT    ,NFT    ,NPT   ,MTN     ,ITHK   ,
     .   NCYCLE,ISTRAIN ,IPLA   ,OFFSET,IHBE    ,ISMSTR,IKGEO,IEXPAN
      INTEGER IXC(NIXC,*) ,IGEO(NPROPGI,*),IPM(*),IPARG(*)
      INTEGER INDXOF(MVSIZ),ISUBSTACK,
     .         ETAG(*),IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*)
C     REAL OU REAL*8
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*),BUFMAT(*),   X(3,*),THKE(*)
      my_real
     .   KE11(36,MVSIZ),KE22(36,MVSIZ),KE33(36,MVSIZ),KE44(36,MVSIZ),
     .   KE12(36,MVSIZ),KE13(36,MVSIZ),KE14(36,MVSIZ),KE23(36,MVSIZ),
     .   KE24(36,MVSIZ),KE34(36,MVSIZ),OFF(MVSIZ),K_DIAG(*) ,K_LT(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_) :: DRAPE
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER
     .   NPLAT,NLAY,IPLAT(MVSIZ)
      INTEGER
     .   I, J,J1,J2, IR, IS, NEL,  IUN,  MX,L_DIRA ,L_DIRB ,
     .   EP,NG,NPG,NNOD,IREP,PT1,PT2,PT3,LENF,LENM,NPTR,NPTS,
     .   PT0,PTF,PTM,PTE,PTEP,PTS,PPTF,PPTM,PPTE,PPTEP,PPTS
      INTEGER  MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ)
      INTEGER  MPT,IORTH,IBID,IDRIL
      PARAMETER (NPG = 4)
      PARAMETER (NNOD = 4)
      MY_REAL
     .   VCORE(MVSIZ,3*NNOD),
     .   VQN(MVSIZ,9*NNOD),VQG(MVSIZ,9*NNOD),VNRM(MVSIZ,3*NNOD),
     .   BM(MVSIZ,9*NNOD),BMF(MVSIZ,9*NNOD),BF(MVSIZ,6*NNOD),
     .   BC(MVSIZ,10*NNOD),VQ(MVSIZ,9),VJFI(MVSIZ,6,4),
     .   TC(MVSIZ,4),JAC(MVSIZ,NPG),HX(MVSIZ,NPG),HY(MVSIZ,NPG),
     .   VETA(4,NPG),VKSI(4,NPG),BZZ(MVSIZ,2*NNOD)
      MY_REAL
     .      VASTN(MVSIZ,4*NNOD),AREA(MVSIZ),
     .      CDET(MVSIZ),THK2(MVSIZ)
      INTEGER
     .   NEL8,NEL5,NEL3,NPTM,IGTYP,PTMAT,NBM_S,NBDIR,NB16A
      MY_REAL
     .   SIGY(MVSIZ),VOL0(MVSIZ),THK0(MVSIZ),
     .   X13(MVSIZ) ,Y13(MVSIZ), X24(MVSIZ) ,HZ(MVSIZ),
     .   VOLG(MVSIZ),Y24(MVSIZ),HM(MVSIZ,4),HF(MVSIZ,4),HC(MVSIZ,2),
     .   HMOR(MVSIZ,2),HFOR(MVSIZ,2),HMFOR(MVSIZ,6),GS(MVSIZ)
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
     .    MF34(9,MVSIZ),MF44(9,MVSIZ),
     .    BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4),
     .    BMRZ(MVSIZ,3,4),BRZ(MVSIZ,4,4)
C-----------------------------------------------
      my_real,
     .  DIMENSION(:) ,POINTER  :: DIR_A, DIR_B
      my_real,
     .    ALLOCATABLE, DIMENSION(:), TARGET :: DIRA,DIRB
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----------------------------------------------
C     INITIALISATION
C--------------------------
      GBUF => ELBUF_STR%GBUF
      IUN = 1
      NEL=JLT-JFT+IUN
      IF (MTN==1) NPT=0
      MPT=IABS(NPT)
      IDRIL = IPARG(41)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      NEL3 = NEL*3
      NEL5 = NEL*5
      NEL8 = NEL*8
      NPTM = MAX(1,MPT)
      NLAY = ELBUF_STR%NLAY
C
      IGTYP = IGEO(11,IXC(6,1))
      IREP  = IGEO(6 ,IXC(6,1))
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
       CALL CBACOORK(JFT,JLT,X,IXC,PM,GBUF%OFF,
     1               GEO,AREA,VCORE,JAC,HX,HY,
     2               VQN,VQG,VQ,VJFI,VNRM,VASTN,NPLAT,IPLAT,
     3               X13  ,X24  ,Y13,Y24,
     4               ELBUF_STR,NLAY, GBUF%SMSTR,
     5               IREP,NPT,ISMSTR,DIR_A,DIR_B ,
     6               PID ,MAT,NGL,OFF,IDRIL,NEL)
       CALL CBAINI3(JFT,JLT,VKSI,VETA,
     1                     K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     2                     M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     3                     MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33,
     4                     MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34)
C
      IF (IREP>0) THEN
      CALL CMATC3(JFT    ,JLT     ,PM     ,MAT     ,GEO     ,
     1            PID    ,AREA    ,THK0   ,THK2    ,GBUF%THK,
     2            THKE   ,VOLG    ,MTN    ,NPT     ,ITHK    ,
     3            HM     ,HF      ,HC     ,HZ      ,IGTYP   ,
     4            IORTH  ,HMOR    ,HFOR   ,DIR_A   ,IGEO    ,
     5            IDRIL  ,IHBE    ,HMFOR  ,GS      ,ISUBSTACK,
     6            STACK  ,ELBUF_STR,NLAY  ,DRAPE   ,NFT      ,
     7            NEL    )
      ELSE
      CALL CMATC3(JFT    ,JLT     ,PM     ,MAT     ,GEO     ,
     1            PID    ,AREA    ,THK0   ,THK2    ,GBUF%THK,
     2            THKE   ,VOLG    ,MTN    ,NPT     ,ITHK    ,
     3            HM     ,HF      ,HC     ,HZ      ,IGTYP   ,
     4            IORTH  ,HMOR    ,HFOR   ,DIRA    ,IGEO    ,
     5            IDRIL  ,IHBE    ,HMFOR  ,GS      ,ISUBSTACK,
     6            STACK  ,ELBUF_STR,NLAY  ,DRAPE  , NFT      ,
     7            NEL    )
      ENDIF
      IF (IDRIL>0) THEN
        CALL CBADERIRZ(JFT  ,JLT  ,AREA ,X13 ,X24   ,
     2                 Y13  ,Y24  ,BM0RZ,BMKRZ,BMERZ,
     3                 VCORE,NPLAT,IPLAT,ISMSTR)
      ELSE
        CALL CBABEC3(JFT ,JLT ,X13  ,X24  ,Y13  ,Y24 ,BM, NPLAT, IPLAT)
      END IF
C-----------------------------------------------
C     BOUCLE SUR POINTS D'INTEGRATION DE GAUSS
C-----------------------------------------------
      LENF = NEL*GBUF%G_FORPG/NPG
      LENM = NEL*GBUF%G_MOMPG/NPG
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      DO IS = 1,NPTS
        DO IR = 1,NPTR
          NG = NPTR*(IS-1) + IR
          PTF = (NG-1)*LENF+1
          PTM = (NG-1)*LENM+1
          DO I=JFT,JLT
           CDET(I) = JAC(I,NG)
           VOL0(I) = THK0(I)*CDET(I)
          ENDDO
C-----------------------------------------------
C         MATRICE [B]
C-----------------------------------------------
          CALL CBABE3(JFT,JLT,NG,VCORE,AREA,CDET,VQN,VQG,VJFI,
     1                  VNRM,VASTN,HX,HY,VETA,VKSI,
     2                  BM,BMF,BF,BC,TC,BZZ,NPLAT,IPLAT,
     3                  IDRIL,BRZ )
C-----------------------------------------------
C         IF [KT]
C-----------------------------------------------
          CALL CMATIP3(JFT    ,JLT     ,PM     ,MAT     ,PID    ,
     1               MTN    ,NPT     ,HM     ,HF      ,IORTH  ,
     2               HMOR   ,HFOR    ,HMFOR  ,NG      )
C----------------------------------------------------------------------------
C         SUB-MATRICES [KE] LOCAL
C----------------------------
          CALL CBALKE3(JFT,JLT,CDET,THK0,THK2,HM,HF,HC,HZ,
     1                      BM,BMF,BF,BC,TC,BZZ,NPLAT,IPLAT,VOL0,
     2                      IKGEO,GBUF%FORPG(PTF),GBUF%MOMPG(PTM),
     3                      K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                      M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                      MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33,
     6                      MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     7                      IORTH,HMOR,HFOR,IDRIL,HMFOR,
     8                      X13  ,X24  ,Y13  ,Y24,NEL)
          IF (IDRIL>0) THEN
           CALL CBABER3(JFT ,JLT  ,BM0RZ,BMKRZ,BMERZ  ,
     2                       BMRZ ,BRZ  ,BM   ,NPLAT  ,IPLAT,
     3                       NG   )
           CALL CBALKERZ(JFT ,JLT   ,VOL0 ,THK0 ,
     2                      HM   ,HZ   ,BM   ,
     6                      K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     7                      M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     8                      MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33,
     9                      MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     A                      IORTH,HMOR,HFOR ,IPLAT,NPLAT,
     B                      BMRZ,BRZ  ,GBUF%HOURG,IKGEO,NG ,HMFOR,BF  ,
     C                      BMF ,NEL)
          END IF
        ENDDO
       ENDDO
C---------FIN DE BOUCLE DE 4 POINTS DE GAUSS------------
C-------------membrane shear traitement--------------------
        IF (IDRIL==0) THEN
         CALL CBALKEC3(JFT,JLT,VOLG  ,X13  ,X24  ,Y13  ,Y24, HM,
     1                    K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     2                    NPLAT,IPLAT,IKGEO,GBUF%FOR,M11,M22,M33,M44,
     3                    IORTH,NEL)
        END IF
C----------------------------
C     TRANSFORME [KE] LOCALE AU GLOBALE et ASSEMBLAGE----
C----------------------------
         CALL CBASUMG3(
     1            JFT    ,JLT    ,VQN    ,VQ     ,NPLAT   ,IPLAT   ,
     2                     K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     3                     M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     4                     MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33,
     5                     MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     6                     KE11,KE22,KE33,KE44,KE12,KE13,KE14,KE23,
     7                     KE24,KE34,VCORE,IDRIL,IORTH)

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
C   |============================================================
C   |   CBAINI3                             /coqueba/cbake3.F
C   |------------------------------------------------------------
C   |-- appele -----------
C   |-- appelee par -----------
C   |        CBAKE3                         /coqueba/cbake3.F
C   |============================================================
Chd|====================================================================
Chd|  CBAINI3                       source/elements/shell/coqueba/cbake3.F
Chd|-- called by -----------
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBAINI3(JFT,JLT,VKSI,VETA,
     1                     K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     2                     M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     3                     MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33,
     4                     MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34)
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
c-----------------------------------------------
c   g l o b a l   p a r a m e t e r s
c-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      MY_REAL
     .   VETA(4,4),VKSI(4,4)
      my_real
     .    K11(9,*),K12(9,*),K13(9,*),K14(9,*),
     .    K22(9,*),K23(9,*),K24(9,*),K33(9,*),
     .    M11(9,*),M12(9,*),M13(9,*),M14(9,*),
     .    M22(9,*),M23(9,*),M24(9,*),M33(9,*),
     .    MF11(9,*),MF12(9,*),MF13(9,*),MF14(9,*),
     .    MF22(9,*),MF23(9,*),MF24(9,*),MF33(9,*),
     .    FM12(9,*),FM13(9,*),FM14(9,*),
     .    FM23(9,*),FM24(9,*),FM34(9,*),
     .    K34(9,*),K44(9,*),M34(9,*),M44(9,*),
     .    MF34(9,*),MF44(9,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   PG
      DATA
     .    PG/.577350269189626/
C-----------------------------------------------
      DO J=1,9
       DO I=JFT,JLT
          K11(J,I)=ZERO
          K12(J,I)=ZERO
          K13(J,I)=ZERO
          K14(J,I)=ZERO
          K22(J,I)=ZERO
          K23(J,I)=ZERO
          K24(J,I)=ZERO
          K33(J,I)=ZERO
          K34(J,I)=ZERO
          K44(J,I)=ZERO
          M11(J,I)=ZERO
          M12(J,I)=ZERO
          M13(J,I)=ZERO
          M14(J,I)=ZERO
          M22(J,I)=ZERO
          M23(J,I)=ZERO
          M24(J,I)=ZERO
          M33(J,I)=ZERO
          M34(J,I)=ZERO
          M44(J,I)=ZERO
       ENDDO
      ENDDO
      DO J=1,9
       DO I=JFT,JLT
          MF11(J,I)=ZERO
          MF12(J,I)=ZERO
          MF13(J,I)=ZERO
          MF14(J,I)=ZERO
          MF22(J,I)=ZERO
          MF23(J,I)=ZERO
          MF24(J,I)=ZERO
          MF33(J,I)=ZERO
          MF34(J,I)=ZERO
          MF44(J,I)=ZERO
          FM12(J,I)=ZERO
          FM13(J,I)=ZERO
          FM14(J,I)=ZERO
          FM23(J,I)=ZERO
          FM24(J,I)=ZERO
          FM34(J,I)=ZERO
       ENDDO
      ENDDO
C
       VKSI(1,1)=-FOURTH*(ONE+PG)
       VKSI(2,1)=-VKSI(1,1)
       VKSI(3,1)= FOURTH*(ONE-PG)
       VKSI(4,1)=-VKSI(3,1)
       VETA(1,1)=-FOURTH*(ONE+PG)
       VETA(2,1)=-FOURTH*(ONE-PG)
       VETA(3,1)=-VETA(2,1)
       VETA(4,1)=-VETA(1,1)
       VKSI(1,2)= VKSI(1,1)
       VKSI(2,2)=-VKSI(1,2)
       VKSI(3,2)= VKSI(3,1)
       VKSI(4,2)=-VKSI(3,2)
       VETA(1,2)= VETA(2,1)
       VETA(2,2)= VETA(1,1)
       VETA(3,2)=-VETA(2,2)
       VETA(4,2)=-VETA(1,2)
       VKSI(1,3)=-VKSI(3,1)
       VKSI(2,3)=-VKSI(1,3)
       VKSI(3,3)=-VKSI(1,1)
       VKSI(4,3)=-VKSI(3,3)
       VETA(1,3)= VETA(1,2)
       VETA(2,3)= VETA(2,2)
       VETA(3,3)=-VETA(2,3)
       VETA(4,3)=-VETA(1,3)
       VKSI(1,4)= VKSI(1,3)
       VKSI(2,4)=-VKSI(1,4)
       VKSI(3,4)= VKSI(3,3)
       VKSI(4,4)=-VKSI(3,4)
       VETA(1,4)= VETA(1,1)
       VETA(2,4)= VETA(2,1)
       VETA(3,4)=-VETA(2,4)
       VETA(4,4)=-VETA(1,4)
C
         RETURN
         END
