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
Chd|  C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_C3                      source/implicit/assem_c3.F    
Chd|        C3BE3                         source/elements/sh3n/coque3n/c3be3.F
Chd|        C3COORK3                      source/elements/sh3n/coque3n/c3coork3.F
Chd|        C3DERIRZ                      source/elements/sh3n/coque3n/c3defo3.F
Chd|        C3EOFF                        source/elements/sh3n/coque3n/c3eoff.F
Chd|        C3LKE3                        source/elements/sh3n/coque3n/c3lke3.F
Chd|        C3LKERZ3                      source/elements/sh3n/coque3n/c3lke3.F
Chd|        C3SUMG3                       source/elements/sh3n/coque3n/c3sumg3.F
Chd|        CMATC3                        source/elements/shell/coqueba/cmatc3.F
Chd|        CMATIP3                       source/elements/shell/coqueba/cmatc3.F
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE C3KE3(
     1            JFT    ,JLT    ,NFT    ,NPT    ,MTN    ,
     2            ITHK   ,NCYCLE ,
     3            ISTRAIN,IPLA   ,PM     ,GEO    ,IXTG   ,
     4            ELBUF_STR  ,BUFMAT ,OFFSET ,INDXOF ,
     5            ETAG  , IDDL  ,NDOF  ,K_DIAG ,K_LT  , IADK  ,JDIK  ,     
     6            IHBE   ,THKE   ,ISMSTR ,X      ,
     7            IKGEO  ,IPM    ,IGEO   ,IEXPAN ,
     8            IPARG  ,ISUBSTACK,STACK,DRAPE_SH3N, INDX_DRAPE ,
     9            SEDRAPE,NUMEL_DRAPE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD  
      USE STACK_MOD  
      USE DRAPE_MOD        
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
C
      INTEGER JFT     ,JLT    ,NFT    ,NPT   ,MTN     ,ITHK   ,
     .        NCYCLE,ISUBSTACK,
     .        ISTRAIN ,IPLA   ,OFFSET,IHBE    ,ISMSTR,IKGEO, IEXPAN
      INTEGER ,    INTENT(IN)     ::        SEDRAPE,NUMEL_DRAPE
      INTEGER, DIMENSION(SEDRAPE) :: INDX_DRAPE
      INTEGER IXTG(NIXTG,*) ,IGEO(NPROPGI,*),IPM(*),IPARG(*)
      INTEGER INDXOF(MVSIZ),
     .         ETAG(*),IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*)
C     REAL OU REAL*8
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*),BUFMAT(*),   X(3,*),THKE(*)
      my_real    
     .   KE11(36,MVSIZ),KE22(36,MVSIZ),KE33(36,MVSIZ),
     .   KE12(36,MVSIZ),KE13(36,MVSIZ),KE23(36,MVSIZ),OFF(MVSIZ),
     .   K_DIAG(*) ,K_LT(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_) :: DRAPE_SH3N(NUMELTG_DRAPE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c  indx utilise localement contrairement aux coques 4n 
      INTEGER MAT(MVSIZ),PID(MVSIZ),NGL(MVSIZ),INDX(MVSIZ)
      INTEGER I,J,J1,J2,IREP,IUN,NEL,IORTH,IGTYP,L_DIRA,L_DIRB,NLAY,
     .   IDRIL,EP
      my_real 
     .   AREA(MVSIZ),PX1(MVSIZ), PY1(MVSIZ), PY2(MVSIZ),
     .   SIGY(MVSIZ),THK0(MVSIZ),
     .   X2(MVSIZ) ,Y2(MVSIZ), X3(MVSIZ),Y3(MVSIZ)
      my_real 
     .   G(MVSIZ) , VOL0(MVSIZ),THK02(MVSIZ),ZCFAC(MVSIZ,2), 
     .   HM(MVSIZ,4),HF(MVSIZ,4),HC(MVSIZ,2),HZ(MVSIZ),
     .   HMOR(MVSIZ,2),HFOR(MVSIZ,2),HMFOR(MVSIZ,6),GS(MVSIZ)
      MY_REAL 
     .    R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .    R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .    R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .    K11(9,MVSIZ),K12(9,MVSIZ),K13(9,MVSIZ),
     .    K22(9,MVSIZ),K23(9,MVSIZ),K33(9,MVSIZ),
     .    M11(9,MVSIZ),M12(9,MVSIZ),M13(9,MVSIZ),
     .    M22(9,MVSIZ),M23(9,MVSIZ),M33(9,MVSIZ),
     .    MF11(9,MVSIZ),MF12(9,MVSIZ),MF13(9,MVSIZ),
     .    MF22(9,MVSIZ),MF23(9,MVSIZ),MF33(9,MVSIZ),
     .    FM12(9,MVSIZ),FM13(9,MVSIZ),FM23(9,MVSIZ),
     .    BM0RZ(MVSIZ,3,2),B0RZ(MVSIZ,3),BKRZ(MVSIZ,2),BERZ(MVSIZ,2)
C-----------------------------------------------
      my_real,
     .  DIMENSION(:) ,POINTER  :: DIR_A, DIR_B
      my_real, 
     .    ALLOCATABLE, DIMENSION(:), TARGET :: DIRA,DIRB
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C-----------------------------------------------
      IUN = 1
      NEL=JLT-JFT+IUN
      GBUF => ELBUF_STR%GBUF
      IDRIL = IPARG(41)

      IGTYP = IGEO(11,IXTG(5,1))
      IREP  = IGEO(6 ,IXTG(5,1))
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
      CALL C3COORK3(JFT   ,JLT    ,X     ,IXTG   ,GBUF%OFF,
     1               GEO  ,PID    ,MAT    ,NGL   ,AREA   ,  
     2               IREP ,NPT    ,ISMSTR ,NLAY  ,
     2               ELBUF_STR    ,GBUF%SMSTR, DIR_A,DIR_B  ,
     3               X2   ,X3  ,Y3 ,
     4               R11,R12,R13,R21,R22,R23,R31,R32,R33,
     5               K11,K12,K13,K22,K23,K33,
     6               M11,M12,M13,M22,M23,M33, 
     7               MF11,MF12,MF13,MF22,MF23,MF33, 
     8               FM12,FM13,FM23,OFF ,NEL) 
      IF (IREP>0) THEN
       CALL CMATC3(JFT    ,JLT       ,PM     ,MAT     ,GEO      ,
     1             PID    ,AREA      ,THK0   ,THK02   ,GBUF%THK ,
     2             THKE   ,VOL0      ,MTN    ,NPT     ,ITHK     ,
     3             HM     ,HF        ,HC     ,HZ      ,IGTYP    ,
     4             IORTH  ,HMOR      ,HFOR   ,DIR_A   ,IGEO     ,
     5             IDRIL  ,IHBE      ,HMFOR  ,GS      ,ISUBSTACK,
     6             STACK  ,ELBUF_STR ,NLAY   ,DRAPE_SH3N , NFT      ,
     .              NEL   ,INDX_DRAPE,SEDRAPE,NUMEL_DRAPE)
      ELSE
       CALL CMATC3(JFT    ,JLT       ,PM     ,MAT     ,GEO      ,
     1             PID    ,AREA      ,THK0   ,THK02   ,GBUF%THK ,
     2             THKE   ,VOL0      ,MTN    ,NPT     ,ITHK     ,
     3             HM     ,HF        ,HC     ,HZ      ,IGTYP    ,
     4             IORTH  ,HMOR      ,HFOR   ,DIRA    ,IGEO     ,
     5             IDRIL  ,IHBE      ,HMFOR  ,GS      ,ISUBSTACK,
     6             STACK  ,ELBUF_STR ,NLAY   ,DRAPE_SH3N   ,NFT      ,
     .             NEL    ,INDX_DRAPE,SEDRAPE,NUMEL_DRAPE)
      ENDIF
C-----------------------------------------------
C     IF [KT]
C-----------------------------------------------
        CALL CMATIP3(JFT    ,JLT     ,PM     ,MAT     ,PID    ,
     1               MTN    ,NPT     ,HM     ,HF      ,IORTH  ,
     2               HMOR   ,HFOR    ,HMFOR  ,IUN      )
      IF (IORTH >0 ) THEN
        DO I=1,9 
        DO EP=JFT,JLT 
         MF11(I,EP) =ZERO
         MF22(I,EP) =ZERO
         MF33(I,EP) =ZERO
         MF12(I,EP) =ZERO
         MF13(I,EP) =ZERO
         MF23(I,EP) =ZERO
         FM12(I,EP) =ZERO
         FM13(I,EP) =ZERO
         FM23(I,EP) =ZERO
        ENDDO
        ENDDO
      ENDIF
      CALL C3BE3(JFT,JLT,PX1,PY1,PY2 ,X2 ,X3 , Y3 , AREA)
C----------------------------------------------------------------------------
C     SUB-MATRICES [KE] LOCAL
C----------------------------
      CALL C3LKE3(JFT,JLT,AREA,THK0,THK02,HM,HF,HC,HZ,
     1                      PX1,PY1,PY2,VOL0,
     2                      K11,K12,K13,K22,K23,K33,
     3                      M11,M12,M13,M22,M23,M33, 
     4                      MF11,MF12,MF13,MF22,MF23,MF33, 
     5                      FM12,FM13,FM23,IKGEO,GBUF%FOR,GBUF%MOM, 
     6                      IORTH,HMOR,HFOR,HMFOR,IDRIL,
     7                      NEL) 
      IF (IDRIL>0) THEN
       DO I=JFT,JLT
        Y2(I)=ZERO
       END DO
C
       CALL C3DERIRZ(JFT ,JLT  ,AREA ,X2  ,X3    ,
     2               Y2  ,Y3   ,BM0RZ,B0RZ,BKRZ,BERZ )
       CALL C3LKERZ3(JFT,JLT,HM,HZ,
     1                   PX1,PY1,PY2,VOL0,AREA,
     2                   K11,K12,K13,K22,K23,K33,
     3                   M11,M12,M13,M22,M23,M33,
     4                   MF11,MF12,MF13,MF22,MF23,MF33,
     5                   FM12,FM13,FM23,IORTH,HMOR,
     6                   BM0RZ,B0RZ,BKRZ,BERZ,THK0,HMFOR)
      END IF
C----------------------------
C     TRANSFORME [KE] LOCALE AU GLOBALE et ASSEMBLAGE----
C----------------------------
        CALL C3SUMG3(JFT ,JLT ,
     1                     R11,R12,R13,R21,R22,R23,R31,R32,R33,
     2                     K11,K12,K13,K22,K23,K33,
     3                     M11,M12,M13,M22,M23,M33,
     4                     MF11,MF12,MF13,MF22,MF23,MF33, 
     5                     FM12,FM13,FM23, 
     6                     KE11,KE22,KE33,KE12,KE13,KE23,IDRIL,
     7                     IORTH)
     
     
              IF (NEIG>0) CALL C3EOFF(
     1                   JFT, JLT, IXTG, ETAG, OFF)
              CALL ASSEM_C3(
     1         IXTG      ,NEL      ,IDDL      ,NDOF      ,K_DIAG    ,
     2         K_LT      ,IADK      ,JDIK      ,KE11      ,KE12      ,
     3         KE13      ,KE22      ,KE23      ,KE33      ,OFF       )
C-------------------------
      RETURN
      END
