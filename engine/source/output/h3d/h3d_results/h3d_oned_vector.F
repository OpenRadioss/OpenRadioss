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
Chd|  H3D_ONED_VECTOR               source/output/h3d/h3d_results/h3d_oned_vector.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_VECTOR              source/output/h3d/h3d_results/h3d_write_vector.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE H3D_ONED_VECTOR(
     .                  ELBUF_TAB      , IFUNC   , IPARG    , GEO   , IXT     ,
     .                  IXP            , IXR     , PM    , ANIM    ,
     .                  ONED_VECTOR    , ID_ELEM , ITY_ELEM , INFO1 , INFO2   , 
     .                  IS_WRITTEN_ONED, IPARTT  , IPARTP   , IPARTR, H3D_PART,
     .                  KEYWORD        , X       , D        , TORS  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "chara_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr23_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*),
     .   ANIM(*),ONED_VECTOR(3,*),X(3,*),D(3,*),TORS(15,*)
      INTEGER IPARG(NPARG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IFUNC,
     .        NANIM1D_L,
     .        IS_WRITTEN_ONED(*),ID_ELEM(*),ITY_ELEM(*),
     .        IPARTT(*) ,IPARTP(*),IPARTR(*),H3D_PART(*)
      INTEGER BUF,INFO1,INFO2
C
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      CHARACTER*ncharline KEYWORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(MVSIZ), MASS(MVSIZ) ,
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE(3),
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3, XM,     
     .   FOR, AREA, FEQ, EPLAS, RHO0, A0, XX1, YY1, ZZ1, AL0
      INTEGER I, II, NG, NEL, NFT, IAD, ITY, LFT, NPT, ISS, ISC,
     .        IADD, N, J, LLT, MLW, NB1, NB2, NB3, NB4, NB5, 
     .        NB6, NB7, NB8, NB9, NB10, NB11, NB12, NB13, NB14, NB15,
     .        NB16, LLL,NUVAR,IGTYP,
     .        ISTRAIN,NN, K1, K2,JTURB,MT,JALE, IMID, IALEL,IPID,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NF,
     .        OFFSET,K,INC,KK,IHBE,ISROT,ILAYER,IR,IS,JJ(6),IOK_PART(MVSIZ),
     .        IS_WRITTEN_VALUE(MVSIZ),N1, N2
      REAL R4
C
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_),POINTER :: LBUF
C-----------------------------------------------
C
      NN1 = 1
      NN3 = 1
      NN4 = NN3 
      NN5 = NN4 
      NN6 = NN5 
      NN7 = NN6 + NUMELT
      NN8 = NN7 + NUMELP
      NN9 = NN8 + NUMELR
      NN10= NN9 
C
      DO NG=1,NGROUP
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        ITY   =IPARG(5,NG)
        IGTYP =IPARG(38,NG)
C---
        GBUF => ELBUF_TAB(NG)%GBUF
C---
        NFT   =IPARG(3,NG)
!
        DO I=1,6
          JJ(I) = NEL*(I-1)
        ENDDO

        VALUE(1:3) = ZERO
        DO I=1,NEL
          IS_WRITTEN_VALUE(I) = 0
        ENDDO	 
c
        IF (ITY == 4) OFFSET = 0
        IF (ITY == 5) OFFSET = NUMELT
        IF (ITY == 6) OFFSET = NUMELT+NUMELP
c
        DO  I=1,NEL 
          IF (ITY == 4) THEN
            ID_ELEM(OFFSET+NFT+I) = IXT(NIXT,NFT+I)
            ITY_ELEM(OFFSET+NFT+I) = 4
            IF( H3D_PART(IPARTT(NFT+I)) == 1) IOK_PART(I) = 1
          ELSEIF (ITY == 5) THEN 
            ID_ELEM(OFFSET+NFT+I) = IXP(NIXP,NFT+I)
            ITY_ELEM(OFFSET+NFT+I) = 5
            IF( H3D_PART(IPARTP(NFT+I)) == 1) IOK_PART(I) = 1
          ELSEIF (ITY == 6) THEN 
            ID_ELEM(OFFSET+NFT+I) = IXR(NIXR,NFT+I)
            ITY_ELEM(OFFSET+NFT+I) = 6
            IF( H3D_PART(IPARTR(NFT+I)) == 1) IOK_PART(I) = 1
          ENDIF
        ENDDO 

        IF(ITY==4 .OR. ITY==5 .OR. ITY==6)THEN
          DO I=1,NEL
            ONED_VECTOR(1:3,OFFSET+NFT+I) = ZERO   ! Default = zero in all cases !
          ENDDO
        ENDIF
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        IF(ITY==4)THEN
C--------------------------------------------------
          IF (KEYWORD == 'FINT') THEN
C--------------------------------------------------
            DO I=1,NEL
              VALUE(1) = TORS(1,OFFSET+NFT+I)
              VALUE(2) = TORS(2,OFFSET+NFT+I)
              VALUE(3) = TORS(3,OFFSET+NFT+I)
              CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_ONED,ONED_VECTOR,I,OFFSET,NFT,
     .                                    VALUE)
            ENDDO
C--------------------------------------------------
c          ELSEIF (KEYWORD == '') THEN   
C--------------------------------------------------                    
c            DO I=1,NEL
c              VALUE(1) =
c              VALUE(2) =
c              VALUE(3) =
c              CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_ONED,NODAL_VECTOR,I,0,0,
c     .                                    VALUE)
c            ENDDO  
          ENDIF
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
        ELSEIF(ITY==5)THEN
C--------------------------------------------------
          IF (KEYWORD == 'FINT') THEN
C--------------------------------------------------
            DO I=1,NEL
              VALUE(1) = TORS(1,OFFSET+NFT+I)
              VALUE(2) = TORS(2,OFFSET+NFT+I)
              VALUE(3) = TORS(3,OFFSET+NFT+I)
              CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_ONED,ONED_VECTOR,I,OFFSET,NFT,
     .                                    VALUE)
            ENDDO
C--------------------------------------------------
c          ELSEIF (KEYWORD == '') THEN   
C--------------------------------------------------                    
c            DO I=1,NEL
c              VALUE(1) =
c              VALUE(2) =
c              VALUE(3) =
c              CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_ONED,NODAL_VECTOR,I,0,0,
c     .                                    VALUE)
c            ENDDO  
          ENDIF
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
        ELSEIF(ITY==6)THEN
C--------------------------------------------------
          IF (KEYWORD == 'FINT') THEN
C--------------------------------------------------
            DO I=1,NEL
              VALUE(1) = TORS(1,OFFSET+NFT+I)
              VALUE(2) = TORS(2,OFFSET+NFT+I)
              VALUE(3) = TORS(3,OFFSET+NFT+I)
              CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_ONED,ONED_VECTOR,I,OFFSET,NFT,
     .                                    VALUE)
            ENDDO
C--------------------------------------------------
c          ELSEIF (KEYWORD == '') THEN   
C--------------------------------------------------                    
c            DO I=1,NEL
c              VALUE(1) =
c              VALUE(2) =
c              VALUE(3) =
c              CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_ONED,NODAL_VECTOR,I,0,0,
c     .                                    VALUE)
c            ENDDO  
          ENDIF
        ENDIF
      ENDDO
 
      RETURN
      END
