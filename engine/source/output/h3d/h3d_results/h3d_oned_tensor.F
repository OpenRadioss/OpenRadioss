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
Chd|  H3D_ONED_TENSOR               source/output/h3d/h3d_results/h3d_oned_tensor.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_TENSOR              source/output/h3d/h3d_results/h3d_write_tensor.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE H3D_ONED_TENSOR(
     .                  ELBUF_TAB      , IFUNC   , IPARG    , GEO   , IXT     ,
     .                  IXP            , IXR     , PM    , ANIM    ,
     .                  ONED_TENSOR    , ID_ELEM , ITY_ELEM , INFO1 , INFO2   , 
     .                  IS_WRITTEN_ONED, IPARTT  , IPARTP   , IPARTR, H3D_PART,
     .                  KEYWORD        , X       , D        , IPT_INPUT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*),
     .   ANIM(*),ONED_TENSOR(6,*),X(3,*),D(3,*)
      INTEGER IPARG(NPARG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IFUNC,
     .        NANIM1D_L,
     .        IS_WRITTEN_ONED(*),ID_ELEM(*),ITY_ELEM(*),
     .        IPARTT(*) ,IPARTP(*),IPARTR(*),H3D_PART(*)
      INTEGER BUF,INFO1,INFO2,IPT_INPUT
C
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      CHARACTER*ncharline KEYWORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(6,MVSIZ),
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
     .        IS_WRITTEN_TENSOR(MVSIZ),N1, N2, IPT
      REAL R4
C
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
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
        NPT  = IPARG(6,NG)
!
        DO I=1,3
          JJ(I) = NEL*(I-1)
        ENDDO

        EVAR(1:6,1:NEL) = ZERO
        IS_WRITTEN_TENSOR(1:NEL) = 0
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

        IPT = IPT_INPUT

        IF(ITY==4 .OR. ITY==5 .OR. ITY==6)THEN
          DO I=1,NEL
            ONED_TENSOR(1:6,OFFSET+NFT+I) = ZERO   ! Default = zero in all cases !
          ENDDO
        ENDIF
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        IF(ITY==4)THEN
C--------------------------------------------------
          IF (KEYWORD == 'TEST') THEN
C--------------------------------------------------
            DO I=1,NEL
              EVAR(1,I) = ZERO
              EVAR(2,I) = ZERO
              EVAR(3,I) = ZERO
              EVAR(4,I) = ZERO
              EVAR(5,I) = ZERO
              EVAR(6,I) = ZERO
              IS_WRITTEN_TENSOR(I) = 1
            ENDDO
C--------------------------------------------------
c          ELSEIF (KEYWORD == '') THEN   
C--------------------------------------------------                    
c            DO I=1,NEL
c              VALUE(1) =
c              VALUE(2) =
c              VALUE(3) =
c            ENDDO  
C--------------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRESS') THEN
C--------------------------------------------------
            DO I=1,NEL
              FOR  = GBUF%FOR(I)
              AREA = GBUF%AREA(I)
              EVAR(1,I) = FOR/AREA
              IS_WRITTEN_TENSOR(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRAIN') THEN
C--------------------------------------------------
            DO I=1,NEL
              EVAR(1,I) = GBUF%STRA(I)
              IS_WRITTEN_TENSOR(I) = 1
            ENDDO            
          ENDIF
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
        ELSEIF(ITY==5)THEN
C--------------------------------------------------
          IF (KEYWORD == 'TENS/STRESS') THEN
C--------------------------------------------------
c NPT=NULL
            IF(IPT < 0) THEN
              DO I=1,NEL
                N = I + NFT
                EVAR(1,I) = GBUF%FOR(JJ(1)+I) / GEO(1,IXP(5,N))
                EVAR(4,I) = GBUF%FOR(JJ(2)+I) / GEO(1,IXP(5,N))
                EVAR(6,I) = GBUF%FOR(JJ(3)+I) / GEO(1,IXP(5,N))
                IS_WRITTEN_TENSOR(I) = 1
              ENDDO
c NPT=IPT
            ELSEIF(IPT > 0 .AND. IPT <= NPT) THEN
              ILAYER = 1
              BUFLY => ELBUF_TAB(NG)%BUFLY(ILAYER)
             IF (BUFLY%L_SIG > 0) THEN
              LBUF  => BUFLY%LBUF(1,1,IPT)
              DO I=1,NEL
                EVAR(1,I) = LBUF%SIG(JJ(1)+I)
                EVAR(4,I) = LBUF%SIG(JJ(2)+I)
                EVAR(6,I) = LBUF%SIG(JJ(3)+I)
                IS_WRITTEN_TENSOR(I) = 1
              ENDDO
             END IF !(BUFLY%L_SIG > 0) THEN
            ENDIF
C--------------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRAIN') THEN
C--------------------------------------------------
c NPT=NULL
            IF(IPT < 0 .AND. NPT > 0) THEN
              ILAYER = 1
              BUFLY => ELBUF_TAB(NG)%BUFLY(ILAYER)
              IF (BUFLY%L_STRA > 0) THEN
               DO IPT = 1,NPT
                LBUF  => BUFLY%LBUF(1,1,IPT)
                DO I=1,NEL
                  EVAR(1,I) =EVAR(1,I)+ LBUF%STRA(JJ(1)+I)/NPT
                  EVAR(4,I) =EVAR(4,I)+ LBUF%STRA(JJ(2)+I)/NPT
                  EVAR(6,I) =EVAR(6,I)+ LBUF%STRA(JJ(3)+I)/NPT
                  IS_WRITTEN_TENSOR(I) = 1
                ENDDO
               ENDDO 
              END IF !(BUFLY%L_STRA > 0) THEN
c NPT=IPT
            ELSEIF(IPT > 0 .AND. IPT <= NPT) THEN
              ILAYER = 1
              BUFLY => ELBUF_TAB(NG)%BUFLY(ILAYER)
              LBUF  => BUFLY%LBUF(1,1,IPT)
             IF (BUFLY%L_STRA > 0) THEN
              DO I=1,NEL
                EVAR(1,I) = LBUF%STRA(JJ(1)+I)
                EVAR(4,I) = LBUF%STRA(JJ(2)+I)
                EVAR(6,I) = LBUF%STRA(JJ(3)+I)
                IS_WRITTEN_TENSOR(I) = 1
              ENDDO
             END IF !(BUFLY%L_STRA > 0) THEN
            ENDIF !IPT
C--------------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRAIN/MAX') THEN
C--------------------------------------------------
              DO IPT = 1,NPT
                ILAYER = 1
                BUFLY => ELBUF_TAB(NG)%BUFLY(ILAYER)
                LBUF  => BUFLY%LBUF(1,1,IPT)
               IF (BUFLY%L_STRA > 0) THEN
                DO I=1,NEL
                  EVAR(1,I) =MAX(EVAR(1,I), ABS(LBUF%STRA(JJ(1)+I)))
                  EVAR(4,I) =MAX(EVAR(4,I), ABS(LBUF%STRA(JJ(2)+I)))
                  EVAR(6,I) =MAX(EVAR(6,I), ABS(LBUF%STRA(JJ(3)+I)))
                  IS_WRITTEN_TENSOR(I) = 1
                ENDDO
               END IF !(BUFLY%L_STRA > 0) THEN
              ENDDO 
C--------------------------------------------------
          ELSEIF (KEYWORD == 'TENS/STRAIN/TMAX') THEN
C--------------------------------------------------
              DO I=1,NEL
                EVAR(1,I) =GBUF%MAXEPS(JJ(1)+I)
                EVAR(4,I) =GBUF%MAXEPS(JJ(2)+I)
                EVAR(6,I) =GBUF%MAXEPS(JJ(3)+I)
                IS_WRITTEN_TENSOR(I) = 1
              ENDDO
C--------------------------------------------------
C--------------------------------------------------
c          ELSEIF (KEYWORD == '') THEN   
C--------------------------------------------------                    
c            DO I=1,NEL
c              VALUE(1) =
c              VALUE(2) =
c              VALUE(3) =
c            ENDDO  
          ENDIF
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
        ELSEIF(ITY==6)THEN
C--------------------------------------------------
          IF (KEYWORD == 'TEST') THEN
C--------------------------------------------------
            DO I=1,NEL
              EVAR(1,I) = ZERO
              EVAR(2,I) = ZERO
              EVAR(3,I) = ZERO
              EVAR(4,I) = ZERO
              EVAR(5,I) = ZERO
              EVAR(6,I) = ZERO
              IS_WRITTEN_TENSOR(I) = 1
            ENDDO
C--------------------------------------------------
c          ELSEIF (KEYWORD == '') THEN   
C--------------------------------------------------                    
c            DO I=1,NEL
c              VALUE(1) =
c              VALUE(2) =
c              VALUE(3) =
c            ENDDO  
          ENDIF
        ENDIF
C-----------------------------------------------
        IF(ITY==4 .OR. ITY==5 .OR. ITY==6)THEN
          CALL H3D_WRITE_TENSOR(IOK_PART,IS_WRITTEN_ONED,ONED_TENSOR,NEL,OFFSET,NFT,
     .                                    EVAR,IS_WRITTEN_TENSOR)
        ENDIF
C
      ENDDO
 
      RETURN
      END
