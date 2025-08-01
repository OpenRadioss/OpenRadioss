Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    s4lagsfem         ../engine/source/elements/solid/solide4_sfem/s4lagsfem.F
!||--- called by ------------------------------------------------------
!||    resol             ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    foat_to_6_float   ../engine/source/system/parit.F
!||    initbuf           ../engine/share/resol/initbuf.F
!||    s10volnod3        ../engine/source/elements/solid/solide4_sfem/s10volnod3.F
!||    s10volnodt3       ../engine/source/elements/solid/solide4_sfem/s10volnodt3.F
!||    s4volnod3         ../engine/source/elements/solid/solide4_sfem/s4volnod3.F
!||    s4volnod_sm       ../engine/source/elements/solid/solide4_sfem/s4volnod_sm.F
!||    spmd_exch_vol     ../engine/source/mpi/nodes/spmd_exch_vol.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod      ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    initbuf_mod       ../engine/share/resol/initbuf.F
!||====================================================================
      SUBROUTINE S4LAGSFEM(IPARG,IXS,X,V,ELBUF_TAB,SFEM_NODVAR,S_SFEM_NODVAR,
     .                     IAD_ELEM,FR_ELEM,IXS10,XDP,SXDP,
     .                     NUMNOD, SFR_ELEM, NSPMD, NUMELS, NUMELS8, NUMELS10, NPARG, NGROUP, IRESP)
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: S_SFEM_NODVAR, SFR_ELEM, SXDP !< size for array declaration
      INTEGER,INTENT(IN) :: NUMELS, NUMELS8, NUMELS10 !< number of elements
      INTEGER,INTENT(IN) :: NPARG, NGROUP !< sizes for data structure IPARG
      INTEGER,INTENT(IN) :: NUMNOD !< number of nodes (/NODE)
      INTEGER,INTENT(IN) :: IRESP
      INTEGER,INTENT(IN) :: NSPMD !< number of SPMD domain
      INTEGER IXS(NIXS,NUMELS),IPARG(NPARG,NGROUP),IAD_ELEM(2,NSPMD+1),FR_ELEM(SFR_ELEM),IXS10(6,NUMELS10)
      my_real, INTENT(IN) :: X(3*NUMNOD),V(3*NUMNOD)
      my_real,intent(inout) :: SFEM_NODVAR(S_SFEM_NODVAR)
      DOUBLE PRECISION , DIMENSION(SXDP), INTENT(IN) :: XDP     
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, I, J, I1, I2, I3, I4, K, LENR,NEL,NNOD,ICPRE,IBID,IP
      INTEGER NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ),NC(MVSIZ,10)
      my_real VOL0(MVSIZ),VOLG(MVSIZ)

      DOUBLE PRECISION VOL06(6,MVSIZ), VARNOD6(6,2*NUMNOD)

      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      SFEM_NODVAR(1:2*NUMNOD) = ZERO
      VARNOD6(1:6,1:2*NUMNOD) = ZERO
      VOL06(1:6,1:MVSIZ) = ZERO
      VOL0(1:MVSIZ)      = ZERO
C---------------------------------------------------
C     COMPUTE NODAL VOLUME FOR ALL TETRAHEDRON
C---------------------------------------------------
C SMP loop
C
      DO NG = 1,NGROUP
        IF(IPARG(8, NG) == 1) CYCLE
        NNOD = IPARG(28,NG)
        ICPRE  = IPARG(10,NG)
        IF(NNOD/=4 .AND. NNOD /= 10) CYCLE
        CALL INITBUF(IPARG    ,NG      ,
     2        MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )

        IF(JEUL  == 1) CYCLE
        IF(JALE == 1) CYCLE
        IF(NNOD == 4 .AND. ISROT /= 3) CYCLE
        IF(ICPRE == 0 .AND. (NNOD == 10 .OR. (NNOD == 4 .AND. ISROT == 1))) CYCLE
        LFT=1
        NEL = LLT

        DO I=LFT,LLT
         J=I+NFT
         NC1(I)=IXS(2,J)
         NC2(I)=IXS(4,J)
         NC3(I)=IXS(7,J)
         NC4(I)=IXS(6,J)
        ENDDO

        GBUF => ELBUF_TAB(NG)%GBUF
        IF(NNOD==4 .AND. ISROT == 3) THEN
          CALL S4VOLNOD3(
     1                   VARNOD6,  X,       NC1,       NC2,
     2                   NC3,      NC4,     GBUF%OFF,  XDP,
     3                   NEL,      ISMSTR)
          IF(IRESP==1)THEN
           LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
           DO I=LFT,LLT
              IF(GBUF%OFF(I) == ZERO) THEN
                VOL0(I)=ZERO
              ELSE
                VOL0(I)=LBUF%VOL0DP(I)
              ENDIF
           ENDDO
          ELSE
           DO I=LFT,LLT
              IF(GBUF%OFF(I) == ZERO) THEN
                VOL0(I)=ZERO
              ELSE
                VOL0(I)=GBUF%VOL(I)
              ENDIF
           ENDDO
          END IF !(IRESP==1)THEN
C-----for small strain elements
          CALL S4VOLNOD_SM(
     1                     VARNOD6,          V,             NC1,           NC2,
     2                     NC3,              NC4,           VOL0,      GBUF%AMU,
     3                     GBUF%OFF,         GBUF%SMSTR,    NEL,       ISMSTR)
          !Parith-On treatment
          CALL FOAT_TO_6_FLOAT(LFT  ,LLT  ,VOL0 ,VOL06 )
          DO I=LFT,LLT      
            I1=NC1(I)+NUMNOD
            I2=NC2(I)+NUMNOD
            I3=NC3(I)+NUMNOD
            I4=NC4(I)+NUMNOD
            !Parith-On treatment
            DO K=1,6
              VARNOD6(K,I1) = VARNOD6(K,I1) + VOL06(K,I)
              VARNOD6(K,I2) = VARNOD6(K,I2) + VOL06(K,I)
              VARNOD6(K,I3) = VARNOD6(K,I3) + VOL06(K,I)
              VARNOD6(K,I4) = VARNOD6(K,I4) + VOL06(K,I)
            ENDDO
          ENDDO
        ELSE
C-----T10 large strain first         
          NC(LFT:LLT,1) =NC1(LFT:LLT)
          NC(LFT:LLT,2) =NC2(LFT:LLT)
          NC(LFT:LLT,3) =NC3(LFT:LLT)
          NC(LFT:LLT,4) =NC4(LFT:LLT)
          IF(ISROT /= 1)THEN
            DO I=LFT,LLT
             J=I+NFT-NUMELS8
             NC(I,5:10) =IXS10(1:6,J)
            ENDDO
          ELSE
            NC(LFT:LLT,5:10) = 0
          ENDIF
          IF (ISMSTR==10) THEN
            NPT = 4 
            CALL S10VOLNODT3(
     1                       ELBUF_TAB(NG),VARNOD6,     X,           NC,
     2                       GBUF%OFF,  GBUF%SMSTR,   XDP,          NEL,
     3                       NPT)
            IBID = 1
            DO IP=1,NPT
              LBUF => ELBUF_TAB(NG)%BUFLY(IBID)%LBUF(IP,IBID,IBID)
               DO I=LFT,LLT
                IF(GBUF%OFF(I) == ZERO) THEN
                  VOL0(I)=ZERO
                ELSE
                  VOL0(I)=LBUF%VOL(I)
                ENDIF
               ENDDO
               !Parith-On treatment
               CALL FOAT_TO_6_FLOAT(LFT  ,LLT  ,VOL0 ,VOL06 )
               DO I=LFT,LLT
                 I1 = NC(I,IP) +NUMNOD         
                 VARNOD6(1:6,I1) = VARNOD6(1:6,I1) + VOL06(1:6,I)
               ENDDO
            END DO !IP=1,NPT
          ELSE
            CALL S10VOLNOD3(
     1                      VARNOD6,      X,      NC,     GBUF%OFF,
     2                      VOLG   ,    XDP,     NEL,          NPT,
     3                      ISMSTR)
            DO I=LFT,LLT
             IF(GBUF%OFF(I) == ZERO) THEN
               VOL0(I)=ZERO
             ELSE
               VOL0(I)=GBUF%VOL(I)
             ENDIF
            ENDDO
            !Parith-On treatment
            CALL FOAT_TO_6_FLOAT(LFT  ,LLT  ,VOL0 ,VOL06 )
            DO I=LFT,LLT
              I1=NC1(I)+NUMNOD
              I2=NC2(I)+NUMNOD
              I3=NC3(I)+NUMNOD
              I4=NC4(I)+NUMNOD
              !Parith-On treatment
              DO K=1,6
                VARNOD6(K,I1) = VARNOD6(K,I1) + VOL06(K,I)
                VARNOD6(K,I2) = VARNOD6(K,I2) + VOL06(K,I)
                VARNOD6(K,I3) = VARNOD6(K,I3) + VOL06(K,I)
                VARNOD6(K,I4) = VARNOD6(K,I4) + VOL06(K,I)
              ENDDO
            ENDDO
          END IF
        ENDIF !ISROT=3

      ENDDO !DO=1,NG

c SPMD EXCHANGE
      IF(NSPMD > 1)THEN
        LENR = 2*(IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1))
        CALL SPMD_EXCH_VOL(VARNOD6(1,1),VARNOD6(1,NUMNOD+1),IAD_ELEM,FR_ELEM,LENR )
      ENDIF

C PARITH/ON assembly
      DO I=1,NUMNOD

        J=I+NUMNOD
        DO K=1,6
          !VOLNOD
          SFEM_NODVAR(I) = SFEM_NODVAR(I) + VARNOD6(K,I)
          !SFEM_NODVAR
          SFEM_NODVAR(J) = SFEM_NODVAR(J) + VARNOD6(K,J)
        ENDDO

        !MODIFY RELATIVE VOLUME 
        IF(SFEM_NODVAR(J) /= 0)THEN
          SFEM_NODVAR(I)=SFEM_NODVAR(I)/SFEM_NODVAR(J)
        ENDIF
      ENDDO

C-----------------------------------------------
      RETURN
      END
