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
Chd|  S4ALESFEM                     source/elements/solid/solide4_sfem/s4alesfem.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        S4VOLNOD3                     source/elements/solid/solide4_sfem/s4volnod3.F
Chd|        SPMD_EXCH_VOL                 source/mpi/nodes/spmd_exch_vol.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE S4ALESFEM(IPARG,IXS,X,ELBUF_TAB,VARNOD,PM,
     .                     IAD_ELEM,FR_ELEM)
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
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARG(NPARG,*),IAD_ELEM(2,*),FR_ELEM(*)
C     REAL
      my_real
     .   X(*),VARNOD(*),PM(NPROPM,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, I, J, I1, I2, I3, I4, K, LENR,NEL
      INTEGER NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ)
      INTEGER MAT(MVSIZ)
      my_real
     .   MASS(MVSIZ)

      DOUBLE PRECISION
     .   VARNOD6(6,2*NUMNOD), MASS6(6,MVSIZ)
C
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C
      VARNOD(1:2*NUMNOD)      = ZERO
      VARNOD6(1:6,1:2*NUMNOD) = ZERO
      MASS(1:MVSIZ)           = ZERO
      MASS6(1:6,1:MVSIZ)      = ZERO
C----------------------------------------------------
C     COMPUTE NODAL VOLUME & MASS FOR ALL TETRAHEDRON
C----------------------------------------------------
C Boucle parallele dynamique SMP
C
      DO NG = 1,NGROUP
        IF(IPARG(8, NG)==1) CYCLE
        IF(IPARG(28,NG)/=4) CYCLE
        CALL INITBUF (IPARG    ,NG      ,
     2        MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
C
        IF(JEUL  == 1) CYCLE
        IF(ISROT <= 2) CYCLE
        LFT=1
        NEL = LLT
C
        DO I=LFT,LLT
         J=I+NFT
         MAT(I)=IXS(1,J)
         NC1(I)=IXS(2,J)
         NC2(I)=IXS(4,J)
         NC3(I)=IXS(7,J)
         NC4(I)=IXS(6,J)
        ENDDO
C
        GBUF => ELBUF_TAB(NG)%GBUF
        IF(ISROT == 3) THEN
          CALL S4VOLNOD3(VARNOD6,X,NC1,NC2,NC3,NC4,GBUF%OFF,GBUF%SMSTR,NEL)
          DO I=LFT,LLT
            MASS(I)=GBUF%RHO(I)/PM(1,MAT(I))
          ENDDO
          !Parith-On treatment
          CALL FOAT_TO_6_FLOAT(LFT  ,LLT  ,MASS ,MASS6 )
          DO I=LFT,LLT
            I1=NC1(I)+NUMNOD
            I2=NC2(I)+NUMNOD
            I3=NC3(I)+NUMNOD
            I4=NC4(I)+NUMNOD
            !Parith-On treatment
            DO K=1,6
              VARNOD6(K,I1) = VARNOD6(K,I1) + MASS6(K,I)
              VARNOD6(K,I2) = VARNOD6(K,I2) + MASS6(K,I)
              VARNOD6(K,I3) = VARNOD6(K,I3) + MASS6(K,I)
              VARNOD6(K,I4) = VARNOD6(K,I4) + MASS6(K,I)
            ENDDO
          ENDDO
        ENDIF !ISROT=3

      ENDDO !DO=1,NG

c EXCHANGE
      IF(NSPMD>1)THEN
        LENR = 2*(IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1))
        CALL SPMD_EXCH_VOL(VARNOD6(1,1),VARNOD6(1,NUMNOD+1),IAD_ELEM,
     .                     FR_ELEM, LENR )  
      ENDIF

C Routine assembly PARITH/ON
      DO I=1,NUMNOD

         J=I+NUMNOD
         DO K=1,6
          !VOLNOD
           VARNOD(I)        = VARNOD(I)        + VARNOD6(K,I) 
          !VARNOD
           VARNOD(I+NUMNOD) = VARNOD(I+NUMNOD) + VARNOD6(K,I+NUMNOD)          
         ENDDO

         !RHO0/RHO -> VARNOD
         IF(VARNOD(J)/=0)THEN
           VARNOD(I)=VARNOD(I)/VARNOD(J)
         ENDIF
      ENDDO
C
      RETURN
      END
