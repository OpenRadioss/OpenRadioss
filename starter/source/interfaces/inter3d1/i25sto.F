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
!||    i25sto             ../starter/source/interfaces/inter3d1/i25sto.F
!||--- called by ------------------------------------------------------
!||    i25trivox1         ../starter/source/interfaces/inter3d1/i25trivox1.F
!||--- calls      -----------------------------------------------------
!||    i25cor3t           ../starter/source/interfaces/inter3d1/i25cor3t.F
!||    i25pen3a           ../starter/source/interfaces/inter3d1/i25pen3a.F
!||    i25s1s2            ../starter/source/interfaces/inter3d1/i25sto.F
!||--- uses       -----------------------------------------------------
!||    tri7box            ../starter/share/modules1/tri7box.F
!||====================================================================
      SUBROUTINE I25STO(
     1      J_STOK,IRECT  ,X     ,NSV   ,local_i_stok,
     2      local_cand_n,local_cand_e ,MARGE  ,
     3      PROV_N ,PROV_E,ESHIFT,NSN    ,
     4      NRTM  ,GAP_S  ,GAP_M ,NBINFLG,MBINFLG,
     5      ILEV ,MSEGTYP,IGAP,GAP_S_L,
     6      GAP_M_L,EDGE_L2,ICODE,ISKEW,DRAD,
     7      DGAPLOAD,NRTMT)
C============================================================================
      USE TRI7BOX
      use array_mod 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NRTM, NBINFLG(*),MBINFLG(*),ILEV, IGAP
      INTEGER J_STOK,ESHIFT
      INTEGER IRECT(4,*),NSV(*)
      INTEGER PROV_N(MVSIZ),PROV_E(MVSIZ),local_i_stok,MSEGTYP(*),ICODE(*),ISKEW(*)
C     REAL
      my_real
     .        X(3,*), GAP_S(*), GAP_M(*),
     .        MARGE, 
     .        GAP_S_L(*), GAP_M_L(*), EDGE_L2(*)
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      INTEGER , INTENT(IN) :: NRTMT

      type(array_type_int_1d) :: local_cand_n
      type(array_type_int_1d) :: local_cand_e
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K_STOK,ITYPE
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        ETYP(MVSIZ), IBC(MVSIZ)
      integer :: my_old_size,my_size
      integer, dimension(:), allocatable :: tmp_array_1,tmp_array_2
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ), 
     .   PENE(MVSIZ), GAPV(MVSIZ)
      DATA ITYPE/25/
C-----------------------------------------------
      CALL I25COR3T( J_STOK  ,X    ,IRECT   ,NSV   ,PROV_E  ,
     1               PROV_N  ,X1    ,X2      ,
     2               X3      ,X4   ,Y1      ,Y2    ,Y3      ,
     3               Y4      ,Z1   ,Z2      ,Z3    ,Z4      ,
     4               XI      ,YI   ,ZI      ,STIF  ,IX1     ,
     5               IX2     ,IX3  ,IX4     ,NSN   ,NRTM    ,
     6               MARGE   ,GAP_S,GAP_M   ,GAPV  ,ITYPE   ,
     7               IGAP ,GAP_S_L ,GAP_M_L ,EDGE_L2,MSEGTYP,
     8               ETYP ,ICODE   ,ISKEW    ,IBC   ,DRAD   ,
     9               DGAPLOAD ,NRTMT)
C-----------------------------------------------
      CALL I25PEN3A( J_STOK ,X1    ,X2     ,X3   ,X4    ,
     .                Y1    ,Y2    ,Y3     ,Y4   ,
     .                Z1    ,Z2    ,Z3    ,Z4     ,XI   ,
     .                YI    ,ZI    ,PENE  ,IX1    ,IX2  ,
     .                IX3   ,IX4   ,GAPV  ,NRTM   ,ETYP ,
     .                IBC   ,NRTMT )
C-----------------------------------------------
      IF (ILEV==2) 
     .  CALL I25S1S2(J_STOK,PROV_N,PROV_E,NBINFLG,MBINFLG,PENE)
C-----------------------------------------------
      K_STOK = 0
      DO I=1,J_STOK
          IF(PENE(I)/=ZERO) THEN
            K_STOK = K_STOK + 1
          END IF 
      ENDDO
      IF(K_STOK==0)RETURN
C
      if(local_i_stok+k_stok>local_cand_n%size_int_array_1d) then
        my_old_size = local_cand_n%size_int_array_1d
        my_size = (my_old_size+k_stok) * 1.25
        allocate( tmp_array_1( my_size ) )
        allocate( tmp_array_2( my_size ) )
        tmp_array_1(1:my_old_size) = local_cand_n%int_array_1d(1:my_old_size)
        tmp_array_2(1:my_old_size) = local_cand_e%int_array_1d(1:my_old_size)
        call dealloc_1d_array(local_cand_n)
        call dealloc_1d_array(local_cand_e)
        call move_alloc(tmp_array_1,local_cand_n%int_array_1d)
        call move_alloc(tmp_array_2,local_cand_e%int_array_1d)
        local_cand_n%size_int_array_1d = my_size
        local_cand_e%size_int_array_1d = my_size
      endif
      DO I=1,J_STOK
         IF(PENE(I)/=ZERO)THEN
           local_i_stok = local_i_stok + 1
           local_cand_n%int_array_1d(local_i_stok) = PROV_N(I)
           local_cand_e%int_array_1d(local_i_stok) = PROV_E(I)+ESHIFT
         ENDIF
       ENDDO
C-----------------------------------------------
      RETURN
      END
!||====================================================================
!||    i25s1s2   ../starter/source/interfaces/inter3d1/i25sto.F
!||--- called by ------------------------------------------------------
!||    i25sto    ../starter/source/interfaces/inter3d1/i25sto.F
!||--- calls      -----------------------------------------------------
!||    bitget    ../starter/source/interfaces/inter3d1/bitget.F
!||====================================================================
      SUBROUTINE I25S1S2(J_STOK,PROV_N,PROV_E,NBINFLG,MBINFLG,PENE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER J_STOK,PROV_N(*),PROV_E(*),NBINFLG(*),MBINFLG(*)
      my_real
     .   PENE(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NE,IMS1,IMS2,ISS1,ISS2
C-----------------------------------------------
C
      INTEGER BITGET
      EXTERNAL BITGET
C=======================================================================
         DO I=1,J_STOK
           N  = PROV_N(I)
           NE = PROV_E(I)
           IMS1 = BITGET(MBINFLG(NE),0)
           IMS2 = BITGET(MBINFLG(NE),1)
           ISS1 = BITGET(NBINFLG(N),0)
           ISS2 = BITGET(NBINFLG(N),1)
           IF(.NOT.((IMS1 == 1 .and. ISS2==1).or.
     .              (IMS2 == 1 .and. ISS1==1)))THEN
             PENE(I)=ZERO
           ENDIF
         ENDDO
C
      RETURN
      END
