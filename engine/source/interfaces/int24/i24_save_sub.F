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
!||    i24_save_sub         ../engine/source/interfaces/int24/i24_save_sub.F
!||--- called by ------------------------------------------------------
!||    i24for3              ../engine/source/interfaces/int24/i24for3.F
!||--- calls      -----------------------------------------------------
!||    i24intarea_fic       ../engine/source/interfaces/int24/i24intarea_fic.F90
!||--- uses       -----------------------------------------------------
!||    i24intarea_fic_mod   ../engine/source/interfaces/int24/i24intarea_fic.F90
!||    tri7box              ../engine/share/modules/tri7box.F
!||====================================================================
       SUBROUTINE I24_SAVE_SUB(NUMNOD,MVSIZ,NISUB,S_ADDSUBM,S_LISUBM,S_TYPSUB,NISUBMAX,I_STOK,
     *                         IE,ITYPSUB,NIN,I,NN,NFT,
     *                         ADDSUBM,LISUBM,TYPSUB,
     *                         INTAREAN,INTCAREA,ISENSINT,
     *                         FXI,FYI,FZI,FNI,DT12,
     *                         FSAVSUB1,FSAVPARIT ,NRTSE,
     *                         IRTSE,NSNE,IS2SE   ,IS2PT,NSNR)
!! \brief Routine to save values for type24 Sub interface for output
!! \details moved from i24for3.F to secondary subroutine due to compiler issue.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE I24INTAREA_FIC_MOD  , ONLY : I24INTAREA_FIC  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMNOD       !< NUMBER of nodes in Model
      INTEGER MVSIZ        !< Vector size
      INTEGER S_ADDSUBM    !< Size of ADDSUBM (computed in Starter)
      INTEGER S_LISUBM     !< Size of LISUBM (computed in Starter)
      INTEGER S_TYPSUB     !< Size of TYPSUB (computed in Starter)
      INTEGER NISUBMAX     !< Size of ISENSINT
      INTEGER NISUB        !< Number of Sub interfaces
      INTEGER I_STOK       !< Number of contact pair / Dimension for FSAVPARIT
      INTEGER IE
      INTEGER ITYPSUB
      INTEGER NIN
      INTEGER NN
      INTEGER I            !< Iterator over Impact : Main Surface/Sec Node from caller routine
      INTEGER NFT          !< Current pinter to FSAVSUB1
      INTEGER ADDSUBM(S_ADDSUBM)
      INTEGER LISUBM(S_LISUBM)
      INTEGER TYPSUB(S_TYPSUB)
      INTEGER ISENSINT(NISUBMAX+1)
      my_real FXI(MVSIZ)
      my_real FYI(MVSIZ)
      my_real FZI(MVSIZ)
      my_real FNI(MVSIZ)
      my_real DT12
      my_real INTAREAN(NUMNOD)
      INTEGER INTCAREA
      my_real FSAVSUB1(25,NISUB)
      my_real FSAVPARIT(NISUB+1,11,I_STOK)
      INTEGER, INTENT(IN) :: NSNE,NRTSE,NSNR
      INTEGER, DIMENSION(5,NRTSE),INTENT(IN) :: IRTSE
      INTEGER, DIMENSION(2,NSNE),INTENT(IN) :: IS2SE
      INTEGER, DIMENSION(NSNE),INTENT(IN) :: IS2PT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK,ISUB,IG
      my_real IMPX,IMPY,IMPZ,AREAN_FIC
C-----------------------------------------------
      DO KK=ADDSUBM(IE),ADDSUBM(IE+1)-1
            ISUB=LISUBM(KK)
            ITYPSUB = TYPSUB(ISUB)

            IF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface : main side

              IMPX=-FXI(I)*DT12
              IMPY=-FYI(I)*DT12
              IMPZ=-FZI(I)*DT12

              FSAVSUB1(1,ISUB)=FSAVSUB1(1,ISUB)+IMPX
              FSAVSUB1(2,ISUB)=FSAVSUB1(2,ISUB)+IMPY
              FSAVSUB1(3,ISUB)=FSAVSUB1(3,ISUB)+IMPZ

              FSAVSUB1(8,ISUB) =FSAVSUB1(8,ISUB) +ABS(IMPX)
              FSAVSUB1(9,ISUB) =FSAVSUB1(9,ISUB) +ABS(IMPY)
              FSAVSUB1(10,ISUB)=FSAVSUB1(10,ISUB)+ABS(IMPZ)

              FSAVSUB1(11,ISUB)=FSAVSUB1(11,ISUB)-FNI(I)*DT12

              IF(ISENSINT(ISUB+1)/=0) THEN
                FSAVPARIT(ISUB+1,1,I+NFT) =  -FXI(I)
                FSAVPARIT(ISUB+1,2,I+NFT) =  -FYI(I)
                FSAVPARIT(ISUB+1,3,I+NFT) =  -FZI(I)
              ENDIF

              IF(INTCAREA > 0)  THEN
                 IF(NN > 0) THEN
                     IF(NN <=NUMNOD) THEN
                        FSAVSUB1(25,ISUB) = FSAVSUB1(25,ISUB) + INTAREAN(NN) 
                     ELSE
                        IG = NN - NUMNOD
                        CALL I24INTAREA_FIC(IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,IG      , 
     +                                    NRTSE , NUMNOD     ,INTAREAN,  AREAN_FIC )
                        FSAVSUB1(25,ISUB) = FSAVSUB1(25,ISUB) + AREAN_FIC
                    ENDIF
                 ELSE
                    IF(ISEDGE_FI(NIN)%P(NN)==1)THEN
                       CALL I24INTAREA_FIC(IRTSE_FI(NIN)%P    ,NSNR    ,IS2SE_FI(NIN)%P  ,IS2PT_FI(NIN)%P   ,NN      , 
     +                                        NSNR , NSNR     , INTAREANFI(NIN)%P, AREAN_FIC)
                          FSAVSUB1(25,ISUB) = FSAVSUB1(25,ISUB) + AREAN_FIC 
                    ELSE ! cas noeud remote en SPMD non edge
                          FSAVSUB1(25,ISUB) = FSAVSUB1(25,ISUB) + INTAREANFI(NIN)%P(NN)
                    ENDIF
                 ENDIF
              ENDIF

            ENDIF

      ENDDO

      END

