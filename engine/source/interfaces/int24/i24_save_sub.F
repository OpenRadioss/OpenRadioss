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
Chd|  I24_SAVE_SUB                  source/interfaces/int24/i24_save_sub.F
Chd|-- called by -----------
Chd|        I24FOR3                       source/interfaces/int24/i24for3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
       SUBROUTINE I24_SAVE_SUB(NUMNOD,MVSIZ,NISUB,S_ADDSUBM,S_LISUBM,S_TYPSUB,NISUBMAX,I_STOK,
     *                         IE,ITYPSUB,NIN,I,NN,NFT,
     *                         ADDSUBM,LISUBM,TYPSUB,
     *                         INTAREAN,INTCAREA,ISENSINT,
     *                         FXI,FYI,FZI,FNI,DT12,
     *                         FSAVSUB1,FSAVPARIT )
!! \brief Routine to save values for type24 Sub interface for output
!! \details moved from i24for3.F to secondary subroutine due to compiler issue.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
      INTEGER INTAREAN(NUMNOD)
      INTEGER INTCAREA
      my_real FSAVSUB1(25,NISUB)
      my_real FSAVPARIT(NISUB+1,11,I_STOK)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK,ISUB
      my_real IMPX,IMPY,IMPZ
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
                     FSAVSUB1(25,ISUB) = FSAVSUB1(25,ISUB) + INTAREAN(NN) 
                 ELSE
                     FSAVSUB1(25,ISUB) = FSAVSUB1(25,ISUB) + INTAREANFI(NIN)%P(-NN)
                 ENDIF
              ENDIF

            ENDIF

      ENDDO

      END

