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
Chd|  I18TRI                        source/interfaces/int18/i18tri.F
Chd|-- called by -----------
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I18TRI(INDEX,I_STOK,CAND_N,ITAB, NSN ,
     2                  NSV  ,NIN   )
C Tri des candidats suivant le numero de noeud user
C=======================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_STOK, NSN, NIN,
     .        INDEX(*),CAND_N(*), ITAB(*), NSV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, NI,
     .         ITRI(I_STOK),INDTRI(2*I_STOK),WORK(70000)
C-----------------------------------------------
      DO I = 1, I_STOK
        NI = CAND_N(INDEX(I))                  ! l ordre des facette n importe plus a nouveau P/on
         IF(NI<=NSN) THEN
            ITRI(I) = ITAB(NSV(NI)) ! 1 seul critere : noeud (user)
        ELSE
          ITRI(I) = ITAFI(NIN)%P(NI-NSN)       ! attention au noeud frontiere
        END IF
      END DO
C
      CALL MY_ORDERS(0,WORK,ITRI,INDTRI,I_STOK,1)
C
      DO I = 1, I_STOK
        INDTRI(I_STOK+I) = INDEX(I)
      END DO
C
      DO I = 1, I_STOK
        INDEX(I) = INDTRI(I_STOK+INDTRI(I))
      END DO
C
      RETURN
      END
