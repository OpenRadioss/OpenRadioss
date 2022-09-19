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
Chd|  ASSPARXX                      source/assembly/assparxx.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE ASSPARXX(ITSK,INTLIST,NBINTC,IPARI)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITSK,NBINTC,INTLIST(*),IPARI(NPARI,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER K,NIN,NODFI,ISHIFT,IG,NI,NTY,NODFTSK,NODLTSK,INTTH,NODFIE
C

      DO NI = 1,NBINTC
         NIN= INTLIST(NI)
         NTY   =IPARI(7,NIN)
         INTTH = IPARI(47,NIN)

         IF(NTY /= 7 .AND. NTY /= 10 .AND. NTY /= 11.AND. NTY /= 24 .AND. NTY /= 25 .AND. NTY /= 20.AND. NTY /= 23) CYCLE
         NODFI = NLSKYFI(NIN)

         NODFTSK   = 1+ITSK*NODFI/ NTHREAD
         NODLTSK   = (ITSK+1)*NODFI/NTHREAD

         DO K=2,NTHREAD

            ISHIFT = NODFI*(K-1)

            DO IG=NODFTSK,NODLTSK !LENR
              AFI(NIN)%P(1,IG)=AFI(NIN)%P(1,IG)+AFI(NIN)%P(1,IG+ISHIFT)
              AFI(NIN)%P(2,IG)=AFI(NIN)%P(2,IG)+AFI(NIN)%P(2,IG+ISHIFT)
              AFI(NIN)%P(3,IG)=AFI(NIN)%P(3,IG)+AFI(NIN)%P(3,IG+ISHIFT)
              STNFI(NIN)%P(IG)=STNFI(NIN)%P(IG)+STNFI(NIN)%P(IG+ISHIFT)
C
              AFI(NIN)%P(1,IG+ISHIFT) = ZERO
              AFI(NIN)%P(2,IG+ISHIFT) = ZERO
              AFI(NIN)%P(3,IG+ISHIFT) = ZERO
              STNFI(NIN)%P(IG+ISHIFT) = ZERO
            ENDDO

            IF (INTTH /=0)THEN
              DO IG=NODFTSK,NODLTSK !LENR
                FTHEFI(NIN)%P(IG)=FTHEFI(NIN)%P(IG)+FTHEFI(NIN)%P(IG+ISHIFT)
                FTHEFI(NIN)%P(IG+ISHIFT)=ZERO

                IF(NODADT_THERM == 1 ) THEN
                  CONDNFI(NIN)%P(IG)=CONDNFI(NIN)%P(IG)+CONDNFI(NIN)%P(IG+ISHIFT)
                  CONDNFI(NIN)%P(IG+ISHIFT)=ZERO
                ENDIF
              ENDDO
            ENDIF

            IF(KDTINT/=0)THEN
              DO IG=NODFTSK,NODLTSK
                VSCFI(NIN)%P(IG)=VSCFI(NIN)%P(IG)+VSCFI(NIN)%P(IG+ISHIFT)
                VSCFI(NIN)%P(IG+ISHIFT)=ZERO
              ENDDO
            ENDIF
         ENDDO

         IF(NTY==20 .OR. (NTY==25 .AND. IPARI(58,NIN) > 0))THEN
            NODFIE =  NLSKYFIE(NIN)
            IF(NODFIE > 0)THEN
               NODFTSK   = 1+ITSK*NODFIE/ NTHREAD
               NODLTSK   = (ITSK+1)*NODFIE/NTHREAD

               DO K=2,NTHREAD
                 ISHIFT = NODFIE*(K-1)
                 DO IG=NODFTSK,NODLTSK !LENR
                   AFIE(NIN)%P(1,IG)=AFIE(NIN)%P(1,IG)+AFIE(NIN)%P(1,IG+ISHIFT)
                   AFIE(NIN)%P(2,IG)=AFIE(NIN)%P(2,IG)+AFIE(NIN)%P(2,IG+ISHIFT)
                   AFIE(NIN)%P(3,IG)=AFIE(NIN)%P(3,IG)+AFIE(NIN)%P(3,IG+ISHIFT)
                   STNFIE(NIN)%P(IG)=STNFIE(NIN)%P(IG)+STNFIE(NIN)%P(IG+ISHIFT)
C
                   AFIE(NIN)%P(1,IG+ISHIFT) = ZERO
                   AFIE(NIN)%P(2,IG+ISHIFT) = ZERO
                   AFIE(NIN)%P(3,IG+ISHIFT) = ZERO
                   STNFIE(NIN)%P(IG+ISHIFT) = ZERO
                 ENDDO

                 IF(KDTINT/=0)THEN
                    DO IG=NODFTSK,NODLTSK
                     VSCFIE(NIN)%P(IG)=VSCFI(NIN)%P(IG)+VSCFIE(NIN)%P(IG+ISHIFT)
                     VSCFIE(NIN)%P(IG+ISHIFT)=ZERO
                    ENDDO
                ENDIF
               ENDDO


            ENDIF
         ENDIF
      ENDDO


      RETURN
      END
