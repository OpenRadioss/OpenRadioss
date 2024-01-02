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
Chd|  ININTMASS                     source/interfaces/inter3d1/inintmass.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ININTMASS( IPARI, INTBUF_TAB, MS  ,ISTIF_DT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(INOUT) :: ISTIF_DT
      INTEGER , INTENT(IN   ) :: IPARI(NPARI,NINTER) 
      my_real , INTENT(IN   ) :: MS(NUMNOD) 
      TYPE(INTBUF_STRUCT_) , INTENT(IN   ) ::  INTBUF_TAB(NINTER)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   N ,NS ,N1, N2, N3 ,N4 ,I ,NI ,NTY ,NRTM ,NSN ,ISTIF_MSDT ,FLAGDT ,
     .   NEDGE
      my_real 
     .   STFACM ,MS1 ,MS2 ,MS3 ,MS4 ,DTSTIF
C----------------------------------------------------------
C    Main and secondary masses needed for contact stiffness computation 
C----------------------------------------------------------
          DO N=1,NINTER
             NTY = IPARI(7,N)
             IF(NTY==24.OR.NTY==25) THEN
                ISTIF_MSDT = IPARI(97,N)
                IF(ISTIF_MSDT > 0) THEN
                   NRTM     = IPARI(4,N)
                   NSN      = IPARI(5,N)
                   STFACM   = INTBUF_TAB(N)%VARIABLES(47)
                   DO I=1,NSN
                      NS = INTBUF_TAB(N)%NSV(I)
                      INTBUF_TAB(N)%STIFMSDT_S(I)  = STFACM*MS(NS)
                   ENDDO  
                   DO I=1,NRTM
                      N1=INTBUF_TAB(N)%IRECTM(4*(I-1)+1)
                      MS1 = MS(N1)
                      N2=INTBUF_TAB(N)%IRECTM(4*(I-1)+2)
                      MS2 = MS(N2)
                      N3=INTBUF_TAB(N)%IRECTM(4*(I-1)+3)
                      MS3 = MS(N3)
                      N4=INTBUF_TAB(N)%IRECTM(4*(I-1)+4)
                      IF (N3 /= N4) THEN
                         MS4 = MS(N4)
                         INTBUF_TAB(N)%STIFMSDT_M(I) = STFACM*FOURTH*(MS1+MS2+MS3+MS4)
                      ELSE
                        INTBUF_TAB(N)%STIFMSDT_M(I) = STFACM*THIRD*(MS1+MS2+MS3)
                      ENDIF
                    ENDDO  

                    IF(NTY==25.AND.IPARI(58,N) > 0) THEN
                       NEDGE = IPARI(68,N)
                       DO I=1,NEDGE
                          N1= INTBUF_TAB(N)%LEDGE(NLEDGE*(I-1)+5)
                          N2= INTBUF_TAB(N)%LEDGE(NLEDGE*(I-1)+6)
                          MS1 = MS(N1)
                          MS2 = MS(N2)
                          INTBUF_TAB(N)%STIFMSDT_EDG(I) = HALF*STFACM*(MS1+MS2)
                       ENDDO
                    ENDIF               
                ENDIF
             ENDIF
          ENDDO
C---- ISTIF_DT flag to compute inital time step if no contact time step is set -----
          FLAGDT = 0
          DO N=1,NINTER
             NTY = IPARI(7,N)
             IF(NTY==24.OR.NTY==25) THEN
                ISTIF_MSDT = IPARI(97,N)
                DTSTIF   = INTBUF_TAB(N)%VARIABLES(48)
                IF(ISTIF_MSDT > 0) THEN
                   IF (DTSTIF == ZERO) FLAGDT = 1
                ENDIF
             ENDIF
          ENDDO
          ISTIF_DT =    FLAGDT

      RETURN
      END SUBROUTINE ININTMASS
