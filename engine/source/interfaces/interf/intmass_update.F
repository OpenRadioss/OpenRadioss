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
Chd|  INTMASS_UPDATE                source/interfaces/interf/intmass_update.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE INTMASS_UPDATE(NIN  , IPARI, INTBUF_TAB, MS )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
      USE TRI25EBOX
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN   ) :: NIN
      INTEGER , INTENT(IN   ) :: IPARI(NPARI) 
      my_real , INTENT(IN   ) :: MS(NUMNOD) 
      TYPE(INTBUF_STRUCT_) , INTENT(INOUT   ) ::  INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   NS ,N1, N2, N3 ,N4 ,I ,NTY ,NRTM ,NSN ,NSNR ,NEDGE ,NEDGR 
      my_real 
     .   STFACM ,MS1 ,MS2 ,MS3 ,MS4 
C----------------------------------------------------------
C    Main and secondary masses needed for contact stiffness computation 
C----------------------------------------------------------

        NRTM     = IPARI(4)
        NSN      = IPARI(5)
        STFACM   = INTBUF_TAB%VARIABLES(47)
        DO I=1,NSN
           NS = INTBUF_TAB%NSV(I)
           INTBUF_TAB%STIFMSDT_S(I)  = STFACM*MS(NS)
        ENDDO  
        DO I=1,NRTM
           N1=INTBUF_TAB%IRECTM(4*(I-1)+1)
           MS1 = MS(N1)
           N2=INTBUF_TAB%IRECTM(4*(I-1)+2)
           MS2 = MS(N2)
           N3=INTBUF_TAB%IRECTM(4*(I-1)+3)
           MS3 = MS(N3)
           IF (N3 /= N4) THEN
              N4=INTBUF_TAB%IRECTM(4*(I-1)+4)
              MS4 = MS(N4)
              INTBUF_TAB%STIFMSDT_M(I) = STFACM*FOURTH*(MS1+MS2+MS3+MS4)
           ELSE
              INTBUF_TAB%STIFMSDT_M(I) = STFACM*THIRD*(MS1+MS2+MS3)
           ENDIF
        ENDDO  

        IF(IPARI(7)==25.AND.IPARI(58)>0) THEN ! case of edges
           NEDGE = IPARI(68)
           DO I=1,NEDGE
              N1= INTBUF_TAB%LEDGE(NLEDGE*(I-1)+5)
              N2= INTBUF_TAB%LEDGE(NLEDGE*(I-1)+6)
              MS1 = MS(N1)
              MS2 = MS(N2)
              INTBUF_TAB%STIFMSDT_EDG(I) = HALF*STFACM*(MS1+MS2)
           ENDDO         
        ENDIF
 
        
        IF(NSPMD > 1) THEN
           NSNR = IPARI(24)
           DO I=1,NSNR
              STIF_MSDT_FI(NIN)%P(I) = STFACM*MSFI(NIN)%P(I) 
           ENDDO
           IF(IPARI(7)==25.AND.IPARI(58)>0) THEN ! case of edges
              NEDGR = IPARI(69)
              DO I=1,NEDGR
                 N1 =2*(I-1)+1
                 N2 =2*I
                 MS1 = MSFIE(NIN)%P(N1)
                 MS2 = MSFIE(NIN)%P(N2)
                 STIFE_MSDT_FI(NIN)%P(I) = HALF*STFACM*(MS1+MS2)
             ENDDO
           ENDIF
        ENDIF              
 
      RETURN
      END SUBROUTINE INTMASS_UPDATE
