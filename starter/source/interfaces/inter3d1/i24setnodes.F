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
Chd|  I24SETNODES                   source/interfaces/inter3d1/i24setnodes.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        INTERSURFL                    source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        I7I10SPLITMOD                 share/modules1/i710split_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I24SETNODES(IPARI,INTBUF_TAB,INTERCEP,ITAB,I24MAXNSNE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------    
      USE MESSAGE_MOD
      USE I7I10SPLITMOD 
      USE FRONT_MOD   
      USE INTBUFDEF_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),ITAB(*),I24MAXNSNE
      TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITY,NRTM,N,IED,N1,N2,N3,N4,NRTSE,NSNE
      INTEGER S1,S2,P1,P2,PP,NEXT,POS
      INTEGER NI,K,I,PROC,KM1(4),KM2(4),PROC0,IEDGE4
      DATA KM1/1,2,3,4/
      DATA KM2/2,3,4,1/
      INTEGER  INTERSURFL
      EXTERNAL INTERSURFL             
C--------------------------------------------------------------
      I24MAXNSNE = 0
      DO NI=1,NINTER

        ITY    = IPARI(7,NI) 
        IEDGE4 = IPARI(59,NI)
        IF (ITY==24)THEN

C First set SECONDARY surface to avoid issues : Same SECONDARY surface not on same
C domain than MAIN surface
          NRTSE = IPARI(52,NI) 
          NSNE  = IPARI(55,NI) 
          I24MAXNSNE = MAX(I24MAXNSNE,NSNE)
          IF (IEDGE4 > 0 ) THEN
            IF (.NOT.(ASSOCIATED(INTERCEP(2,NI)%P)))THEN
C             In Type24 INTERCEP(2 gives Edge MPI Domain
              ALLOCATE(INTERCEP(2,NI)%P(NRTSE))
            ENDIF	  
C 
            DO I=1,NRTSE
              N1= INTBUF_TAB(NI)%IRTSE((I-1)*5+1)
              N2= INTBUF_TAB(NI)%IRTSE((I-1)*5+2)
              N3= INTBUF_TAB(NI)%IRTSE((I-1)*5+3)
              N4= INTBUF_TAB(NI)%IRTSE((I-1)*5+4)

 	      PROC = INTERSURFL(N1,N2,N3,N4)
    
              INTERCEP(2,NI)%P(I)=PROC
           ENDDO
          ENDIF


          NRTM   = IPARI(4,NI)
          DO K=1,NRTM	  
             PROC = INTERCEP(1,NI)%P(K)
             IF(PROC==0)THEN
                 CALL ANCMSG(MSGID=978,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANSTOP,
     .                       I1=IPARI(15,NI))
             ELSE
               DO I=1,8
                 N = IABS(INTBUF_TAB(NI)%NVOISIN((K-1)*8+I))
                 IF(N /=0) THEN              
                  CALL IFRONTPLUS(N,PROC)
                 ENDIF
               ENDDO
             END IF
          ENDDO


        ENDIF
      ENDDO 
      END


Chd|====================================================================
Chd|  SECND_SURFACE_ON_DOMAIN       source/interfaces/inter3d1/i24setnodes.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      INTEGER FUNCTION SECND_SURFACE_ON_DOMAIN(INTERCEP,SE,PROC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------    
      USE FRONT_MOD   
      USE INTBUFDEF_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTERSURFP)     :: INTERCEP
       INTEGER SE,PROC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NEXT,P
C-----------------------------------------------
      SECND_SURFACE_ON_DOMAIN = 0

      P= INTERCEP%P(2+2*(SE-1)+1)
      NEXT=INTERCEP%P(2+2*(SE-1)+2) 

      IF (P==PROC)THEN
         SECND_SURFACE_ON_DOMAIN = 1
         RETURN
      ENDIF

      DO WHILE(P/=PROC.AND.NEXT/=-1)
         P=INTERCEP%P(2+2*(NEXT-1)+1)
         NEXT = INTERCEP%P(2+2*(NEXT-1)+2)
      ENDDO

      IF(P==PROC)THEN
         SECND_SURFACE_ON_DOMAIN = 1
      ENDIF
      
      RETURN 

      END
