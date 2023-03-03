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
Chd|  ININTR_ORTHDIRFRIC            source/interfaces/interf1/inintr_orthdirfric.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|        ORTHDIR_PROJ                  source/interfaces/interf1/inintr_orthdirfric.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ININTR_ORTHDIRFRIC(
     A                  IPARI   ,INTBUF_TAB,INTBUF_FRIC_TAB,IGEO  ,GEO      ,
     B                  X       , IXTG     ,IXC          ,IPARTTG , IPARTC  ,
     C                  PFRICORTH,IREPFORTH,PHIFORTH     , VFORTH ,KNOD2ELC ,
     D                  KNOD2ELTG,NOD2ELTG ,NOD2ELC      ,IWORKSH  ,PM      ,
     E                  PM_STACK ,THK          ,SKEW     ,ITAB     ,IPART   ) 
!IDFRICORIENT,TITFRICORIENT, 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE INTBUF_FRIC_MOD                     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),  IPARTTG(*), IPARTC(*) ,
     .        IXC(NIXC,*), IXTG(NIXTG,*),IPART(LIPART1,*) ,
     .        IREPFORTH(*), PFRICORTH(*),IGEO(NPROPGI,*),ITAB(*),
     .        KNOD2ELC(*), KNOD2ELTG(*), NOD2ELC(*), NOD2ELTG(*), 
     .        IWORKSH(3,*)
c     .        IDFRICORIENT(*),
      my_real X(3,*), PHIFORTH(*), VFORTH(3,*) ,GEO(NPROPG,*),PM(NPROPM,*),
     .        PM_STACK(20,*) ,THK(*) ,SKEW(LSKEW,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
c      CHARACTER*ncharline ,  TITFRICORIENT(*)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N ,NIF ,IREP ,NLAY ,IORTH ,IE ,NRTM ,I ,NELTG ,NELC ,STAT ,
     .   NRT_SH,J,INRT ,NTY ,IL ,N3 ,N4 ,IP ,IPORTH , IGTYP ,ID   ,ISU2 ,ILEV ,ISU1,NRT1,NRT2,NSHIF,
     .   PID ,ISK
      my_real
     .   VX ,VY ,VZ ,E1X ,E1Y ,E1Z ,E2X ,E2Y ,E2Z ,E3X ,E3Y ,E3Z ,
     .   RX ,RY ,RZ ,SX ,SY ,SZ ,SUMA ,S1 ,S2 ,VR ,VS ,CP , SP ,
     .   AA ,BB ,D1 ,D2 ,S ,DET ,PHI ,U1X ,U1Y ,U2X ,U2Y ,W1X ,W1Y ,W2X ,W2Y ,
     .   TORTH , SUM
c      CHARACTER*nchartitle,
c     .   TITR 
C-----------------------------------------------

C----
       DO N=1,NINTER
          NTY   =IPARI(7,N)
          IF(NTY == 7.OR.NTY==24.OR.NTY==25) THEN
             NIF = IPARI(72,N)
             IF(NIF > 0) THEN
               IORTH = INTBUF_FRIC_TAB(NIF)%IORTHFRIC
               IF(IORTH > 0 ) THEN
                  NRTM  =IPARI(4,N)
                  DO I=1,NRTM
                     NELC = 0
                     NELTG = 0
                     CALL INCOQ3(INTBUF_TAB(N)%IRECTM,IXC ,IXTG ,N    ,NELC     ,
     .                            NELTG              ,I   ,GEO  ,PM   ,KNOD2ELC ,
     .                            KNOD2ELTG         ,NOD2ELC ,NOD2ELTG,THK,NTY,IGEO,
     .                            PM_STACK          , IWORKSH)

                     IF(NELTG/=0) THEN    
                        IP= IPARTTG(NELTG)
                        IE = NELTG
                        IGTYP = IGEO(11,IXTG(NIXTG-1,IE))
                        PID   = IXTG(NIXTG-1,IE)
                     ELSE
                        IP= IPARTC(NELC)
                        IE = NELC
                        IGTYP = IGEO(11,IXC(NIXC-1,IE))
                        PID   = IXC(NIXC-1,IE)
                     ENDIF
                     IF(IE > 0) THEN
                        IPORTH = PFRICORTH(IP)
C---1st Case : orthotropic directions are defined in /FRITION/ORIENTATION for part IP

                        IF(IPORTH >0) THEN 
c
                          PHI = PHIFORTH(IPORTH)  
                          IREP = IREPFORTH(IPORTH)

                          INTBUF_TAB(N)%IREP_FRICM(I) = IREP
                          VX  = VFORTH(1,IPORTH)   
                          VY  = VFORTH(2,IPORTH)                     
                          VZ  = VFORTH(3,IPORTH)  

                         CALL ORTHDIR_PROJ(
     .                         I      ,VX    , VY        ,VZ     , PHI   ,          
     .                         IREP   ,X     ,INTBUF_TAB(N)%IRECTM,ITAB  , 
     .                         INTBUF_TAB(N)%DIR_FRICM,IP  ,IPART      )     
c
C---2nd Case : Friction orthotropic directions same as property

                        ELSEIF(IGTYP == 9.OR.IGTYP==10.OR.IGTYP==11.OR.IGTYP==17.OR.IGTYP==51.OR.IGTYP==52) THEN
c
                          IREP = IGEO(6,PID)
                          INTBUF_TAB(N)%IREP_FRICM(I) = IREP

                          INTBUF_TAB(N)%IREP_FRICM(I) = IREP
                          IF(IGTYP==9.OR.IGTYP==10) THEN
                             ISK = 0
                          ELSE
                             ISK = IGEO(2,PID)
                          ENDIF
                          IF(ISK==0) THEN
                             VX  = GEO(7,PID)  
                             VY  = GEO(8,PID)                    
                             VZ  = GEO(9,PID)
                          ELSE             
                             VX = SKEW(1,ISK)
                             VY = SKEW(2,ISK)
                             VZ = SKEW(3,ISK)
                          ENDIF 
                          NLAY = IGEO(15,PID)
                          IF(NLAY == 1) THEN
                             PHI = GEO(10,PID) 
                          ELSE
                             IL = IABS(NLAY)/2 + 1
                             PHI =GEO(200+IL,PID)
                          ENDIF

                         CALL ORTHDIR_PROJ(
     .                         I      ,VX    , VY        ,VZ     , PHI   ,          
     .                         IREP   ,X     ,INTBUF_TAB(N)%IRECTM,ITAB  , 
     .                         INTBUF_TAB(N)%DIR_FRICM,IP  ,IPART      )    
c
C---3rd Case : Isotropic friction
                         ELSE                
                          INTBUF_TAB(N)%IREP_FRICM(I) = 10  
c
                         ENDIF
                      ENDIF
                    ENDDO
               ENDIF
             ENDIF
           ENDIF
       ENDDO

C-----------
      RETURN
      END

Chd|====================================================================
Chd|  ORTHDIR_PROJ                  source/interfaces/interf1/inintr_orthdirfric.F
Chd|-- called by -----------
Chd|        ININTR_ORTHDIRFRIC            source/interfaces/interf1/inintr_orthdirfric.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ORTHDIR_PROJ(
     .                  I      ,VX    , VY        ,VZ     , PHI   ,          
     .                  IREP   ,X     ,IRECTM     , ITAB  ,
     .                  DIR_FRICM,IP  ,IPART      )   
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .        I ,IREP ,IP ,
     .        IRECTM(4,*),ITAB(*),IPART(LIPART1,*)
      my_real VX ,VY ,VZ ,PHI ,X(3,*), DIR_FRICM(2,*)
c      CHARACTER*ncharline ,  TITFRICORIENT(*)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1 ,N2,N3 ,N4 
      my_real
     .   E1X ,E1Y ,E1Z ,E2X ,E2Y ,E2Z ,E3X ,E3Y ,E3Z ,
     .   RX ,RY ,RZ ,SX ,SY ,SZ ,SUMA ,S1 ,S2 ,VR ,VS ,CP , SP ,
     .   AA ,BB ,D1 ,D2 ,S ,DET ,U1X ,U1Y ,U2X ,U2Y ,W1X ,W1Y ,W2X ,W2Y ,
     .   TORTH , SUM ,V
C-----------------------------------------------
        N1 = IRECTM(1,I)
        N2 = IRECTM(2,I)
        N3 = IRECTM(3,I)
        N4 = IRECTM(4,I)

C---Element Frame :

        IF (N3 /= N4) THEN
C---      shell 4N
           E1X= X(1,N2) + X(1,N3) - X(1,N1) - X(1,N4) 
           E1Y= X(2,N2) + X(2,N3) - X(2,N1) - X(2,N4) 
           E1Z= X(3,N2) + X(3,N3) - X(3,N1) - X(3,N4) 
 
           E2X= X(1,N3) + X(1,N4) - X(1,N1) - X(1,N2) 
           E2Y= X(2,N3) + X(2,N4) - X(2,N1) - X(2,N2)  
           E2Z= X(3,N3) + X(3,N4) - X(3,N1) - X(3,N2) 
      
        ELSE
C---      shell 3N
           E1X= X(1,N2) - X(1,N1)
           E1Y= X(2,N2) - X(2,N1)
           E1Z= X(3,N2) - X(3,N1)
           E2X= X(1,N3) - X(1,N1)
           E2Y= X(2,N3) - X(2,N1)
           E2Z= X(3,N3) - X(3,N1)
        ENDIF
        RX = E1X
        RY = E1Y
        RZ = E1Z
        SX = E2X
        SY = E2Y
        SZ = E2Z
c
        E3X = E1Y*E2Z-E1Z*E2Y
        E3Y = E1Z*E2X-E1X*E2Z
        E3Z = E1X*E2Y-E1Y*E2X  
  
        SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z 
        SUMA   = ONE/MAX(SQRT(SUMA),EM20)                    
        E3X = E3X*SUMA                              
        E3Y = E3Y*SUMA                              
        E3Z = E3Z*SUMA    
                          
C
        S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z
        S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z
        SUMA   = SQRT(S1/S2)                
        E1X    = E1X  + (E2Y *E3Z-E2Z*E3Y)*SUMA
        E1Y    = E1Y  + (E2Z *E3X-E2X*E3Z)*SUMA
        E1Z    = E1Z  + (E2X *E3Y-E2Y*E3X)*SUMA

        SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z  
        SUMA   = ONE/MAX(SQRT(SUMA),EM20)                    
        E1X    = E1X*SUMA                              
        E1Y    = E1Y*SUMA                              
        E1Z    = E1Z*SUMA                              
C  
        E2X    = E3Y * E1Z - E3Z * E1Y
        E2Y    = E3Z * E1X - E3X * E1Z
        E2Z    = E3X * E1Y - E3Y * E1X

C---    projection of V on element plance
        V  = VX*E3X + VY*E3Y + VZ*E3Z
        VX = VX-V*E3X
        VY = VY-V*E3Y
        VZ = VZ-V*E3Z
        V =SQRT(VX*VX+VY*VY+VZ*VZ)
        IF (V < EM10) THEN 
             CALL ANCMSG(MSGID=1641,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO_BLIND_1,
c     .            I1=ID,
c     .            C1=TITR,
     .            I2=IPART(4,IP))       !   
        ENDIF 

        V= MAX(V,EM20)
 
        VX = VX / V
        VY = VY / V
        VZ = VZ / V

C---    Projection of orthotropic axes 

        VR  =  VX*E1X+VY*E1Y+VZ*E1Z
        VS  =  VX*E2X+VY*E2Y+VZ*E2Z

        CP = COS(PHI)
        SP = SIN(PHI)

        AA  = VR*CP - VS*SP
        BB  = VS*CP + VR*SP

        IF (IREP == 1) THEN
           U1X = RX*E1X+RY*E1Y+RZ*E1Z
           U1Y = RX*E2X+RY*E2Y+RZ*E2Z
           U2X = SX*E1X+SY*E1Y+SZ*E1Z
           U2Y = SX*E2X+SY*E2Y+SZ*E2Z
           DET = U1X*U2Y-U1Y*U2X
           W1X = U2Y/DET
           W2Y = U1X/DET
           W1Y = -U1Y/DET
           W2X = -U2X/DET

           D1 = AA
           D2 = BB

           AA = W1X*D1 + W2X*D2
           BB = W1Y*D1 + W2Y*D2
           S = SQRT(AA**2 + BB**2)
           AA = AA/S
           BB = BB/S
        ENDIF

        DIR_FRICM(1,I) = AA
        DIR_FRICM(2,I) = BB

C-----------
      RETURN
      END
