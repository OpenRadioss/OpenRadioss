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
Chd|  BUILD_CNEL                    source/model/mesh/build_cnel.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BUILD_CNEL(
     2   IXS        ,IXQ        ,IXC        ,IXT      ,IXP      ,
     3   IXR        ,IXTG       ,IXUR       ,IXS10    ,IXS20    ,
     4   IXS16      ,IXTG1      ,IGEO       ,KNOD2ELS ,KNOD2ELC ,
     5   KNOD2ELTG  ,NOD2ELS    ,NOD2ELC    ,NOD2ELTG ,CNEL     ,
     6   ADDCNEL    ,KXX        ,IXX        ,X        ,LELX     ,
     7   IXIG3D     ,KXIG3D     ,KNOD2ELIG3D,NOD2ELIG3D,KNOD2ELQ,
     8   NOD2ELQ   )
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "rnur_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*), IXUR(NIXUR,*),
     .        IXS10(6,*),IXS20(12,*),IXS16(8,*),IXTG1(4,*),
     .        IGEO(NPROPGI,*),KNOD2ELS(*),KNOD2ELC(*),KNOD2ELTG(*),
     .        NOD2ELS(*),NOD2ELC(*),NOD2ELTG(*),ADDCNEL(*),CNEL(*),
     .        KXX(NIXX,*),IXX(*),KXIG3D(NIXIG3D,*),IXIG3D(*),
     .        KNOD2ELIG3D(*),NOD2ELIG3D(*),KNOD2ELQ(*),NOD2ELQ(*)

      my_real
     .   X(3,*),LELX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, PX, PY, PZ
C-----------------------------------------------
C
C Pre construction ADRESSES
C    
      DO  K=2,9
        DO  I=1,NUMELS
          N = IXS(K,I)
          IF(N/=0) KNOD2ELS(N) = KNOD2ELS(N) + 1
        END DO
      END DO
C
      DO K=1,6
        DO I=1,NUMELS10
          N = IXS10(K,I)
          IF(N/=0) KNOD2ELS(N) = KNOD2ELS(N) + 1
        END DO
      END DO
C
      DO K=1,12
        DO I=1,NUMELS20
          N = IXS20(K,I)
          IF(N/=0) KNOD2ELS(N) = KNOD2ELS(N) + 1
        END DO
      END DO
C
      DO K=1,8
        DO I=1,NUMELS16
          N = IXS16(K,I)
          IF(N/=0) KNOD2ELS(N) = KNOD2ELS(N) + 1
        END DO
      END DO
C
      DO I=1,NUMNOD
        KNOD2ELS(I+1) = KNOD2ELS(I+1) + KNOD2ELS(I)
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2ELS(N+1)=KNOD2ELS(N)
      END DO
      KNOD2ELS(1)=0
C
      DO K=2,5
        DO I=1,NUMELC
          N = IXC(K,I)
          KNOD2ELC(N) = KNOD2ELC(N) + 1
        END DO
      END DO
C
      DO I=1,NUMNOD
        KNOD2ELC(I+1) = KNOD2ELC(I+1) + KNOD2ELC(I)
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2ELC(N+1)=KNOD2ELC(N)
      END DO
      KNOD2ELC(1)=0
C
      DO K=2,4
        DO I=1,NUMELTG
          N = IXTG(K,I)
          KNOD2ELTG(N) = KNOD2ELTG(N) + 1
        END DO
      END DO
C
      DO K=1,3
        DO I=1,NUMELTG6
          N = IXTG1(K,I)
          IF (N/=0) KNOD2ELTG(N) = KNOD2ELTG(N) + 1
        END DO
      END DO
C
      DO I=1,NUMNOD
        KNOD2ELTG(I+1) = KNOD2ELTG(I+1) + KNOD2ELTG(I)
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2ELTG(N+1)=KNOD2ELTG(N)
      END DO
      KNOD2ELTG(1)=0
C
      DO I=1,NUMELIG3D
       PX = IGEO(41,KXIG3D(2,I))
       PY = IGEO(42,KXIG3D(2,I))
       PZ = IGEO(43,KXIG3D(2,I))
        DO K=1,PX*PY*PZ
          N = IXIG3D(KXIG3D(4,I)+K-1)
          KNOD2ELIG3D(N) = KNOD2ELIG3D(N) + 1
        END DO
      END DO
C
      DO I=1,NUMNOD
        KNOD2ELIG3D(I+1) = KNOD2ELIG3D(I+1) + KNOD2ELIG3D(I)
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2ELIG3D(N+1)=KNOD2ELIG3D(N)
      END DO
      KNOD2ELIG3D(1)=0

C------------Quad elements nodes ----------
C
      DO K=2,5
        DO I=1,NUMELQ
          N = IXQ(K,I)
          KNOD2ELQ(N) = KNOD2ELQ(N) + 1
        END DO
      END DO
C
      DO I=1,NUMNOD
        KNOD2ELQ(I+1) = KNOD2ELQ(I+1) + KNOD2ELQ(I)
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2ELQ(N+1)=KNOD2ELQ(N)
      END DO
      KNOD2ELQ(1)=0
C-----------------------------------------------
C      DO K=2,5
C        DO I=1,NUMELQ
C          N = IXQ(K,I)
C          ADDCNEL(N) = ADDCNEL(N) + 1
C        END DO
C      END DO
C
      DO K=2,3
        DO I=1,NUMELT
          N = IXT(K,I)
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO

      DO K=2,3
        DO I=1,NUMELP
          N = IXP(K,I)
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO

      DO K=2,3
        DO I=1,NUMELR
          N = IXR(K,I)
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO

      DO I=1,NUMELX
        DO K=1,KXX(3,I)-1
          N = IXX(KXX(4,I)+K)
          ADDCNEL(N) = ADDCNEL(N) + 1
        END DO
      END DO

      DO I=1,NUMNOD
        ADDCNEL(I+1) = ADDCNEL(I+1) + ADDCNEL(I)
      END DO

      DO N=NUMNOD,1,-1
        ADDCNEL(N+1)=ADDCNEL(N)
      END DO
      ADDCNEL(1)=0
c traitement a part du 3eme noeud optionnel sauf type 12
c      DO I=1,NUMELR
c        N = IXR(4,I)
c        IF(IGEO(11,IXR(1,I))==12)  ADDCNEL(N) = ADDCNEL(N) + 1
c      END DO

c      DO K=2,5
c        DO I=1,NUMELUR
c          N = IXUR(K,I) + 1
c          ADDCNEL(N) = ADDCNEL(N) + 1
c        END DO
c      END DO
C
C-----------------------------------------------
C
C Construction de la matrice Nod -> Solid elt
C
      DO  K=2,9
        DO  I=1,NUMELS
          N = IXS(K,I)
          KNOD2ELS(N) = KNOD2ELS(N) + 1
          IF(N/=0) NOD2ELS(KNOD2ELS(N)) = I
        END DO
      END DO
C
      DO K=1,6
        DO I=1,NUMELS10
          N = IXS10(K,I)
          IF (N/=0) THEN
            KNOD2ELS(N) = KNOD2ELS(N) + 1
            NOD2ELS(KNOD2ELS(N)) = NUMELS8+I
          END IF
        END DO
      END DO
C
      DO K=1,12
        DO I=1,NUMELS20
          N = IXS20(K,I)
          IF (N/=0) THEN
            KNOD2ELS(N) = KNOD2ELS(N) + 1
            NOD2ELS(KNOD2ELS(N)) = NUMELS10+NUMELS8+I
          END IF
        END DO
      END DO
C
      DO K=1,8
        DO I=1,NUMELS16
          N = IXS16(K,I)
          IF (N/=0) THEN
            KNOD2ELS(N) = KNOD2ELS(N) + 1
            NOD2ELS(KNOD2ELS(N)) = NUMELS20+NUMELS10+NUMELS8+I
          END IF
        END DO
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2ELS(N+1)=KNOD2ELS(N)
      END DO
      KNOD2ELS(1)=0
C
C Construction de la matrice Nod -> Shell elt
C
      DO K=2,5
        DO I=1,NUMELC
          N = IXC(K,I)
          KNOD2ELC(N) = KNOD2ELC(N) + 1
          NOD2ELC(KNOD2ELC(N)) = I
        END DO
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2ELC(N+1)=KNOD2ELC(N)
      END DO
      KNOD2ELC(1)=0
C
C Construction de la matrice Nod -> 3-node Shell elt
C
      DO K=2,4
        DO I=1,NUMELTG
          N = IXTG(K,I)
          KNOD2ELTG(N) = KNOD2ELTG(N) + 1
          NOD2ELTG(KNOD2ELTG(N)) = I
        END DO
      END DO
C
      DO K=1,3
        DO I=1,NUMELTG6
          N = IXTG1(K,I)
          IF (N/=0) THEN
            KNOD2ELTG(N) = KNOD2ELTG(N) + 1
            NOD2ELTG(KNOD2ELTG(N)) = NUMELTG-NUMELTG6+I
          END IF
        END DO
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2ELTG(N+1)=KNOD2ELTG(N)
      END DO
      KNOD2ELTG(1)=0
C
C      DO K=2,5
C        DO I=1,NUMELQ
C          N = IXQ(K,I)
C          ADDCNEL(N) = ADDCNEL(N) + 1
C          CNEL(ADDCNEL(N)) = OFFSET_Q+I
C        END DO
C      END DO

      DO K=2,3
        DO I=1,NUMELT
          N = IXT(K,I)
          ADDCNEL(N) = ADDCNEL(N) + 1
          CNEL(ADDCNEL(N)) = I
        END DO
      END DO

      DO K=2,3
        DO I=1,NUMELP
          N = IXP(K,I)
          ADDCNEL(N) = ADDCNEL(N) + 1
          CNEL(ADDCNEL(N)) = NUMELT+I
        END DO
      END DO

      DO K=2,3
        DO I=1,NUMELR
          N = IXR(K,I)
          ADDCNEL(N) = ADDCNEL(N) + 1
          CNEL(ADDCNEL(N)) = NUMELT+NUMELP+I
        END DO
      END DO

      DO I=1,NUMELX
        DO K=1,KXX(3,I)-1
          N = IXX(KXX(4,I)+K)
          ADDCNEL(N) = ADDCNEL(N) + 1
          CNEL(ADDCNEL(N)) = NUMELT+NUMELP+NUMELR+I
        END DO
      END DO

      DO N=NUMNOD,1,-1
        ADDCNEL(N+1)=ADDCNEL(N)
      END DO
      ADDCNEL(1)=0
c traitement a part du 3eme noeud optionnel sauf type 12
c      DO I=1,NUMELR
c        N = IXR(4,I)
c        IF(IGEO(11,IXR(1,I))==12)  THEN
c          ADDCNEL(N) = ADDCNEL(N) + 1
c          CNEL(ADDCNEL(N)) = OFFSET_R+I
c        END IF
c      END DO

c      DO K=2,5
c        DO I=1,NUMELUR
c          N = IXUR(K,I) + 1
c          ADDCNEL(N) = ADDCNEL(N) + 1
c          CNEL(ADDCNEL(N)) = NUMELT+NUMELP+NUMELR+I
c        END DO
c      END DO
C

c fill LELX (use in I11STI3 and I20STI3E)
      IF(NUMELX /= 0)THEN
	LELX(1:NUMELX) = ZERO
	DO I=1,NUMELX
	  DO J=KXX(4,I)+1,KXX(3,I)+KXX(4,I)-1
	    LELX(I) = LELX(I) + 
     .       SQRT((X(1,IXX(J))-X(1,IXX(J-1)))**2
     .  	 +(X(2,IXX(J))-X(2,IXX(J-1)))**2
     .  	 +(X(3,IXX(J))-X(3,IXX(J-1)))**2)
	  ENDDO
	ENDDO
      ENDIF
C
      DO I=1,NUMELIG3D
       PX = IGEO(41,KXIG3D(2,I))
       PY = IGEO(42,KXIG3D(2,I))
       PZ = IGEO(43,KXIG3D(2,I))
        DO K=1,PX*PY*PZ
          N = IXIG3D(KXIG3D(4,I)+K-1)
          KNOD2ELIG3D(N) = KNOD2ELIG3D(N) + 1
          NOD2ELIG3D(KNOD2ELIG3D(N)) = I
        END DO
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2ELIG3D(N+1)=KNOD2ELIG3D(N)
      END DO
      KNOD2ELIG3D(1)=0

C------------Quad elements nodes  ----------
C Construction de la matrice Nod -> Quad elt
C
      DO K=2,5
        DO I=1,NUMELQ
          N = IXQ(K,I)
          KNOD2ELQ(N) = KNOD2ELQ(N) + 1
          NOD2ELQ(KNOD2ELQ(N)) = I
        END DO
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2ELQ(N+1)=KNOD2ELQ(N)
      END DO
      KNOD2ELQ(1)=0
C------------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  PREPARE_INT25                 source/model/mesh/build_cnel.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE PREPARE_INT25(
     1   INTBUF_TAB , IPARI, INTERCEP, NRTMT_25)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "rnur_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), NRTMT_25
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIN, NTY, NRTM, NADMSR, NSN, 
     .        NRTLM_L(NSPMD), K, P, N, IERROR
      INTEGER, ALLOCATABLE, DIMENSION(:) :: IRTLM_L
C-----------------------------------------------
C
C Pre calcul variables specifiques TYPE25
C   
      NUMNOR   = 0
      NINTER25 = 0
      NSNT25  = 0

      NRTMX25 = 0

      NRTMT_25 = 0

      DO  NIN=1,NINTER
        NTY=IPARI(7,NIN)
        IF(NTY/=25) CYCLE
 
        NINTER25 = NINTER25 + 1
 
        NRTM  =IPARI(4,NIN)
        NADMSR=IPARI(67,NIN)

        NUMNOR   = NUMNOR + NADMSR
        NRTMT_25 = NRTMT_25 + NRTM

        NSN    = IPARI(5,NIN)
        NSNT25 = NSNT25 + NSN

        NRTMX25=MAX(NRTMX25,NRTM)
      END DO

      DO  NIN=1,NINTER
        NTY=IPARI(7,NIN)
        IF(NTY/=25) CYCLE
C
        NRTM  = IPARI(4,NIN)
        NSN   = IPARI(5,NIN)

        ALLOCATE(IRTLM_L(NRTM),STAT=IERROR)
        IF (IERROR /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='Prepare data related to /INTER/TYPE25')

        NRTLM_L(1:NSPMD)=0
        DO K=1,NRTM
          P = INTERCEP(1,NIN)%P(K)          
          NRTLM_L(P)=NRTLM_L(P)+1
          IRTLM_L(K)=NRTLM_L(P)
        ENDDO

        DO N=1,NSN
C
C         IRTLM(3,-) <=> n° de segment local sur IRTLM(4,-)
          K = INTBUF_TAB(NIN)%IRTLM(4*(N-1)+3)
          IF(K /= 0)THEN
            INTBUF_TAB(NIN)%IRTLM(4*(N-1)+3)=IRTLM_L(K)
            INTBUF_TAB(NIN)%IRTLM(4*(N-1)+4)=INTERCEP(1,NIN)%P(K) 
          END IF
        END DO

        DEALLOCATE(IRTLM_L)
      END DO

      RETURN
      END 
Chd|====================================================================
Chd|  BUILD_CSRECT                  source/model/mesh/build_cnel.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE BUILD_CSRECT(
     1   INTBUF_TAB , IPARI, CSRECT   ,ADDCSRECT    )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD 
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "rnur_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      INTEGER IPARI(NPARI,*), ADDCSRECT(0:NUMNOR),CSRECT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIN, NTY, I, N, I1, I2, I3, I4, NRTM, NADMSR, ISHIFT
C-----------------------------------------------
C
C Pre construction ADRESSES
C   
      ADDCSRECT(0) = 0

      ISHIFT=0 
      DO  NIN=1,NINTER
        NTY=IPARI(7,NIN)
        IF(NTY/=25) CYCLE
 
        NRTM  =IPARI(4,NIN)
        NADMSR=IPARI(67,NIN)
        DO N=1,NRTM
          I1=ISHIFT+INTBUF_TAB(NIN)%ADMSR(4*(N-1)+1)
          I2=ISHIFT+INTBUF_TAB(NIN)%ADMSR(4*(N-1)+2)
          I3=ISHIFT+INTBUF_TAB(NIN)%ADMSR(4*(N-1)+3)
          I4=ISHIFT+INTBUF_TAB(NIN)%ADMSR(4*(N-1)+4)
          ADDCSRECT(I1)=ADDCSRECT(I1)+1
          ADDCSRECT(I2)=ADDCSRECT(I2)+1
          ADDCSRECT(I3)=ADDCSRECT(I3)+1
          IF(I4/=I3) ADDCSRECT(I4)=ADDCSRECT(I4)+1
        END DO
C
        DO I=ISHIFT,ISHIFT+NADMSR-1
          ADDCSRECT(I+1) = ADDCSRECT(I+1) + ADDCSRECT(I)
        END DO
C
        DO N=ISHIFT+NADMSR-1,ISHIFT,-1
          ADDCSRECT(N+1)=ADDCSRECT(N)
        END DO
C
        DO N=1,NRTM
          I1=ISHIFT+INTBUF_TAB(NIN)%ADMSR(4*(N-1)+1)
          I2=ISHIFT+INTBUF_TAB(NIN)%ADMSR(4*(N-1)+2)
          I3=ISHIFT+INTBUF_TAB(NIN)%ADMSR(4*(N-1)+3)
          I4=ISHIFT+INTBUF_TAB(NIN)%ADMSR(4*(N-1)+4)
          ADDCSRECT(I1) = ADDCSRECT(I1) + 1
          CSRECT(ADDCSRECT(I1)) = N
          ADDCSRECT(I2) = ADDCSRECT(I2) + 1
          CSRECT(ADDCSRECT(I2)) = N
          ADDCSRECT(I3) = ADDCSRECT(I3) + 1
          CSRECT(ADDCSRECT(I3)) = N
          IF(I4/=I3)THEN
            ADDCSRECT(I4) = ADDCSRECT(I4) + 1
            CSRECT(ADDCSRECT(I4)) = N
          END IF
        END DO
C
        DO N=ISHIFT+NADMSR-1,ISHIFT
          ADDCSRECT(N+1)=ADDCSRECT(N)
        END DO
C
        ISHIFT=ISHIFT+NADMSR
 
      END DO
C-----------------------------------------------
      DO I=0,NUMNOR
        ADDCSRECT(I)=ADDCSRECT(I)+1
      END DO
C-----------------------------------------------
      RETURN
      END
