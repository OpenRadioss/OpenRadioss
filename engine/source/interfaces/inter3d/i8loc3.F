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
Chd|  I8LOC3                        source/interfaces/inter3d/i8loc3.F
Chd|-- called by -----------
Chd|        INTVO8                        source/interfaces/inter3d/intvo8.F
Chd|-- calls ---------------
Chd|        INT8_MOD                      ../common_source/modules/interfaces/int8_mod.F
Chd|====================================================================
      SUBROUTINE I8LOC3(X,IRECT,LMSR,MSR,NSV,ILOC,NSEG,
     .                  XI,YI,ZI,XFACE,ITAB,DISTANCE,
     .                  IFLINEAR,DISTLIN,NSN)

C-----------------------------------------------
C   I n f o r m a t i o n s            
C-----------------------------------------------
C   This routine computes the local  
C   ILOCS (i.e. main nodes on the SPMD domain 
C   of each secnd).
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

      USE INT8_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IRECT(4,*), LMSR(*), MSR(*), NSV(*), ILOC(*), NSEG(*)
      INTEGER :: ITAB(*)
      INTEGER  , INTENT(IN) :: IFLINEAR,NSN
C     REAL
      my_real
     .   X(3,*),DISTANCE(*),
     .   XI(*), YI(*), ZI(*), XFACE(*)
      my_real  , INTENT(INOUT) :: DISTLIN(NSN)
      TYPE(INT8_STRUCT_) :: T8
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, IG, JL, JLNEW, LL2, LL1, LL, LG, J, K, M, N, JG,
     .   KG, MG, NG, I1
C     REAL
      my_real
     .   GMS, CMS,
     .   DMS, EMS, FMS
C-----------------------------------------------
      DO 100 I=LFT,LLT
      XFACE(I)=ONE
      IL=I+NFT
      IG=NSV(IL)
      XI(I)=X(1,IG)
      YI(I)=X(2,IG)
      ZI(I)=X(3,IG)
 100  CONTINUE
C
      DO 130 I=LFT,LLT
      IF(ILOC(i) >  0) THEN

        IL=I+NFT
        IG=NSV(IL)
        JL=ILOC(IL)
        JLNEW=JL
        LL2=NSEG(JL+1)-1
        LL1=NSEG(JL)
        GMS=1.E30

        DO 120 LL=LL1,LL2
        LG=LMSR(LL)
        J=IRECT(1,LG)
        K=IRECT(2,LG)
        M=IRECT(3,LG)
        N=IRECT(4,LG)
        JG=MSR(J)
        KG=MSR(K)
        MG=MSR(M)
        NG=MSR(N)
        
        CMS=(XI(I)-X(1,JG))**2+(YI(I)-X(2,JG))**2+(ZI(I)-X(3,JG))**2
        DMS=(XI(I)-X(1,KG))**2+(YI(I)-X(2,KG))**2+(ZI(I)-X(3,KG))**2
        EMS=(XI(I)-X(1,MG))**2+(YI(I)-X(2,MG))**2+(ZI(I)-X(3,MG))**2
        FMS=(XI(I)-X(1,NG))**2+(YI(I)-X(2,NG))**2+(ZI(I)-X(3,NG))**2

        IF(CMS < GMS .OR.
     .    (CMS == GMS .AND. ITAB(MSR(JLNEW))>ITAB(MSR(J)))) THEN
         GMS=CMS
         JLNEW=J
        ENDIF
        IF(DMS<GMS .OR. 
     .     (DMS==GMS .AND. ITAB(MSR(JLNEW))>ITAB(MSR(K))))  THEN
         GMS=DMS
         JLNEW=K
        ENDIF
        IF(EMS<GMS .OR. 
     .    (EMS==GMS .AND. ITAB(MSR(JLNEW))>ITAB(MSR(M)) )) THEN
         GMS=EMS
         JLNEW=M
        ENDIF
        IF(FMS < GMS .OR.
     .    (FMS==GMS .AND. ITAB(MSR(JLNEW))>ITAB(MSR(N))) ) THEN
         GMS=FMS
         JLNEW=N
        ENDIF

 120    CONTINUE
        DISTANCE(IL)=GMS
        ILOC(IL)=JLNEW
      ENDIF
 130  CONTINUE
C
C----Compute curvilinear distance for linear force computation ----

      IF(IFLINEAR == 1 ) THEN
        DO  I=LFT,LLT
           IL=I+NFT
           IG=NSV(IL)
           IF(IL > 1) THEN
              I1=NSV(IL-1)
              DISTLIN(IL) = DISTLIN(IL-1)+
     .                  SQRT((X(1,IG) - X(1,I1))**2 + 
     .                       (X(2,IG) - X(2,I1))**2 +
     .                       (X(3,IG) - X(3,I1))**2 )
           ENDIF
         ENDDO
      ENDIF
C

      RETURN
      END
