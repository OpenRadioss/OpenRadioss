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
Chd|  I11ASS3                       source/interfaces/int11/i11ass3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        I11ASS0                       source/interfaces/int11/i11ass3.F
Chd|        I11ASS05                      source/interfaces/int11/i11ass3.F
Chd|        I11ASS2                       source/interfaces/int11/i11ass3.F
Chd|        I11ASS25                      source/interfaces/int11/i11ass3.F
Chd|        I11SMS2                       source/interfaces/int11/i11sms2.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11ASS3(
     1                  JLT    ,A      ,NIN     ,NOINT  ,CS_LOC   ,
     2                  STIFN  ,STIF   ,FSKYI   ,ISKY   ,FCONT    ,
     3                  HS1    ,HS2     ,HM1    ,HM2    ,N1       ,
     4                  N2     ,M1      ,M2     ,NISKYFI,ISECIN   , 
     5                  NSTRF  ,SECFCUM,VISCN   ,NRTS   ,ISKYI_SMS,
     6                  NSMS ,ICONTACT,MSKYI_SMS,FX1    ,FY1      ,
     7                  FZ1     ,FX2    ,FY2    ,FZ2    ,FX3      ,
     8                  FY3     ,FZ3    ,FX4    ,FY4    ,FZ4      ,
     9                  K1      ,K2     ,K3     ,K4     ,C1       ,
     A                  C2      ,C3     ,C4     ,INTTH  ,PHIS1    ,
     B                  PHIS2   ,PHIM1  ,PHIM2  ,FTHE  ,FTHESKYI  ,
     C                  CONDINTS1,CONDINTS2,CONDINTM1,CONDINTM2,CONDN,
     D                  CONDNSKYI,JTASK ,H3D_DATA) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NRTS,NISKYFI,NIN,INTTH
      INTEGER  ISKY(*),
     .        NOINT,ISECIN, NSTRF(*)
      INTEGER N1(MVSIZ), N2(MVSIZ), M1(MVSIZ), M2(MVSIZ),
     .        CS_LOC(MVSIZ),NSMS(MVSIZ),ICONTACT(*),ISKYI_SMS(*),JTASK
      my_real
     .   A(3,*), FCONT(3,*),
     .   STIFN(*),FSKYI(LSKYI,NFSKYI),
     .   DTMI0
      my_real
     .   HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), STIF(MVSIZ),
     .   SECFCUM(7,NUMNOD,NSECT), VISCN(*),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   PHIS1(MVSIZ),PHIS2(MVSIZ),FTHE(*),FTHESKYI(*),CONDN(*),
     .   CONDINTS1(MVSIZ),CONDINTS2(MVSIZ),CONDINTM1(MVSIZ),CONDINTM2(MVSIZ),
     .   CONDNSKYI(*),MSKYI_SMS(*),PHIM1(MVSIZ),PHIM2(MVSIZ)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, J , K0,NBINTER,K1S,K, NI
      INTEGER NISKYL,NISKYL1,IDTM
      my_real
     .   DTMI(MVSIZ)

C---------------------------------
      IF(IDTMINS==2.OR.IDTMINS_INT/=0)
     .  CALL I11SMS2(JLT   ,CS_LOC ,N1    ,N2     ,M1     ,
     2               M2    ,HS1    ,HS2   ,HM1    ,HM2    ,
     3               STIF  ,NIN    ,NOINT ,MSKYI_SMS ,ISKYI_SMS,
     4               NSMS  ,K1     ,K2    ,K3     ,K4     ,
     5               C1    ,C2     ,C3    ,C4     ,NRTS   )
C
      IF(IDTMINS_INT/=0)THEN
        STIF(1:JLT)=ZERO
      END IF
C
      IF(IPARIT==0)THEN
       IF(KDTINT==0)THEN
         CALL I11ASS0(JLT  ,CS_LOC,N1   ,N2   ,M1   ,
     2                M2   ,HS1   ,HS2  ,HM1  ,HM2  ,
     3                FX1  ,FY1   ,FZ1  ,FX2  ,FY2  ,
     4                FZ2  ,FX3   ,FY3  ,FZ3  ,FX4  ,
     5                FY4  ,FZ4   ,A    ,STIFN,STIF ,
     6                NRTS ,NIN   ,INTTH,PHIS1,PHIS2,
     7                PHIM1,PHIM2 ,FTHE ,CONDINTS1  ,
     8                CONDINTS2,CONDINTM1,CONDINTM2,CONDN ,
     9                JTASK )             
       ELSE
         CALL I11ASS05(JLT   ,CS_LOC,N1    ,N2   ,M1   ,
     2                 M2    ,HS1   ,HS2   ,HM1  ,HM2  ,
     3                 FX1   ,FY1   ,FZ1   ,FX2  ,FY2  ,
     4                 FZ2   ,FX3   ,FY3   ,FZ3  ,FX4  ,
     5                 FY4   ,FZ4   ,A     ,STIFN,NRTS ,
     6                 K1    ,K2    ,K3    ,K4   ,C1   ,
     7                 C2    ,C3    ,C4    ,VISCN,NIN  ,
     8                 INTTH ,PHIS1 ,PHIS2 ,PHIM1,PHIM2,
     9                 FTHE  ,CONDINTS1,CONDINTS2,CONDINTM1,
     A                 CONDINTM2,CONDN,JTASK)
       END IF
      ELSE
       IF(KDTINT==0)THEN
         CALL I11ASS2(JLT   ,CS_LOC ,N1    ,N2    ,M1    ,
     2                M2    ,HS1    ,HS2   ,HM1   ,HM2   ,
     3                FX1   ,FY1    ,FZ1   ,FX2   ,FY2   ,
     4                FZ2   ,FX3    ,FY3   ,FZ3   ,FX4   ,
     5                FY4   ,FZ4    ,FSKYI ,ISKY  ,NISKYFI,
     6                STIF  ,NRTS   ,NIN   , NOINT,INTTH ,
     7                PHIS1 ,PHIS2  ,PHIM1 ,PHIM2 ,FTHESKYI,
     8                CONDINTS1,CONDINTS2,CONDINTM1,CONDINTM2,
     A                CONDNSKYI)
       ELSE
         CALL I11ASS25(JLT  ,CS_LOC ,N1    ,N2     ,M1    ,
     2                 M2   ,HS1    ,HS2   ,HM1    ,HM2   ,
     3                 FX1  ,FY1    ,FZ1   ,FX2    ,FY2   ,
     4                 FZ2  ,FX3    ,FY3   ,FZ3    ,FX4   ,
     5                 FY4  ,FZ4    ,ISKY  ,NISKYFI,NRTS  ,
     6                 K1   ,K2     ,K3    ,K4     ,C1    ,
     7                 C2   ,C3     ,C4    ,NIN    , NOINT,
     8                 INTTH ,PHIS1 ,PHIS2 ,PHIM1  ,PHIM2 ,
     9                 FTHESKYI,CONDINTS1,CONDINTS2,CONDINTM1,
     A                 CONDINTM2,CONDNSKYI)
       END IF
      END IF            
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0)THEN
         IF (INCONV==1) THEN
#include "lockon.inc"
           DO I=1,JLT
            IF(CS_LOC(I)<=NRTS) THEN
              FCONT(1,N1(I)) =FCONT(1,N1(I)) + FX1(I)
              FCONT(2,N1(I)) =FCONT(2,N1(I)) + FY1(I)
              FCONT(3,N1(I)) =FCONT(3,N1(I)) + FZ1(I)
              FCONT(1,N2(I)) =FCONT(1,N2(I)) + FX2(I)
              FCONT(2,N2(I)) =FCONT(2,N2(I)) + FY2(I)
              FCONT(3,N2(I)) =FCONT(3,N2(I)) + FZ2(I)
            END IF
            FCONT(1,M1(I)) =FCONT(1,M1(I)) + FX3(I)
            FCONT(2,M1(I)) =FCONT(2,M1(I)) + FY3(I)
            FCONT(3,M1(I)) =FCONT(3,M1(I)) + FZ3(I)
            FCONT(1,M2(I)) =FCONT(1,M2(I)) + FX4(I)
            FCONT(2,M2(I)) =FCONT(2,M2(I)) + FY4(I)
            FCONT(3,M2(I)) =FCONT(3,M2(I)) + FZ4(I)
           ENDDO
#include "lockoff.inc"
         END IF !(INCONV==1) THEN
      ENDIF
C
      IF(ISECIN>0.AND.INCONV==1)THEN
       K0=NSTRF(25)
       IF(NSTRF(1)+NSTRF(2)/=0)THEN
       DO I=1,NSECT
        NBINTER=NSTRF(K0+14)
        K1S=K0+30
        DO J=1,NBINTER
         IF(NSTRF(K1S)==NOINT)THEN
           IF(ISECUT/=0)THEN
#include "lockon.inc"
             DO K=1,JLT
              IF(CS_LOC(K)<=NRTS) THEN
               IF(SECFCUM(4,N1(K),I)==1.)THEN
                SECFCUM(1,N1(K),I)=SECFCUM(1,N1(K),I)-FX1(K)
                SECFCUM(2,N1(K),I)=SECFCUM(2,N1(K),I)-FY1(K)
                SECFCUM(3,N1(K),I)=SECFCUM(3,N1(K),I)-FZ1(K)
               ENDIF
               IF(SECFCUM(4,N2(K),I)==1.)THEN
                SECFCUM(1,N2(K),I)=SECFCUM(1,N2(K),I)-FX2(K)
                SECFCUM(2,N2(K),I)=SECFCUM(2,N2(K),I)-FY2(K)
                SECFCUM(3,N2(K),I)=SECFCUM(3,N2(K),I)-FZ2(K)
               ENDIF
              END IF
              IF(SECFCUM(4,M1(K),I)==1.)THEN
                SECFCUM(1,M1(K),I)=SECFCUM(1,M1(K),I)-FX3(K)
                SECFCUM(2,M1(K),I)=SECFCUM(2,M1(K),I)-FY3(K)
                SECFCUM(3,M1(K),I)=SECFCUM(3,M1(K),I)-FZ3(K)
              ENDIF
              IF(SECFCUM(4,M2(K),I)==1.)THEN
                SECFCUM(1,M2(K),I)=SECFCUM(1,M2(K),I)-FX4(K)
                SECFCUM(2,M2(K),I)=SECFCUM(2,M2(K),I)-FY4(K)
                SECFCUM(3,M2(K),I)=SECFCUM(3,M2(K),I)-FZ4(K)
              ENDIF
             ENDDO
#include "lockoff.inc"
           ENDIF
C +fsav(section)
         ENDIF
         K1S=K1S+1
        ENDDO
        K0=NSTRF(K0+24)
       ENDDO
       ENDIF
      ENDIF
C
C-----------------------------------------------------     
      IF(IDAMP_RDOF/=0) THEN
        DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
          IF(FX1(I)/=ZERO.OR.FY1(I)/=ZERO.OR.FZ1(I)/=ZERO)THEN
            ICONTACT(N1(I))=1	  
          ENDIF
          IF(FX2(I)/=ZERO.OR.FY2(I)/=ZERO.OR.FZ2(I)/=ZERO)THEN
            ICONTACT(N2(I))=1	    	  
          ENDIF		  
	  ENDIF	
C test modifie pour coherence avec communication SPMD (spmd_i7tools)
          IF(FX3(I)/=ZERO.OR.FY3(I)/=ZERO.OR.FZ3(I)/=ZERO)THEN
            ICONTACT(M1(I))=1	    	  
          ENDIF
          IF(FX4(I)/=ZERO.OR.FY4(I)/=ZERO.OR.FZ4(I)/=ZERO)THEN
            ICONTACT(M2(I))=1	    	  
          ENDIF	  
        ENDDO
      ENDIF
C      
      RETURN
      END
C
Chd|====================================================================
Chd|  I11ASS0                       source/interfaces/int11/i11ass3.F
Chd|-- called by -----------
Chd|        I11ASS3                       source/interfaces/int11/i11ass3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11ASS0(JLT  ,CS_LOC,N1   ,N2   ,M1   ,
     2                   M2   ,HS1   ,HS2  ,HM1  ,HM2  ,
     3                   FX1  ,FY1   ,FZ1  ,FX2  ,FY2  ,
     4                   FZ2  ,FX3   ,FY3  ,FZ3  ,FX4  ,
     5                   FY4  ,FZ4   ,A    ,STIFN,STIF ,
     6                   NRTS ,NIN   ,INTTH ,PHIS1,PHIS2,
     7                   PHIM1,PHIM2 ,FTHE ,CONDINTS1  ,
     8                   CONDINTS2,CONDINTM1,CONDINTM2,CONDN,
     9                   JTASK )  
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
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NRTS, NIN,INTTH,
     +        CS_LOC(*),
     +        N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ),JTASK
      my_real
     .        HS1(MVSIZ),HS2(MVSIZ),HM1(MVSIZ),HM2(MVSIZ),
     .        FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .        FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .        FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .        FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .        PHIS1(MVSIZ),PHIS2(MVSIZ),CONDINTS1(MVSIZ),
     .        CONDINTS2(MVSIZ),PHIM1(MVSIZ),PHIM2(MVSIZ),
     .        CONDINTM1(MVSIZ),CONDINTM2(MVSIZ),
     .        A(3,*), STIFN(*), STIF(*),FTHE(*),CONDN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, NODFI, ISHIFT
C-----------------------------------------------
C
      NODFI = NLSKYFI(NIN)
      ISHIFT = NODFI*(JTASK-1)
C
      IF(INTTH==0)THEN
        DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
            J1=N1(I)
            A(1,J1)=A(1,J1)+FX1(I)
            A(2,J1)=A(2,J1)+FY1(I)
            A(3,J1)=A(3,J1)+FZ1(I)
            STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HS1(I))
C
            J1=N2(I)
            A(1,J1)=A(1,J1)+FX2(I)
            A(2,J1)=A(2,J1)+FY2(I)
            A(3,J1)=A(3,J1)+FZ2(I)
            STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HS2(I))
          ELSE
            J1=N1(I)
            AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)+FX1(I)
            AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)+FY1(I)
            AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)+FZ1(I)
            STNFI(NIN)%P(J1+ISHIFT) = STNFI(NIN)%P(J1+ISHIFT) + STIF(I)*ABS(HS1(I))
C
            J1=N2(I)
            AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)+FX2(I)
            AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)+FY2(I)
            AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)+FZ2(I)
            STNFI(NIN)%P(J1+ISHIFT) = STNFI(NIN)%P(J1+ISHIFT) + STIF(I)*ABS(HS2(I))
          END IF
        END DO
      ELSE
       IF(NODADT_THERM == 1 ) THEN  
        DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
            J1=N1(I)
            A(1,J1)=A(1,J1)+FX1(I)
            A(2,J1)=A(2,J1)+FY1(I)
            A(3,J1)=A(3,J1)+FZ1(I)
            STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HS1(I))
            FTHE(J1)=FTHE(J1)+PHIS1(I)
            CONDN(J1) = CONDN(J1) + CONDINTS1(I)
C
            J1=N2(I)
            A(1,J1)=A(1,J1)+FX2(I)
            A(2,J1)=A(2,J1)+FY2(I)
            A(3,J1)=A(3,J1)+FZ2(I)
            STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HS2(I))
            FTHE(J1)=FTHE(J1)+PHIS2(I)
            CONDN(J1) = CONDN(J1) + CONDINTS2(I)
          ELSE
            J1=N1(I)
            AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)+FX1(I)
            AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)+FY1(I)
            AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)+FZ1(I)
            STNFI(NIN)%P(J1+ISHIFT) = STNFI(NIN)%P(J1+ISHIFT) + STIF(I)*ABS(HS1(I))
            FTHEFI(NIN)%P(J1+ISHIFT)= FTHEFI(NIN)%P(J1+ISHIFT) + PHIS1(I)
            CONDNFI(NIN)%P(J1+ISHIFT)=CONDNFI(NIN)%P(J1+ISHIFT) + CONDINTS1(I)
C
            J1=N2(I)
            AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)+FX2(I)
            AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)+FY2(I)
            AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)+FZ2(I)
            STNFI(NIN)%P(J1+ISHIFT) = STNFI(NIN)%P(J1+ISHIFT) + STIF(I)*ABS(HS2(I))
            FTHEFI(NIN)%P(J1+ISHIFT)= FTHEFI(NIN)%P(J1+ISHIFT) + PHIS2(I)
            CONDNFI(NIN)%P(J1+ISHIFT)=CONDNFI(NIN)%P(J1+ISHIFT) + CONDINTS2(I)
          END IF
        END DO
       ELSE
        DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
            J1=N1(I)
            A(1,J1)=A(1,J1)+FX1(I)
            A(2,J1)=A(2,J1)+FY1(I)
            A(3,J1)=A(3,J1)+FZ1(I)
            STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HS1(I))
            FTHE(J1)=FTHE(J1)+PHIS1(I)
C
            J1=N2(I)
            A(1,J1)=A(1,J1)+FX2(I)
            A(2,J1)=A(2,J1)+FY2(I)
            A(3,J1)=A(3,J1)+FZ2(I)
            STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HS2(I))
            FTHE(J1)=FTHE(J1)+PHIS2(I)
          ELSE
            J1=N1(I)
            AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)+FX1(I)
            AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)+FY1(I)
            AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)+FZ1(I)
            STNFI(NIN)%P(J1+ISHIFT) = STNFI(NIN)%P(J1+ISHIFT) + STIF(I)*ABS(HS1(I))
            FTHEFI(NIN)%P(J1+ISHIFT)= FTHEFI(NIN)%P(J1+ISHIFT) + PHIS1(I)
C
            J1=N2(I)
            AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)+FX2(I)
            AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)+FY2(I)
            AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)+FZ2(I)
            STNFI(NIN)%P(J1+ISHIFT) = STNFI(NIN)%P(J1+ISHIFT) + STIF(I)*ABS(HS2(I))
            FTHEFI(NIN)%P(J1+ISHIFT)= FTHEFI(NIN)%P(J1+ISHIFT) + PHIS2(I)
          END IF
        END DO
       ENDIF
      ENDIF
C
      IF(INTTH==0)THEN
       DO I=1,JLT
        J1=M1(I)
        A(1,J1)=A(1,J1)+FX3(I)
        A(2,J1)=A(2,J1)+FY3(I)
        A(3,J1)=A(3,J1)+FZ3(I)
        STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HM1(I))
C
        J1=M2(I)
        A(1,J1)=A(1,J1)+FX4(I)
        A(2,J1)=A(2,J1)+FY4(I)
        A(3,J1)=A(3,J1)+FZ4(I)
        STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HM2(I))

       ENDDO
C
      ELSE
       IF(NODADT_THERM == 1 ) THEN        
        DO I=1,JLT
         J1=M1(I)
         A(1,J1)=A(1,J1)+FX3(I)
         A(2,J1)=A(2,J1)+FY3(I)
         A(3,J1)=A(3,J1)+FZ3(I)
         STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HM1(I))
         FTHE(J1)  = FTHE(J1) + PHIM1(I)
         CONDN(J1) = CONDN(J1) + CONDINTM1(I)
C
         J1=M2(I)
         A(1,J1)=A(1,J1)+FX4(I)
         A(2,J1)=A(2,J1)+FY4(I)
         A(3,J1)=A(3,J1)+FZ4(I)
         STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HM2(I))
         FTHE(J1)  = FTHE(J1) + PHIM2(I)
         CONDN(J1) = CONDN(J1) + CONDINTM2(I)
        ENDDO
C
       ELSE
C
        DO I=1,JLT
         J1=M1(I)
         A(1,J1)=A(1,J1)+FX3(I)
         A(2,J1)=A(2,J1)+FY3(I)
         A(3,J1)=A(3,J1)+FZ3(I)
         STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HM1(I))
         FTHE(J1)  = FTHE(J1) + PHIM1(I)
C
         J1=M2(I)
         A(1,J1)=A(1,J1)+FX4(I)
         A(2,J1)=A(2,J1)+FY4(I)
         A(3,J1)=A(3,J1)+FZ4(I)
         STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HM2(I))
         FTHE(J1)  = FTHE(J1) + PHIM2(I)
        ENDDO
       ENDIF
      ENDIF
      RETURN
      END
C
Chd|====================================================================
Chd|  I11ASS05                      source/interfaces/int11/i11ass3.F
Chd|-- called by -----------
Chd|        I11ASS3                       source/interfaces/int11/i11ass3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11ASS05(JLT  ,CS_LOC,N1    ,N2   ,M1   ,
     2                    M2   ,HS1   ,HS2   ,HM1  ,HM2  ,
     3                    FX1  ,FY1   ,FZ1   ,FX2  ,FY2  ,
     4                    FZ2  ,FX3   ,FY3   ,FZ3  ,FX4  ,
     5                    FY4  ,FZ4   ,A     ,STIFN,NRTS ,
     6                    K1   ,K2    ,K3    ,K4   ,C1   ,
     7                    C2   ,C3    ,C4    ,VISCN,NIN  ,
     8                    INTTH ,PHIS1 ,PHIS2 ,PHIM1,PHIM2,
     9                    FTHE  ,CONDINTS1,CONDINTS2,CONDINTM1,
     A                    CONDINTM2,CONDN,JTASK)
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
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NRTS, NIN,INTTH ,
     +        CS_LOC(*),
     +        N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ),JTASK
      my_real
     .        HS1(MVSIZ),HS2(MVSIZ),HM1(MVSIZ),HM2(MVSIZ),
     .        FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .        FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .        FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .        FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .        K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .        C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .        PHIS1(MVSIZ),PHIS2(MVSIZ),CONDINTS1(MVSIZ),
     .        CONDINTS2(MVSIZ),PHIM1(MVSIZ),PHIM2(MVSIZ),
     .        CONDINTM1(MVSIZ),CONDINTM2(MVSIZ),
     .        A(3,*), STIFN(*), VISCN(*),FTHE(*),CONDN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, NODFI, ISHIFT
C-----------------------------------------------
C
      NODFI = NLSKYFI(NIN)
      ISHIFT = NODFI*(JTASK-1)
C
      IF(INTTH==0)THEN
        DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
            J1=N1(I)
            A(1,J1)=A(1,J1)+FX1(I)
            A(2,J1)=A(2,J1)+FY1(I)
            A(3,J1)=A(3,J1)+FZ1(I)
            STIFN(J1)=STIFN(J1)+K1(I)
            VISCN(J1)=VISCN(J1)+C1(I)
C
            J1=N2(I)
            A(1,J1)=A(1,J1)+FX2(I)
            A(2,J1)=A(2,J1)+FY2(I)
            A(3,J1)=A(3,J1)+FZ2(I)
            STIFN(J1)=STIFN(J1)+K2(I)
            VISCN(J1)=VISCN(J1)+C2(I)
          ELSE
            J1=N1(I)
            AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)+FX1(I)
            AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)+FY1(I)
            AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)+FZ1(I)
            STNFI(NIN)%P(J1+ISHIFT)=STNFI(NIN)%P(J1+ISHIFT)+K1(I)
            VSCFI(NIN)%P(J1+ISHIFT)=VSCFI(NIN)%P(J1+ISHIFT)+C1(I)
C
            J1=N2(I)
            AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)+FX2(I)
            AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)+FY2(I)
            AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)+FZ2(I)
            STNFI(NIN)%P(J1+ISHIFT)=STNFI(NIN)%P(J1+ISHIFT)+K2(I)
            VSCFI(NIN)%P(J1+ISHIFT)=VSCFI(NIN)%P(J1+ISHIFT)+C2(I)
          END IF
        END DO
      ELSE
       IF(NODADT_THERM == 1 ) THEN  
        DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
            J1=N1(I)
            A(1,J1)=A(1,J1)+FX1(I)
            A(2,J1)=A(2,J1)+FY1(I)
            A(3,J1)=A(3,J1)+FZ1(I)
            STIFN(J1)=STIFN(J1)+K1(I)
            VISCN(J1)=VISCN(J1)+C1(I)
            FTHE(J1)=FTHE(J1)+PHIS1(I)
            CONDN(J1) = CONDN(J1) + CONDINTS1(I)
C
            J1=N2(I)
            A(1,J1)=A(1,J1)+FX2(I)
            A(2,J1)=A(2,J1)+FY2(I)
            A(3,J1)=A(3,J1)+FZ2(I)
            STIFN(J1)=STIFN(J1)+K2(I)
            VISCN(J1)=VISCN(J1)+C2(I)
            FTHE(J1)=FTHE(J1)+PHIS2(I)
            CONDN(J1) = CONDN(J1) + CONDINTS2(I)
          ELSE
            J1=N1(I)
            AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)+FX1(I)
            AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)+FY1(I)
            AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)+FZ1(I)
            STNFI(NIN)%P(J1+ISHIFT)=STNFI(NIN)%P(J1+ISHIFT)+K1(I)
            VSCFI(NIN)%P(J1+ISHIFT)=VSCFI(NIN)%P(J1+ISHIFT)+C1(I)
            FTHEFI(NIN)%P(J1+ISHIFT)=FTHEFI(NIN)%P(J1+ISHIFT)+PHIS1(I)
            CONDNFI(NIN)%P(J1+ISHIFT)=CONDNFI(NIN)%P(J1+ISHIFT) + CONDINTS1(I)
C
            J1=N2(I)
            AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)+FX2(I)
            AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)+FY2(I)
            AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)+FZ2(I)
            STNFI(NIN)%P(J1+ISHIFT)=STNFI(NIN)%P(J1+ISHIFT)+K2(I)
            VSCFI(NIN)%P(J1+ISHIFT)=VSCFI(NIN)%P(J1+ISHIFT)+C2(I)
            FTHEFI(NIN)%P(J1+ISHIFT)=FTHEFI(NIN)%P(J1+ISHIFT)+PHIS2(I)
            CONDNFI(NIN)%P(J1+ISHIFT)=CONDNFI(NIN)%P(J1+ISHIFT) + CONDINTS2(I)
          END IF
        END DO
       ELSE
        DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
            J1=N1(I)
            A(1,J1)=A(1,J1)+FX1(I)
            A(2,J1)=A(2,J1)+FY1(I)
            A(3,J1)=A(3,J1)+FZ1(I)
            STIFN(J1)=STIFN(J1)+K1(I)
            VISCN(J1)=VISCN(J1)+C1(I)
            FTHE(J1)=FTHE(J1)+PHIS1(I)
C
            J1=N2(I)
            A(1,J1)=A(1,J1)+FX2(I)
            A(2,J1)=A(2,J1)+FY2(I)
            A(3,J1)=A(3,J1)+FZ2(I)
            STIFN(J1)=STIFN(J1)+K2(I)
            VISCN(J1)=VISCN(J1)+C2(I)
            FTHE(J1)=FTHE(J1)+PHIS2(I)
          ELSE
            J1=N1(I)
            AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)+FX1(I)
            AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)+FY1(I)
            AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)+FZ1(I)
            STNFI(NIN)%P(J1+ISHIFT)=STNFI(NIN)%P(J1+ISHIFT)+K1(I)
            VSCFI(NIN)%P(J1+ISHIFT)=VSCFI(NIN)%P(J1+ISHIFT)+C1(I)
            FTHEFI(NIN)%P(J1+ISHIFT)=FTHEFI(NIN)%P(J1+ISHIFT)+PHIS1(I)
C
            J1=N2(I)
            AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)+FX2(I)
            AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)+FY2(I)
            AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)+FZ2(I)
            STNFI(NIN)%P(J1+ISHIFT)=STNFI(NIN)%P(J1+ISHIFT)+K2(I)
            VSCFI(NIN)%P(J1+ISHIFT)=VSCFI(NIN)%P(J1+ISHIFT)+C2(I)
            FTHEFI(NIN)%P(J1+ISHIFT)=FTHEFI(NIN)%P(J1+ISHIFT)+PHIS2(I)
          END IF
        END DO
       ENDIF
      ENDIF
C
      IF(INTTH==0)THEN
       DO I=1,JLT
        J1=M1(I)
        A(1,J1)=A(1,J1)+FX3(I)
        A(2,J1)=A(2,J1)+FY3(I)
        A(3,J1)=A(3,J1)+FZ3(I)
        STIFN(J1)=STIFN(J1)+K3(I)
        VISCN(J1)=VISCN(J1)+C3(I)
C
        J1=M2(I)
        A(1,J1)=A(1,J1)+FX4(I)
        A(2,J1)=A(2,J1)+FY4(I)
        A(3,J1)=A(3,J1)+FZ4(I)
        STIFN(J1)=STIFN(J1)+K4(I)
        VISCN(J1)=VISCN(J1)+C4(I)
       ENDDO
C
      ELSE
       IF(NODADT_THERM == 1 ) THEN   
        DO I=1,JLT
         J1=M1(I)
         A(1,J1)=A(1,J1)+FX3(I)
         A(2,J1)=A(2,J1)+FY3(I)
         A(3,J1)=A(3,J1)+FZ3(I)
         STIFN(J1)=STIFN(J1)+K3(I)
         VISCN(J1)=VISCN(J1)+C3(I)
         FTHE(J1)  = FTHE(J1) + PHIM1(I)
         CONDN(J1) = CONDN(J1) + CONDINTM1(I)
C
         J1=M2(I)
         A(1,J1)=A(1,J1)+FX4(I)
         A(2,J1)=A(2,J1)+FY4(I)
         A(3,J1)=A(3,J1)+FZ4(I)
         STIFN(J1)=STIFN(J1)+K4(I)
         VISCN(J1)=VISCN(J1)+C4(I)
         FTHE(J1)  = FTHE(J1) + PHIM2(I)
         CONDN(J1) = CONDN(J1) + CONDINTM2(I)
        ENDDO     
C
       ELSE
C
        DO I=1,JLT
         J1=M1(I)
         A(1,J1)=A(1,J1)+FX3(I)
         A(2,J1)=A(2,J1)+FY3(I)
         A(3,J1)=A(3,J1)+FZ3(I)
         STIFN(J1)=STIFN(J1)+K3(I)
         VISCN(J1)=VISCN(J1)+C3(I)
         FTHE(J1)  = FTHE(J1) + PHIM1(I)
C
         J1=M2(I)
         A(1,J1)=A(1,J1)+FX4(I)
         A(2,J1)=A(2,J1)+FY4(I)
         A(3,J1)=A(3,J1)+FZ4(I)
         STIFN(J1)=STIFN(J1)+K4(I)
         VISCN(J1)=VISCN(J1)+C4(I)
         FTHE(J1)  = FTHE(J1) + PHIM2(I)
        ENDDO     
       ENDIF
      ENDIF
      RETURN
      END
C
Chd|====================================================================
Chd|  I11ASS2                       source/interfaces/int11/i11ass3.F
Chd|-- called by -----------
Chd|        I11ASS3                       source/interfaces/int11/i11ass3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11ASS2(JLT   ,CS_LOC ,N1    ,N2    ,M1     ,
     2                   M2    ,HS1    ,HS2   ,HM1   ,HM2    ,
     3                   FX1   ,FY1    ,FZ1   ,FX2   ,FY2    ,
     4                   FZ2   ,FX3    ,FY3   ,FZ3   ,FX4    ,
     5                   FY4   ,FZ4    ,FSKYI ,ISKY  ,NISKYFI,
     6                   STIF  ,NRTS   ,NIN   ,NOINT ,INTTH ,
     7                   PHIS1 ,PHIS2  ,PHIM1 ,PHIM2 ,FTHESKYI,
     8                   CONDINTS1,CONDINTS2,CONDINTM1,CONDINTM2,
     A                   CONDNSKYI)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "parit_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NRTS,NISKYFI,NIN,NOINT,INTTH ,
     +        CS_LOC(*),ISKY(*),
     +        N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ)
      my_real
     .        HS1(MVSIZ),HS2(MVSIZ),HM1(MVSIZ),HM2(MVSIZ),
     .        FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .        FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .        FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .        FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .        PHIS1(MVSIZ),PHIS2(MVSIZ),CONDINTS1(MVSIZ),
     .        CONDINTS2(MVSIZ),PHIM1(MVSIZ),PHIM2(MVSIZ),
     .        CONDINTM1(MVSIZ),CONDINTM2(MVSIZ),
     .        FSKYI(LSKYI,NFSKYI), STIF(*),FTHESKYI(*),CONDNSKYI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, NISKYL1, NISKYL,IGP,IGM,NISKYFIL
C
      NISKYL1 = 0
      DO I = 1, JLT
        IF (HM1(I)/=ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
      DO I = 1, JLT
        IF (HM2(I)/=ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO

      IGP = 0
      IGM = 0
      DO I=1,JLT
        IF(CS_LOC(I)<=NRTS) THEN
           IGP = IGP+2
        ELSE
           IGM = IGM+1
        ENDIF
      ENDDO   
     
#include "lockon.inc"
      NISKYL = NISKY
      NISKY = NISKY + NISKYL1 + IGP
      NISKYFIL = NISKYFI
      NISKYFI = NISKYFI + IGM
#include "lockoff.inc"

      IF (NISKYL+NISKYL1+IGP > LSKYI) THEN
         CALL ANCMSG(MSGID=26,ANMODE=ANINFO_BLIND)   
         CALL ARRET(2)
      ENDIF
      IF (NISKYFIL+IGM > NLSKYFI(NIN)) THEN
        CALL ANCMSG(MSGID=25,ANMODE=ANINFO_BLIND,
     .            I1=NOINT)
        CALL ARRET(2)
      ENDIF
C
      IF(INTTH==0)THEN
        DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(HS1(I))
            ISKY(NISKYL) = N1(I)
C
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(HS2(I))
            ISKY(NISKYL) = N2(I)
          ELSE
            NISKYFIL = NISKYFIL + 1
            FSKYFI(NIN)%P(1,NISKYFIL)=FX1(I)
            FSKYFI(NIN)%P(2,NISKYFIL)=FY1(I)
            FSKYFI(NIN)%P(3,NISKYFIL)=FZ1(I)
            FSKYFI(NIN)%P(4,NISKYFIL)=STIF(I)*ABS(HS1(I))
            FSKYFI(NIN)%P(5,NISKYFIL)=FX2(I)
            FSKYFI(NIN)%P(6,NISKYFIL)=FY2(I)
            FSKYFI(NIN)%P(7,NISKYFIL)=FZ2(I)
            FSKYFI(NIN)%P(8,NISKYFIL)=STIF(I)*ABS(HS2(I))
            ISKYFI(NIN)%P(NISKYFIL) = CS_LOC(I)-NRTS
          END IF
        END DO
      ELSE
       IF(NODADT_THERM == 1 ) THEN  
        DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(HS1(I))
            FTHESKYI(NISKYL)=PHIS1(I)
            CONDNSKYI(NISKYL)=CONDINTS1(I)
            ISKY(NISKYL) = N1(I)
C
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(HS2(I))
            FTHESKYI(NISKYL)=PHIS2(I)
            CONDNSKYI(NISKYL)=CONDINTS2(I)
            ISKY(NISKYL) = N2(I)
          ELSE
            NISKYFIL = NISKYFIL + 1
            FSKYFI(NIN)%P(1,NISKYFIL)=FX1(I)
            FSKYFI(NIN)%P(2,NISKYFIL)=FY1(I)
            FSKYFI(NIN)%P(3,NISKYFIL)=FZ1(I)
            FSKYFI(NIN)%P(4,NISKYFIL)=STIF(I)*ABS(HS1(I))
            FTHESKYFI(NIN)%P(2*(NISKYFIL-1)+1)=PHIS1(I)
             CONDNSKYFI(NIN)%P(2*(NISKYFIL-1)+1)=CONDINTS1(I)
            FSKYFI(NIN)%P(5,NISKYFIL)=FX2(I)
            FSKYFI(NIN)%P(6,NISKYFIL)=FY2(I)
            FSKYFI(NIN)%P(7,NISKYFIL)=FZ2(I)
            FSKYFI(NIN)%P(8,NISKYFIL)=STIF(I)*ABS(HS2(I))
            FTHESKYFI(NIN)%P(2*(NISKYFIL-1)+2)=PHIS2(I)
             CONDNSKYFI(NIN)%P(2*(NISKYFIL-1)+2)=CONDINTS2(I)
            ISKYFI(NIN)%P(NISKYFIL) = CS_LOC(I)-NRTS
          END IF
        END DO
       ELSE
        DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(HS1(I))
            FTHESKYI(NISKYL)=PHIS1(I)
            ISKY(NISKYL) = N1(I)
C
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(HS2(I))
            FTHESKYI(NISKYL)=PHIS2(I)
            ISKY(NISKYL) = N2(I)
          ELSE
            NISKYFIL = NISKYFIL + 1
            FSKYFI(NIN)%P(1,NISKYFIL)=FX1(I)
            FSKYFI(NIN)%P(2,NISKYFIL)=FY1(I)
            FSKYFI(NIN)%P(3,NISKYFIL)=FZ1(I)
            FSKYFI(NIN)%P(4,NISKYFIL)=STIF(I)*ABS(HS1(I))
            FTHESKYFI(NIN)%P(2*(NISKYFIL-1)+1)=PHIS1(I)
            FSKYFI(NIN)%P(5,NISKYFIL)=FX2(I)
            FSKYFI(NIN)%P(6,NISKYFIL)=FY2(I)
            FSKYFI(NIN)%P(7,NISKYFIL)=FZ2(I)
            FSKYFI(NIN)%P(8,NISKYFIL)=STIF(I)*ABS(HS2(I))
            FTHESKYFI(NIN)%P(2*(NISKYFIL-1)+2)=PHIS2(I)
            ISKYFI(NIN)%P(NISKYFIL) = CS_LOC(I)-NRTS
          END IF
        END DO
       ENDIF
      ENDIF
C
      IF(INTTH==0)THEN
       DO I=1,JLT
         IF (HM1(I)/=ZERO) THEN
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX3(I)
          FSKYI(NISKYL,2)=FY3(I)
          FSKYI(NISKYL,3)=FZ3(I)
          FSKYI(NISKYL,4)=STIF(I)*ABS(HM1(I))
          ISKY(NISKYL) = M1(I)
        ENDIF
       ENDDO
       DO I=1,JLT
        IF (HM2(I)/=ZERO) THEN
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX4(I)
          FSKYI(NISKYL,2)=FY4(I)
          FSKYI(NISKYL,3)=FZ4(I)
          FSKYI(NISKYL,4)=STIF(I)*ABS(HM2(I))
          ISKY(NISKYL) = M2(I)
        ENDIF
       ENDDO
      ELSE
       IF(NODADT_THERM == 1 ) THEN  
        DO I=1,JLT
         IF (HM1(I)/=ZERO) THEN
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX3(I)
          FSKYI(NISKYL,2)=FY3(I)
          FSKYI(NISKYL,3)=FZ3(I)
          FSKYI(NISKYL,4)=STIF(I)*ABS(HM1(I))
          ISKY(NISKYL) = M1(I)
          FTHESKYI(NISKYL)=PHIM1(I)
          CONDNSKYI(NISKYL)=CONDINTM1(I)
         ENDIF
        ENDDO
        DO I=1,JLT
         IF (HM2(I)/=ZERO) THEN
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX4(I)
          FSKYI(NISKYL,2)=FY4(I)
          FSKYI(NISKYL,3)=FZ4(I)
          FSKYI(NISKYL,4)=STIF(I)*ABS(HM2(I))
          ISKY(NISKYL) = M2(I)
          FTHESKYI(NISKYL)=PHIM2(I)
          CONDNSKYI(NISKYL)=CONDINTM2(I)
         ENDIF
        ENDDO
       ELSE
        DO I=1,JLT
         IF (HM1(I)/=ZERO) THEN
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX3(I)
          FSKYI(NISKYL,2)=FY3(I)
          FSKYI(NISKYL,3)=FZ3(I)
          FSKYI(NISKYL,4)=STIF(I)*ABS(HM1(I))
          ISKY(NISKYL) = M1(I)
          FTHESKYI(NISKYL)=PHIM1(I)
         ENDIF
        ENDDO
        DO I=1,JLT
         IF (HM2(I)/=ZERO) THEN
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX4(I)
          FSKYI(NISKYL,2)=FY4(I)
          FSKYI(NISKYL,3)=FZ4(I)
          FSKYI(NISKYL,4)=STIF(I)*ABS(HM2(I))
          ISKY(NISKYL) = M2(I)
          FTHESKYI(NISKYL)=PHIM2(I)
         ENDIF
        ENDDO
       ENDIF
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I11ASS25                      source/interfaces/int11/i11ass3.F
Chd|-- called by -----------
Chd|        I11ASS3                       source/interfaces/int11/i11ass3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11ASS25(JLT   ,CS_LOC ,N1    ,N2     ,M1    ,
     2                    M2    ,HS1    ,HS2   ,HM1    ,HM2   ,
     3                    FX1   ,FY1    ,FZ1   ,FX2    ,FY2   ,
     4                    FZ2   ,FX3    ,FY3   ,FZ3    ,FX4   ,
     5                    FY4   ,FZ4    ,ISKY  ,NISKYFI,NRTS  ,
     6                    K1    ,K2     ,K3    ,K4     ,C1    ,
     7                    C2    ,C3     ,C4    ,NIN    ,NOINT ,
     8                    INTTH ,PHIS1 ,PHIS2 ,PHIM1  ,PHIM2 ,
     9                    FTHESKYI,CONDINTS1,CONDINTS2,CONDINTM1,
     A                    CONDINTM2,CONDNSKYI)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "parit_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NRTS,NISKYFI,NIN,NOINT,INTTH ,
     +        CS_LOC(*),ISKY(*),
     +        N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ)
      my_real
     .        HS1(MVSIZ),HS2(MVSIZ),HM1(MVSIZ),HM2(MVSIZ),
     .        FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .        FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .        FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .        FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .        K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .        C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .        PHIS1(MVSIZ),PHIS2(MVSIZ),CONDINTS1(MVSIZ),
     .        CONDINTS2(MVSIZ),PHIM1(MVSIZ),PHIM2(MVSIZ),
     .        CONDINTM1(MVSIZ),CONDINTM2(MVSIZ),
     .        FSKYI(LSKYI,NFSKYI),FTHESKYI(*),CONDNSKYI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, NISKYL1, NISKYL,IGP,IGM,NISKYFIL
C
      NISKYL1 = 0
      DO I = 1, JLT
        IF (HM1(I)/=ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
      DO I = 1, JLT
        IF (HM2(I)/=ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO

      IGP = 0
      IGM = 0
      DO I=1,JLT
        IF(CS_LOC(I)<=NRTS) THEN
           IGP = IGP+2
        ELSE
           IGM = IGM+1
        ENDIF
      ENDDO        

#include "lockon.inc"
      NISKYL = NISKY
      NISKY = NISKY + NISKYL1 + IGP
      NISKYFIL = NISKYFI
      NISKYFI = NISKYFI + IGM
#include "lockoff.inc"
C
      IF (NISKYL+NISKYL1+IGP > LSKYI) THEN
         CALL ANCMSG(MSGID=26,ANMODE=ANINFO_BLIND)
         CALL ARRET(2)
      ENDIF
      IF (NISKYFIL+IGM > NLSKYFI(NIN)) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO_BLIND)
        CALL ARRET(2)
      ENDIF
C
      IF(INTTH == 0)THEN
        DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=K1(I)
            FSKYI(NISKYL,5)=C1(I)
            ISKY(NISKYL) = N1(I)
C
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=K2(I)
            FSKYI(NISKYL,5)=C2(I)
            ISKY(NISKYL) = N2(I)
          ELSE
            NISKYFIL = NISKYFIL + 1
            FSKYFI(NIN)%P(1,NISKYFIL)=FX1(I)
            FSKYFI(NIN)%P(2,NISKYFIL)=FY1(I)
            FSKYFI(NIN)%P(3,NISKYFIL)=FZ1(I)
            FSKYFI(NIN)%P(4,NISKYFIL)=K1(I)
            FSKYFI(NIN)%P(5,NISKYFIL)=C1(I)
            FSKYFI(NIN)%P(6,NISKYFIL)=FX2(I)
            FSKYFI(NIN)%P(7,NISKYFIL)=FY2(I)
            FSKYFI(NIN)%P(8,NISKYFIL)=FZ2(I)
            FSKYFI(NIN)%P(9,NISKYFIL)=K2(I)
            FSKYFI(NIN)%P(10,NISKYFIL)=C2(I)

            ISKYFI(NIN)%P(NISKYFIL) = CS_LOC(I)-NRTS
          END IF
         END DO
       ELSE
        IF(NODADT_THERM == 1 ) THEN  
         DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=K1(I)
            FSKYI(NISKYL,5)=C1(I)
            FTHESKYI(NISKYL)=PHIS1(I)
            CONDNSKYI(NISKYL)=CONDINTS1(I)
            ISKY(NISKYL) = N1(I)
C
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=K2(I)
            FSKYI(NISKYL,5)=C2(I)
            FTHESKYI(NISKYL)=PHIS2(I)
            CONDNSKYI(NISKYL)=CONDINTS2(I)
            ISKY(NISKYL) = N2(I)
          ELSE
            NISKYFIL = NISKYFIL + 1
            FSKYFI(NIN)%P(1,NISKYFIL)=FX1(I)
            FSKYFI(NIN)%P(2,NISKYFIL)=FY1(I)
            FSKYFI(NIN)%P(3,NISKYFIL)=FZ1(I)
            FSKYFI(NIN)%P(4,NISKYFIL)=K1(I)
            FSKYFI(NIN)%P(5,NISKYFIL)=C1(I)
            FTHESKYFI(NIN)%P(2*(NISKYFIL-1)+1)=PHIS1(I)
            CONDNSKYFI(NIN)%P(2*(NISKYFIL-1)+1)=CONDINTS1(I)
            FSKYFI(NIN)%P(6,NISKYFIL)=FX2(I)
            FSKYFI(NIN)%P(7,NISKYFIL)=FY2(I)
            FSKYFI(NIN)%P(8,NISKYFIL)=FZ2(I)
            FSKYFI(NIN)%P(9,NISKYFIL)=K2(I)
            FSKYFI(NIN)%P(10,NISKYFIL)=C2(I)
            FTHESKYFI(NIN)%P(2*(NISKYFIL-1)+2)=PHIS2(I)
            CONDNSKYFI(NIN)%P(2*(NISKYFIL-1)+2)=CONDINTS1(I)
            ISKYFI(NIN)%P(NISKYFIL) = CS_LOC(I)-NRTS
          END IF
         END DO
        ELSE
         DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=K1(I)
            FSKYI(NISKYL,5)=C1(I)
            FTHESKYI(NISKYL)=PHIS1(I)
            ISKY(NISKYL) = N1(I)
C
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=K2(I)
            FSKYI(NISKYL,5)=C2(I)
            FTHESKYI(NISKYL)=PHIS2(I)
            ISKY(NISKYL) = N2(I)
          ELSE
            NISKYFIL = NISKYFIL + 1
            FSKYFI(NIN)%P(1,NISKYFIL)=FX1(I)
            FSKYFI(NIN)%P(2,NISKYFIL)=FY1(I)
            FSKYFI(NIN)%P(3,NISKYFIL)=FZ1(I)
            FSKYFI(NIN)%P(4,NISKYFIL)=K1(I)
            FSKYFI(NIN)%P(5,NISKYFIL)=C1(I)
            FTHESKYFI(NIN)%P(2*(NISKYFIL-1)+1)=PHIS1(I)
            FSKYFI(NIN)%P(6,NISKYFIL)=FX2(I)
            FSKYFI(NIN)%P(7,NISKYFIL)=FY2(I)
            FSKYFI(NIN)%P(8,NISKYFIL)=FZ2(I)
            FSKYFI(NIN)%P(9,NISKYFIL)=K2(I)
            FSKYFI(NIN)%P(10,NISKYFIL)=C2(I)
            FTHESKYFI(NIN)%P(2*(NISKYFIL-1)+2)=PHIS2(I)
            ISKYFI(NIN)%P(NISKYFIL) = CS_LOC(I)-NRTS
          END IF
         END DO
        ENDIF
       ENDIF
C
      IF(INTTH == 0)THEN
        DO I=1,JLT
          IF (HM1(I)/=ZERO) THEN
           NISKYL = NISKYL + 1
           FSKYI(NISKYL,1)=FX3(I)
           FSKYI(NISKYL,2)=FY3(I)
           FSKYI(NISKYL,3)=FZ3(I)
           FSKYI(NISKYL,4)=K3(I)
           FSKYI(NISKYL,5)=C3(I)
           ISKY(NISKYL) = M1(I)
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (HM2(I)/=ZERO) THEN
           NISKYL = NISKYL + 1
           FSKYI(NISKYL,1)=FX4(I)
           FSKYI(NISKYL,2)=FY4(I)
           FSKYI(NISKYL,3)=FZ4(I)
           FSKYI(NISKYL,4)=K4(I)
           FSKYI(NISKYL,5)=C4(I)
           ISKY(NISKYL) = M2(I)
          ENDIF
        ENDDO
      ELSE
       IF(NODADT_THERM == 1 ) THEN  
        DO I=1,JLT
          IF (HM1(I)/=ZERO) THEN
           NISKYL = NISKYL + 1
           FSKYI(NISKYL,1)=FX3(I)
           FSKYI(NISKYL,2)=FY3(I)
           FSKYI(NISKYL,3)=FZ3(I)
           FSKYI(NISKYL,4)=K3(I)
           FSKYI(NISKYL,5)=C3(I)
           ISKY(NISKYL) = M1(I)
           FTHESKYI(NISKYL)=PHIM1(I)
           CONDNSKYI(NISKYL)=CONDINTM1(I)
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (HM2(I)/=ZERO) THEN
           NISKYL = NISKYL + 1
           FSKYI(NISKYL,1)=FX4(I)
           FSKYI(NISKYL,2)=FY4(I)
           FSKYI(NISKYL,3)=FZ4(I)
           FSKYI(NISKYL,4)=K4(I)
           FSKYI(NISKYL,5)=C4(I)
           ISKY(NISKYL) = M2(I)
           FTHESKYI(NISKYL)=PHIM2(I)
           CONDNSKYI(NISKYL)=CONDINTM2(I)
          ENDIF
        ENDDO
       ELSE
        DO I=1,JLT
          IF (HM1(I)/=ZERO) THEN
           NISKYL = NISKYL + 1
           FSKYI(NISKYL,1)=FX3(I)
           FSKYI(NISKYL,2)=FY3(I)
           FSKYI(NISKYL,3)=FZ3(I)
           FSKYI(NISKYL,4)=K3(I)
           FSKYI(NISKYL,5)=C3(I)
           ISKY(NISKYL) = M1(I)
           FTHESKYI(NISKYL)=PHIM1(I)
          ENDIF
        ENDDO
        DO I=1,JLT
          IF (HM2(I)/=ZERO) THEN
           NISKYL = NISKYL + 1
           FSKYI(NISKYL,1)=FX4(I)
           FSKYI(NISKYL,2)=FY4(I)
           FSKYI(NISKYL,3)=FZ4(I)
           FSKYI(NISKYL,4)=K4(I)
           FSKYI(NISKYL,5)=C4(I)
           ISKY(NISKYL) = M2(I)
           FTHESKYI(NISKYL)=PHIM2(I)
          ENDIF
        ENDDO
       ENDIF
      ENDIF
C
      RETURN
      END
