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
Chd|  INIEBCS_DP                    source/boundary_conditions/ebcs/iniebcs_dp.F
Chd|-- called by -----------
Chd|        INIEBCSP0                     source/boundary_conditions/ebcs/iniebcsp0.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INIGRAV                       share/modules1/inigrav_mod.F  
Chd|====================================================================
      SUBROUTINE INIEBCS_DP(NSEG,NOD,ISEG,IELEM,IRECT,LISTE,
     .                    IPARG,ELBUF_STR,X, IXS,IXQ,IXTG,DP0,
     .                    IPARTS,IPARTQ,IPARTTG)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C  INPUT : SEGMENT defined from /EBCS/NRF
C OUTPUT : EBCS%DP0 : static hydropressure increment due to gravity loading if defined (/INIGRAV)
C pre-condition : /EBCS/NRF defined (already check from parent subroutine)
C                 NSEG >0 : LOOP is from 1 to NSEG ;  NSEG=0 : subroutine does nothing
C
C comments : DP0 = rho0 * grav0 * dist
C                    rho0 is retrived from adjacent cell, initialized possibly by /INIGRAV, so it is in GBUF%RHO and not PM( )
C                    grav0 is stored during INIGRAV procedure
C                    dist is computed here, it is 2*dh  where h=distance(cell_centroid, cell_face). It corresponds to distance to the centroid of a ghost cell
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD    
      USE INIGRAV 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER             :: NSEG,NOD,ISEG(NSEG),IRECT(4,NSEG),LISTE(NOD),IPARG(NPARG,NGROUP), IELEM(NSEG)
      INTEGER,INTENT(IN)  :: IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ), IXTG(NIXTG,NUMELTG)
      INTEGER, INTENT(IN) :: IPARTS(NUMELS), IPARTQ(NUMELQ), IPARTTG(NUMELTG)      
      my_real :: X(3,NUMNOD), DP0(NSEG)
      TYPE (ELBUF_STRUCT_),DIMENSION(NGROUP), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NodeLOC(4),I,NG,IS,KSEG,NodeG(8),ESEG,EAD,KTY,KLT,MFT,II(6),ISOLNOD,ITY,IP
      my_real :: ORIENT, FAC, X13,Y13,Z13,X24,Y24,Z24, NX,NY,NZ,S,VN,P,DPDV,ZF(3),ZC(3),VEC(3),RHO ,DIST,GRAV0
      my_real :: NGX,NGY,NGZ                       
      TYPE(G_BUFEL_)  ,POINTER :: GBUF  
      LOGICAL IS_TRIA   
c=======================================================================

      DO IS=1,NSEG    
                                                                     
        KSEG = ABS(ISEG(IS))                                                         
        ORIENT = ZERO                                                                
        IF(KSEG /= 0)ORIENT=FLOAT(ISEG(IS)/KSEG)                                     

        !local ids in IRECT array
        NodeLOC(1:4) = IRECT(1:4,IS)                                                               

        !global internal node ids
        NodeG(1:4)=LISTE(NodeLOC(1:4))                                                                
        
        IS_TRIA = .FALSE.
        IF(NodeLOC(4) == 0 .OR. NodeLOC(3) == NodeLOC(4))IS_TRIA=.TRUE.
        
        !centroid at face
        IF(IS_TRIA) THEN                                               
          FAC=THIRD*ORIENT                                                           
          !NodeLOC(3)=NodeLOC(4)
          ZF(1) = FAC*SUM( X(1,NodeG(1:3)) )
          ZF(2) = FAC*SUM( X(2,NodeG(1:3)) )
          ZF(3) = FAC*SUM( X(3,NodeG(1:3)) )
        ELSE                                                                         
          FAC=FOURTH*ORIENT                                                          
          ZF(1) = FAC*SUM( X(1,NodeG(1:4)) )
          ZF(2) = FAC*SUM( X(2,NodeG(1:4)) )
          ZF(3) = FAC*SUM( X(3,NodeG(1:4)) )
        ENDIF                                                                        

        !centroid at cell
        ESEG=IELEM(IS)                                                                                   
        !get density                                                                                     
        DO NG=1,NGROUP                                                                                   
          KTY = IPARG(5,NG)                                                                              
          KLT = IPARG(2,NG)                                                                              
          MFT = IPARG(3,NG)                                                                              
          ITY = IPARG(5,NG)
          ISOLNOD = IPARG(28,NG)                                                                              
          IF (ESEG.LE.KLT+MFT) EXIT                                                      
          IF(N2D==0)THEN
            IF(ITY/=1)CYCLE
            IF(ISOLNOD == 0)THEN
              print *,"**ERROR /EBCS/NRF : #2205"
              CYCLE
            ENDIF
          ELSE
            IF(ITY /= 2 .AND. ITY /= 7)CYCLE
          ENDIF
        ENDDO                                                                                            
        EAD = ESEG-MFT  ! at this step it is ensured that EAD \in [1,LLT]      
        IF(EAD<=0)CYCLE
        GBUF => ELBUF_STR(NG)%GBUF                                                                           
        RHO = GBUF%RHO(EAD) !retrieve rho brom GBUF and not PM since it could be initialized by INIGRAV  
        !cell centroid                                                                                   
        IF(ITY==1)THEN
           NodeG(1:8)=IXS(2:9,ESEG)
           ZC(1)= SUM(X(1,NodeG(1:ISOLNOD)))/ISOLNOD
           ZC(2)= SUM(X(2,NodeG(1:ISOLNOD)))/ISOLNOD
           ZC(3)= SUM(X(3,NodeG(1:ISOLNOD)))/ISOLNOD
           IP = IPARTS(ESEG)
        ELSEIF(ITY ==  2)THEN
           NodeG(1:4)=IXQ(2:5,ESEG)
           ZC(1)= FOURTH*SUM(X(1,NodeG(1:4)))
           ZC(2)= FOURTH*SUM(X(2,NodeG(1:4)))
           ZC(3)= FOURTH*SUM(X(3,NodeG(1:4)))  
           IP = IPARTQ(ESEG)                 
        ELSEIF(ITY == 7)THEN
           NodeG(1:3)=IXTG(2:4,ESEG)
           ZC(1)= THIRD*SUM(X(1,NodeG(1:3)))
           ZC(2)= THIRD*SUM(X(2,NodeG(1:3)))
           ZC(3)= THIRD*SUM(X(3,NodeG(1:3)))
           IP = IPARTTG(ESEG)                     
        ELSE
          !not supposed to happen
          print *, "**ERROR /EBCS/NRF : ONE SEGMENT IS LOCATED TO AN UNEXPECTED TYPE OF ELEMENTS"
          CYCLE !next IS (segment)
        ENDIF  
        
        !-- distance from cell centroid to ghost cell is twice the distance from centroid to face centroid
        VEC(1) = -ZC(1)+ZF(1)
        VEC(2) = -ZC(2)+ZF(2)
        VEC(3) = -ZC(3)+ZF(3)
        !DIST = VEC(1)*VEC(1) + VEC(2)*VEC(2) + VEC(3)*VEC(3)
        !DIST = TWO*SQRT(DIST) 
        
        !-- increment of static pressure due to initial gravity loading
        !    stored in DP0 => EBCS_TAB%..%DP0
        IF(INIGRAV_PARTS%IS_ALLOCATED)THEN
          IF(INIGRAV_PARTS%TAGPART(IP) == 1)THEN
            GRAV0 = INIGRAV_PARTS%GRAV0(IP)   !0.0 if not related to inigrav option
          ELSE
            GRAV0 = ZERO
          ENDIF
        ELSE
          GRAV0 = ZERO
        ENDIF
        IF(GRAV0 /= ZERO)THEN
        
            NGX=INIGRAV_PARTS%NG(1,IP)
            NGY=INIGRAV_PARTS%NG(2,IP)
            NGZ=INIGRAV_PARTS%NG(3,IP)
            
            DIST = VEC(1)*NGX + VEC(2)*NGY +VEC(3)*NGZ
            DIST = TWO * DIST
            
            DP0(IS) = RHO*ABS(GRAV0)*DIST
         ELSE 
           !no increment to hydrostatic pressure
           DP0(IS)=ZERO
         ENDIF

                                                                       
      ENDDO                                                                          


      END SUBROUTINE INIEBCS_DP
