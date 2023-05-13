      character(16) cell_name(200)
	character(85) string
	character(30) db_file_name
	character(1) answer
      integer number_of_out
	character(70) buffer
     
c************************************ Columns in NCI DATABASE*****************************
	character(8) NSC
	character(6) CONCUNIT
	character(6) LCONC
	character(30) PANEL
	character(16) CELL
	character(6) PANELNBR
	character(6) CELLNBR
	character(10) NLOGGI50
	character(6) INDN  
	character(6) TOTN  
	character(10) STDDEV
c******************************************************************************************
      logical flag
	logical find_name

      integer NSC_number
	integer NSC_number_array(1000000) !array, containing inner number for a given NCI number
      integer(1) multiple(1000000)
      integer NSC_pointer_array(100000)
	integer n_compound
	real activity(100000,200)
      real mean_activity(100000)
	real activity_cen(100000,200)
	integer seed_number,seed_number1
	integer seed,seed1
	real zero, mm
	real covariation
	real variation1
	real variation2
	integer n_cells
	integer n_compare(100000)
	integer n_compare_thr
	real correlation(100000)
	real maxcor,max_ref_cor
	integer maxnumber
	integer max_n_compare
	integer number_over_mean(100000)
	integer number_over_mean_thr
c*********************************************************************************************
	integer agent_number
      character(20) action_mode
	character(30)  agent_name
	integer n_compare_agent

      common/NSC/NSC_number_array,NSC_pointer_array,activity_cen,
     &	n_compound,mean_activity,number_over_mean






      data (cell_name(i), i=1,159) /
     &  'A431           ',    ! MIS                                 
     &  'A-FOS 2        ',    ! MIS                              
     &  'A-FOS 3        ',    ! MIS                             
     &  'A-JUN 1        ',    ! MIS                             
     &  'A-JUN 3        ',    ! MIS                             
     &  'A-C/EBP 3      ',    ! MIS                           
     &  'A-CREB 1       ',    ! MIS                            
     &  'A-CREB 2       ',    ! MIS                            
     &  'CHO            ',    ! MIS                               
     &  'CHO/159-1      ',    ! MIS                         
     &  'NYH            ',    ! Non-Small Cell Lung              
     &  'NYH/ICRF-187-1 ',    ! Non-Small Cell Lung    
     &  'NCI-H23         ',   ! Non-Small Cell Lung      1              
     &  'NCI-H522        ',   ! Non-Small Cell Lung      2             
     &  'A549/ATCC       ',   ! Non-Small Cell Lung      3            
     &  'CALU-1          ',   ! Non-Small Cell Lung               
     &  'EKVX            ',   ! Non-Small Cell Lung                 
     &  'NCI-H226        ',   ! Non-Small Cell Lung            
     &  'NCI-H322M       ',   ! Non-Small Cell Lung           
     &  'NCI-H460        ',   ! Non-Small Cell Lung            
     &  'HOP-62          ',   ! Non-Small Cell Lung              
     &  'HOP-18          ',   ! Non-Small Cell Lung              
     &  'HOP-92          ',   ! Non-Small Cell Lung              
     &  'LXFL 529        ',   ! Non-Small Cell Lung            
     &  'SW-1573         ',   ! Non-Small Cell Lung             
     &  'LXFS 650L       ',   ! Non-Small Cell Lung           
     &  'MLI-019         ',   ! Non-Small Cell Lung             
     &  'MLI-076         ',   ! Non-Small Cell Lung             
     &  'MLI-045         ',   ! Non-Small Cell Lung             
     &  'UABLG22         ',   ! Non-Small Cell Lung             
     &  'DMS 114         ',   ! Small Cell Lung                  
     &  'DMS 273         ',   ! Small Cell Lung                 
     &  'HT29            ',   ! Colon                               
     &  'HCC-2998        ',   ! Colon                           
     &  'HCT-116         ',   ! Colon                            
     &  'SW-620          ',   ! Colon                             
     &  'COLO 205        ',   ! Colon                          
     &  'DLD-1           ',   ! Colon                             
     &  'HCT-15          ',   ! Colon                            
     &  'KM12            ',   ! Colon                              
     &  'KM20L2          ',   ! Colon                            
     &  'COLO 741        ',   ! Colon                          
     &  'CXF 264L        ',   ! Colon                          
     &  'COLO 746        ',   ! Colon                          
     &  'MLI-059         ',   ! Colon                           
     &  'CACO-2          ',   ! Colon                            
     &  'HCT-116/P       ',   ! Colon                      
     &  'HCT-116/CMV-1   ',   ! Colon                  
     &  'HCT-116/CMV-2   ',   ! Colon                  
     &  'HCT-116/E6-1    ',   ! Colon                   
     &  'HCT-116/E6-2    ',   ! Colon                   
     &  'HCT-116/PV      ',   ! Colon                        
     &  'HCT-116/P21/A   ',   ! Colon                  
     &  'HCT-116/P21/B   ',   ! Colon                  
     &  'HCT-116/P21/C   ',   ! Colon                  
     &  'RKOp53RE1       ',   ! Colon                      
     &  'HT29p53RE22     ',   ! Colon                    
     &  'H1299p53RE29    ',   ! Colon                   
     &  'RKO Waf1        ',   ! Colon                       
     &  'MCF7            ',   ! Breast                                
     &  'NCI/ADR-RES     ',   ! Breast                         
     &  'ZR-75-1         ',   ! Breast                           
     &  'ZR-75-30        ',   ! Breast                           
     &  'MDA-MB-231/ATCC ',   ! Breast                    
     &  'HS 578T         ',   ! Breast                             
     &  'UISO-BCA-1      ',   ! Breast                        
     &  'MCF7/ATCC       ',   ! Breast                         
     &  'SK-BR-3         ',   ! Breast                       
     &  'MDA-MB-435      ',   ! Breast                         
     &  'MDA-N           ',   ! Breast                              
     &  'BT-549          ',   ! Breast                             
     &  'T-47D           ',   ! Breast
     &  'MAXF 401        ',   ! Breast                         
     &  'MDA-MB-468      ',   ! Breast                       
     &  'T47D FOS1       ',   ! Breast                     
     &  'T47D NFkB15     ',   ! Breast                   
     &  'T47D ERE4       ',   ! Breast                     
     &  'MCF7-E6         ',   ! Breast                       
     &  'MDA-MB-435S     ',   ! Breast                  
     &  'OVCAR-3         ',   ! Ovarian                          
     &  'OVCAR-4         ',   ! Ovarian                          
     &  'OVCAR-5         ',   ! Ovarian                          
     &  'OVCAR-8         ',   ! Ovarian                          
     &  'IGROV1          ',   ! Ovarian                          
     &  'SK-OV-3         ',   ! Ovarian                         
     &  'ES-2            ',   ! Ovarian                            
     &  'P388            ',   ! Leukemia                           
     &  'P388/ADR        ',   ! Leukemia                       
     &  'CCRF-CEM        ',   ! Leukemia                        
     &  'K-562           ',   ! Leukemia                           
     &  'MOLT-4          ',   ! Leukemia                          
     &  'HL-60(TB)       ',   ! Leukemia                       
     &  'RPMI-8226       ',   ! Leukemia                         
     &  'VDSO/P          ',   ! Leukemia                           
     &  'VDSO/CMV-8      ',   ! Leukemia                       
     &  'VDSO/CMV-9      ',   ! Leukemia                       
     &  'VDSO/E6-18      ',   ! Leukemia                       
     &  'VDSO/E6-19      ',   ! Leukemia                       
     &  'SR              ',   ! Leukemia                             
     &  'NB4             ',   ! Leukemia                             
     &  'WI-38           ',   ! FIB                          
     &  'CCD-19LU        ',   ! FIB                          
     &  'Mar-Bel         ',   ! FIB                          
     &  'UO-31           ',   ! Renal                              
     &  'SN12C           ',   ! Renal                              
     &  'SN12K1          ',   ! Renal                           
     &  'A498            ',   ! Renal                              
     &  'CAKI-1          ',   ! Renal                            
     &  'RXF 393         ',   ! Renal                           
     &  'RXF-631         ',   ! Renal                           
     &  '786-0           ',   ! Renal                             
     &  'SW-156          ',   ! Renal                         
     &  'TK-164          ',   ! Renal                          
     &  'ACHN            ',   ! Renal                              
     &  'TK-10           ',   ! Renal                             
     &  'RXF 486L        ',   ! Renal                          
     &  'UOK-57          ',   ! Renal                            
     &  'UOK-57LN        ',   ! Renal                         
     &  'LOX IMVI        ',   ! Melanoma                       
     &  'MALME-3M        ',   ! Melanoma                       
     &  'RPMI-7951       ',   ! Melanoma                      
     &  'SK-MEL-2        ',   ! Melanoma                       
     &  'SK-MEL-5        ',   ! Melanoma                       
     &  'SK-MEL-28       ',   ! Melanoma                      
     &  'M14             ',   ! Melanoma                           
     &  'M19-MEL         ',   ! Melanoma                       
     &  'UACC-62         ',   ! Melanoma                       
     &  'UACC-257        ',   ! Melanoma                      
     &  'MEXF 514L       ',   ! Melanoma                     
     &  'UABMEL3         ',   ! Melanoma                       
     &  'PC-3            ',   ! Prostate                          
     &  'DU-145          ',   ! Prostate                        
     &  'JCA-1           ',   ! Prostate
     &  'ND-1            ',   ! Prostate                          
     &  'TSU-PRI         ',   ! Prostate                       
     &  'SNB-19          ',   ! Central Nervous System           
     &  'SNB-75          ',   ! Central Nervous System           
     &  'SNB-78          ',   ! Central Nervous System           
     &  'U251            ',   ! Central Nervous System             
     &  'SF-268          ',   ! Central Nervous System          
     &  'SF-295          ',   ! Central Nervous System          
     &  'SF-539          ',   ! Central Nervous System          
     &  'XF 498          ',   ! Central Nervous System          
     &  'SW 1088         ',   ! Central Nervous System        
     &  'SW 1783         ',   ! Central Nervous System        
     &  'SF-767          ',   ! Central Nervous System          
     &  'SF-763          ',   ! Central Nervous System          
     &  'SMS-KCNR        ',   ! Central Nervous System        
     &  'A204/ATCC       ',   ! Sarcoma                       
     &  'OHS             ',   ! Sarcoma                             
     &  'TE85            ',   ! Sarcoma                           
     &  'A673            ',   ! Sarcoma                            
     &  'CHA-59          ',   ! Sarcoma                          
     &  'RH18            ',   ! Sarcoma                           
     &  'RH30            ',   ! Sarcoma                           
     &  'RD              ',   ! Sarcoma                             
     &  'HT              ',   ! LYM                               
     &  'RL              ',   ! LYM                                  
     &  'DB              '/   ! LYM                               





c      number_of_out=200
c      call GETARG(1,buffer)
c      if(buffer(1:5).EQ.'-help') call show_help
c      if(buffer(1:5).EQ.'-Help') call show_help
c      if(buffer(1:2).EQ.'-h') call show_help
c      if(buffer(1:2).EQ.'-H') call show_help
c      if(buffer(1:1).EQ.'h') call show_help
c      if(buffer(1:1).EQ.'H') call show_help
c	read(buffer,*,END=1,ERR=1) number_of_out
c    1 continue


c      n_compare_thr=4
c	number_over_mean_thr=4

c      call GETARG(2,buffer)
c	read(buffer,*,END=2,ERR=2) n_compare_thr
c    2 continue
c
c      call GETARG(3,buffer)
c	read(buffer,*,END=3,ERR=3) number_over_mean_thr
c    3 continue



      write(*,*) 'CANCER CELL LINES FOR CORRELATION:'
      do i=1,159
       write(*,*) i, cell_name(i)
      enddo
      write(*,*) '********************************************'
c      write(*,fmt='(A20,2X,I6)') 
c     &	'Compounds to output:', number_of_out  
c      write(*,fmt='(A41,2X,I4)') 
c     &	'Output threshold for compared cell lines:', n_compare_thr  
c      write(*,fmt='(A61,2X,I4)') 
c     &'Output threshold for number of cell lines over mean activity:'
c     & , number_over_mean_thr  
c      write(*,*) '********************************************'




      zero=0.00000000000000000000
	mm=-1000000.000

      
      
      db_file_name='CANCER60GI50_LIS_target.txt          '

      INQUIRE(FILE=db_file_name, EXIST=flag)
      if(.not.FLAG) then
	 write(*,fmt='(A28,A30)') 'There is no file with name: ',
     & db_file_name 
       write(*,fmt='(A20)') 'in working directory'
       stop
      endif


	open (1, file=db_file_name)

	read (1,fmt='(A85)',END=10,ERR=20) string


      do i=1,1000000
	 NSC_number_array(i)=-1
	 multiple(i)=0
      enddo
	do i=1,100000
	 do j=1,200
	  activity(i,j) = -10000000.0
       enddo
      enddo

      n_compound=0

	do i=1, 1000000000
	read (1,fmt='(A85)',END=10,ERR=20) string

      NSC      ='      '
	CONCUNIT ='      '
	LCONC    ='      '
	PANEL    ='                              '
	CELL     ='                '
	PANELNBR ='      '
	CELLNBR  ='      '
	NLOGGI50 ='          '
      

        j=1
	  k=1

  100   if(string(j:j).EQ.',') then
	   j=j+1
	   k=1
        else
         NSC(k:k)=string(j:j)
	   j=j+1
	   k=k+1
         goto 100
        endif         
c       write(*,*) NSC


  200   if(string(j:j).EQ.',') then
	   j=j+1
	   k=1
        else
         CONCUNIT(k:k)=string(j:j)
	   j=j+1
	   k=k+1
         goto 200
        endif         
c        write(*,*) CONCUNIT


  300   if(string(j:j).EQ.',') then
	   j=j+1
	   k=1
        else
         LCONC(k:k)=string(j:j)
	   j=j+1
	   k=k+1
         goto 300
        endif         
c        write(*,*) LCONC



  400   if(string(j:j).EQ.',') then
	   j=j+1
	   k=1
        else
         PANEL(k:k)=string(j:j)
	   j=j+1
	   k=k+1
         goto 400
        endif
c	  write(*,*) PANEL         

  500   if(string(j:j).EQ.',') then
	   j=j+1
	   k=1
        else
         CELL(k:k)=string(j:j)
	   j=j+1
   	   k=k+1
         goto 500
        endif


c	  write(*,*) CELL         

  600   if(string(j:j).EQ.',') then
	   j=j+1
	   k=1
        else
         PANELNBR(k:k)=string(j:j)
	   j=j+1
	   k=k+1
         goto 600
        endif         

  700   if(string(j:j).EQ.',') then
	   j=j+1
	   k=1
        else
         CELLNBR(k:k)=string(j:j)
	   j=j+1
	   k=k+1
         goto 700
        endif         

  800   if(string(j:j).EQ.',') then
	   j=j+1
	   k=1
        else
         NLOGGI50(k:k)=string(j:j)
	   j=j+1
	   k=k+1
         goto 800
        endif         


      read(NSC,*,ERR=20,END=20) NSC_number
       if(NSC_number_array(NSC_number).EQ.-1) then
	  n_compound=n_compound+1
        NSC_number_array(NSC_number)=n_compound
	 endif  


       find_name=.FALSE.
       do l=1,159
       if(cell_name(l).EQ.CELL) then
	  find_name=.TRUE. 
        if(activity(NSC_number_array(NSC_number),l).GT.0.0)
     &   multiple(NSC_number)=1
        read(NLOGGI50,*,END=20,ERR=20) 
     &	            activity(NSC_number_array(NSC_number),l)
       endif
       enddo
	 if(find_name.EQ..FALSE.) then
	  write(*,*) 'not find cell name:', NSC,CELL,PANEL
       endif



	enddo


	

   10 continue
      close(1)

      j=0
      do i=1,1000000
	 if(NSC_number_array(i).NE.-1) 
     &	 NSC_pointer_array(NSC_number_array(i))=i
	 if(multiple(i).EQ.1) then
	  write(*,*) 'multiple data, NSC', i
	  j=j+1
	 endif  
      enddo
      write(*,*) 'Number of multiple:', j

      write(*,*) 'Finding ', n_compound, ' compounds in database'

      do i=1,n_compound
       mean_activity(i)=zero
       number_over_mean(i)=0 
	 n_cells=0
	 do j=1,200
	  if (activity(i,j).GE.zero) then
	   mean_activity(i) = mean_activity(i) + activity(i,j)
	   n_cells=n_cells+1
        endif
       enddo
       mean_activity(i)=mean_activity(i)/(float(n_cells))      
	 do j=1,200
	  if (activity(i,j).GE.zero) then
	   activity_cen(i,j) =  activity(i,j) - mean_activity(i)
          if(activity_cen(i,j).GT.zero) 
     &                    number_over_mean(i)=number_over_mean(i) + 1
c         write(*,*) j,activity(i,j),mean_activity,activity_cen(i,j)
	  else
	   activity_cen(i,j) = -10000000.0
        endif 
       enddo
      enddo



			

      call clusterize
	call clusterize_plus



      stop

   20 write(*,*) 'some mistakes occured while db file reading'
      stop
	end