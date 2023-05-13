      subroutine clusterize_plus
      use msimsl


	integer NSC_number_array(1000000) !array, containing inner number for a given NCI number
      integer NSC_pointer_array(100000)
	real activity_cen(100000,200)
      integer i,j,k,iter

      integer seed1,seed2

	real zero,mm
	real covariation,variation1,variation2
	integer n_cells,n_ref,max_ref
	real correlation(200,200),correlation_m, maxcor
	real D(200)
	integer n_compare(200,200)
	integer agent_number(200),agent_number_m
      character(20) action_mode(200),action_mode_m
	character(30)  agent_name(200),agent_name_m
	integer n_compound, icomp

      real min_corr

      complex eval(200)
	complex evec(200,200)
	integer ldevec


	integer n_clust,cluster_n(200),cluster_n_old
	integer number_in_cluster(200)
	real centers(200,200),distance,min_distance
	integer n_dim
	real max_evec
	logical convergence

	integer cluster_pop(10),max_pop,false_pop,max_cluster
	integer n_antimitotics
      real mean_activity(100000)
	integer number_over_mean(100000)

	integer time

      common/NSC/NSC_number_array,NSC_pointer_array,activity_cen,
     & n_compound, mean_activity, number_over_mean




      zero=0.00000000000000000000
	mm=-1000000.000
	n_antimitotics=0





      data (action_mode(i),agent_name(i),agent_number(i), i=1,127) /
     &'Alkylating Agents   ','Asaley                        ', 167780,  
     &'Alkylating Agents   ','AZQ                           ', 182986,  
     &'Alkylating Agents   ','BCNU                          ', 409962,  
     &'Alkylating Agents   ','Busulfan                      ',    750,  
     &'Alkylating Agents   ','Carboxyphthalatoplatinum      ', 271674,  
     &'Alkylating Agents   ','CBDCA                         ', 241240,  
     &'Alkylating Agents   ','CCNU                          ',  79037,  
     &'Alkylating Agents   ','CHIP                          ', 256927,  
     &'Alkylating Agents   ','Chlorambucil                  ',   3088,  
     &'Alkylating Agents   ','Chlorozotocin                 ', 178248,  
     &'Alkylating Agents   ','Cis-platinum                  ', 119875,  
     &'Alkylating Agents   ','Clomesone                     ', 338947,  
c     &'Alkylating Agents   ','Cyanomorpholinodoxorubicin    ', 357704,  
     &'Alkylating Agents   ','Cyclodisone                   ', 348948,  
     &'Alkylating Agents   ','Dianhydrogalactitol           ', 132313,  
     &'Alkylating Agents   ','Fluorodopan                   ',  73754,  
     &'Alkylating Agents   ','Hepsulfam                     ', 329680,  
     &'Alkylating Agents   ','Hycanthone                    ', 142982,  
     &'Alkylating Agents   ','Melphalan                     ',   8806,  
     &'Alkylating Agents   ','Methyl CCNU                   ',  95441,  
     &'Alkylating Agents   ','mitomycin C                   ',  26980,  
     &'Alkylating Agents   ','mitozolamide                  ', 353451,  
     &'Alkylating Agents   ','nitrogen mustard              ',    762,  
     &'Alkylating Agents   ','PCNU                          ',  95466,  
     &'Alkylating Agents   ','piperazine ../drugs/mainator  ', 344007,  
     &'Alkylating Agents   ','piperazinedione               ', 135758,  
     &'Alkylating Agents   ','pipobroman                    ',  25154,  
     &'Alkylating Agents   ','porfiromycin                  ',  56410,  
     &'Alkylating Agents   ','spirohydantoin mustard        ', 172112,  
     &'Alkylating Agents   ','teroxirone                    ', 296934,  
     &'Alkylating Agents   ','tetraplatin                   ', 363812,  
     &'Alkylating Agents   ','thio-tepa                     ',   6396,  
     &'Alkylating Agents   ','triethylenemelamine           ',   9706,  
     &'Alkylating Agents   ','uracil nitrogen mustard       ',  34462,
     &'Alkylating Agents   ','Yoshi-864                     ', 102627, 
     &'Antimitotic Agents  ','allocolchicine                ', 406042,      
     &'Antimitotic Agents  ','Halichondrin B                ', 609395,  
     &'Antimitotic Agents  ','colchicine                    ',    757,  
     &'Antimitotic Agents  ','colchicine derivative         ',  33410,  
     &'Antimitotic Agents  ','dolastatin 10                 ', 376128,  
     &'Antimitotic Agents  ','maytansine                    ', 153858,  
     &'Antimitotic Agents  ','rhizoxin                      ', 332598,  
     &'Antimitotic Agents  ','taxol                         ', 125973,  
     &'Antimitotic Agents  ','taxol derivative              ', 608832,  
     &'Antimitotic Agents  ','thiocolchicine                ', 361792,  
     &'Antimitotic Agents  ','trityl cysteine               ',  83265,  
     &'Antimitotic Agents  ','vinblastine sulfate           ',  49842,  
     &'Antimitotic Agents  ','vincristine sulfate           ',  67574,  
     &'Antimitotic Agents  ','noscapine                     ',   5366,
     &'Antimitotic Agents  ','griseofulvin                  ',  34533,
     &'Antimitotic Agents  ','Me-estradiol                  ', 659853,
     &'Antimitotic Agents  ','Combretastatin A1             ', 600032,
     &'Antimitotic Agents  ','Combretastatin A4             ', 613729,
     &'Antimitotic Agents  ','NSC 56030                     ',  56030,
     &'Antimitotic Agents  ','CryptophycinB                 ', 670038,
     &'Topoisomerase I Inh.','Camptothecin                  ',  94600,  
     &'Topoisomerase I Inh.','camptothecin, Na salt         ', 100880,  
     &'Topoisomerase I Inh.','aminocamptothecin             ', 603071,  
     &'Topoisomerase I Inh.','camptothecin derivative       ',  95382,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 107124,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 643833,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 629971,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 295500,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 249910,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 606985,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 374028,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 176323,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 295501,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 606172,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 606173,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 610458,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 618939,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 610457,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 610459,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 606499,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 610456,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 364830,  
     &'Topoisomerase I Inh.','camptothecin derivative       ', 606497,  
     &'Topoisomerase I Inh.','morpholinodoxorubicin         ', 354646, 
     &'TopoisomeraseII Inh.','doxorubicin                   ', 123127,         
     &'TopoisomeraseII Inh.','amonafide                     ', 308847,  
     &'TopoisomeraseII Inh.','m-AMSA                        ', 249992,  
     &'TopoisomeraseII Inh.','anthrapyrazole derivative     ', 355644,  
     &'TopoisomeraseII Inh.','pyrazoloacridine              ', 366140,  
     &'TopoisomeraseII Inh.','bisantrene HCL                ', 337766,  
     &'TopoisomeraseII Inh.','daunorubicin                  ',  82151,  
     &'TopoisomeraseII Inh.','deoxydoxorubicin              ', 267469,  
     &'TopoisomeraseII Inh.','mitoxantrone                  ', 301739,  
     &'TopoisomeraseII Inh.','menogaril                     ', 269148,  
     &'TopoisomeraseII Inh.','N,N-dibenzyl daunomycin       ', 268242,  
     &'TopoisomeraseII Inh.','oxanthrazole                  ', 349174,  
     &'TopoisomeraseII Inh.','rubidazone                    ', 164011,  
     &'TopoisomeraseII Inh.','VM-26                         ', 122819,  
     &'TopoisomeraseII Inh.','VP-16                         ', 141540,  
     &'RNA/DNA Antimetab.  ','L-alanosine                   ', 153353,  
     &'RNA/DNA Antimetab.  ','5-azacytidine                 ', 102816,  
     &'RNA/DNA Antimetab.  ','5-fluorouracil                ',  19893,  
     &'RNA/DNA Antimetab.  ','acivicin                      ', 163501,  
     &'RNA/DNA Antimetab.  ','aminopterin derivative        ', 132483,  
     &'RNA/DNA Antimetab.  ','aminopterin derivative        ', 184692,  
     &'RNA/DNA Antimetab.  ','aminopterin derivative        ', 134033,  
     &'RNA/DNA Antimetab.  ','an antifol                    ', 623017,  
     &'RNA/DNA Antimetab.  ','Bakers soluble antifol        ', 139105,  
     &'RNA/DNA Antimetab.  ','dichlorallyl lawsone          ', 126771,  
     &'RNA/DNA Antimetab.  ','brequinar                     ', 368390,  
     &'RNA/DNA Antimetab.  ','ftorafur (pro-drug)           ', 148958,  
     &'RNA/DNA Antimetab.  ','5,6-dihydro-5-azacytidine     ', 264880,  
     &'RNA/DNA Antimetab.  ','methotrexate                  ',    740,  
     &'RNA/DNA Antimetab.  ','methotrexate derivative       ', 174121,  
     &'RNA/DNA Antimetab.  ','N-(phosphonoAc)-L-asp.(PALA)  ', 224131,  
     &'RNA/DNA Antimetab.  ','pyrazofurin                   ', 143095,  
     &'RNA/DNA Antimetab.  ','trimetrexate                  ', 352122,  
     &'DNA Antimetabolites ','3-HP                          ',  95678,  
     &'DNA Antimetabolites ','2s-deoxy-5-fluorouridine      ',  27640,  
     &'DNA Antimetabolites ','5-HP                          ', 107392,              
     &'DNA Antimetabolites ','alpha-TGDR                    ',  71851,  
     &'DNA Antimetabolites ','aphidicolin glycinate         ', 303812,  
     &'DNA Antimetabolites ','ara-C                         ',  63878,  
     &'DNA Antimetabolites ','5-aza-2s-deoxycytidine        ', 127716,  
     &'DNA Antimetabolites ','beta-TGDR                     ',  71261,  
     &'DNA Antimetabolites ','cyclocytidine                 ', 145668,  
     &'DNA Antimetabolites ','guanazole                     ',   1895,  
     &'DNA Antimetabolites ','hydroxyurea                   ',  32065,  
     &'DNA Antimetabolites ','inosine glycodialdehyde       ', 118994,  
     &'DNA Antimetabolites ','macbecin II                   ', 330500,  
     &'DNA Antimetabolites ','pyrazoloimidazole             ',  51143,  
     &'DNA Antimetabolites ','thioguanine                   ',    752,  
     &'DNA Antimetabolites ','thiopurine                    ',    755/  



      n_ref=127

      

c      do i=1,n_ref
c       write(*,*) i,agent_name(i),agent_number(i),action_mode(i)
c      enddo

      n_ref=n_ref+1
      
	open(2, file='out_antimitotics_numbers.txt')

	do icomp=1, n_compound

      action_mode(n_ref)='Unknown             '
	agent_name(n_ref)='Unknown                       '
	agent_number(n_ref)=NSC_pointer_array(icomp)
      write(*,*) 'testing ', icomp, ' compound',' NSC number: ',
     &	agent_number(n_ref) 

      variation1=zero
      do k=1,200
      if (activity_cen(icomp,k).GE.mm)
     &	variation1=variation1+activity_cen(icomp,k)**2
      enddo
      if(variation1.LE.0.00000001) then
	 write(*,*) 'insufficient data for correlation'
      goto 10
      endif

      do i=1,n_ref
	 do j=1,n_ref
	  if (i.LE.127) then 
	    seed1=NSC_number_array(agent_number(i))
        else
	    seed1=icomp
        endif
	  if (j.LE.127) then
          seed2=NSC_number_array(agent_number(j))
        else
	    seed2=icomp
        endif
       covariation=zero
       variation1=zero
	 variation2=zero
	 n_cells=0
	 do k=1,200
	  if (activity_cen(seed1,k).GE.mm.AND.activity_cen(seed2,k).GE.mm)
     &                                                             then
	   covariation=
     &    covariation+activity_cen(seed1,k)*activity_cen(seed2,k)
	   variation1=variation1+activity_cen(seed1,k)**2
	   variation2=variation2+activity_cen(seed2,k)**2
	   n_cells=n_cells+1
        endif
       enddo
       if(variation1.LE.0.00000001.OR.variation2.LE.0.00000001) then
	  correlation(i,j)=-100.0
        n_compare(i,j)=0
        write(*,*) 'warning! insufficient data for correlation!',i,j
       else
        correlation(i,j)=covariation/sqrt(variation1*variation2)
        n_compare(i,j)=n_cells
       endif
  
c      write(*,*) i,j,correlation(i,j),n_compare(i,j)

      enddo
	enddo


c      do k=1,200
c	 write(*,*) '13', activity_cen(NSC_number_array(357704),k)
c      enddo


      min_corr=1.000000
      do i=1,n_ref
	 do j=1,n_ref
        if(correlation(i,j).LE.min_corr) min_corr=correlation(i,j)
       enddo
      enddo
 
c      write(*,*) "minimal correlation: ", min_corr


      do i=1,n_ref
	 do j=1,n_ref
        if(correlation(i,j).LE.zero) correlation(i,j)=zero
       enddo
      enddo

      do i=1,n_ref
       D(i)=zero
	 do j=1,n_ref
        D(i)=D(i)+correlation(i,j)
       enddo
	 do j=1,n_ref
        correlation(i,j)=correlation(i,j)/D(i)
       enddo
      enddo


      CALL EVCRG (n_ref, correlation, 200, eval, evec, 200)

c	write(*,*) 'eigenvalues:'

c	do i=1,n_ref
c	 write(*,*) i,eval(i)
c      enddo


c      do n_clust=2,10
      n_clust=4
	n_dim=1
      write(*,*) '*************************'
	write(*,*) 'N_clust=', n_clust 


      call system_clock(time)
      call srand(time)



      do i=1,n_ref
	 cluster_n(i)=int(float(n_clust)*RAND()) + 1
      enddo


c      do i=1,n_ref
c       cluster_n=2
c	 max_evec=real(evec(i,2))
c       do j=2,n_clust+1 
c        if(real(evec(i,j)).GT.max_evec) then
c	   max_evec=real(evec(i,j))
c         cluster_n=j
c        endif
c       enddo
c      write(*,*) cluster_n-1, action_mode(i),agent_name(i)
c      enddo

      do iter = 1,200

      do i=1,n_clust
       do j=1,n_dim
        centers(i,j) = zero
        number_in_cluster(i)=0
       enddo
      enddo

      do i=1,n_ref
       number_in_cluster(cluster_n(i))=number_in_cluster(cluster_n(i))
     &                                                         	  + 1
       do j=1,n_dim
        centers(cluster_n(i),j) = centers(cluster_n(i),j) + evec(i,j+1)
       enddo
      enddo

      do i=1,n_clust
       do j=1,n_dim
        centers(i,j) = centers(i,j)/float(number_in_cluster(i))
       enddo
      enddo

      convergence=.TRUE. 
      do i=1,n_ref
       cluster_n_old=cluster_n(i)
	 min_distance = 100000000000000000000.0  
       do k=1,n_clust
        distance = zero
	  do j=1,n_dim
         distance = distance + (centers(k,j) - evec(i,j+1))**2
        enddo
        if (distance.LE.min_distance) then
	   min_distance=distance
	   cluster_n(i)=k
        endif
	 enddo
	 if(cluster_n_old.NE.cluster_n(i)) convergence=.FALSE.
      enddo

c      write(*,*) iter,convergence
      enddo      


c      do i=1,n_ref
c      write(*,*) cluster_n(i), action_mode(i),agent_name(i)
c      enddo

      do i=1,n_clust
	cluster_pop(i)=0
	enddo
      do i=35,54
c       write(*,*) i,agent_name(i),agent_number(i),action_mode(i)
       cluster_pop(cluster_n(i))=cluster_pop(cluster_n(i))+1
      enddo

      max_pop=cluster_pop(1)
	max_cluster=1
	 do i=2,n_clust
	  if(cluster_pop(i).GT.max_pop) then
	   max_pop=cluster_pop(i)
	   max_cluster=i
        endif
       enddo

      false_pop=0
	do i=1,34
	 if(cluster_n(i).EQ.max_cluster) false_pop=false_pop+1
      enddo



c      write(*,*) 'max_pop: ', max_pop, 'mean_activity: ', 
c     *	mean_activity(icomp)
c      do i=1,128
c	 write(*,*) i, cluster_n(i)
c      enddo
      
	if(max_pop.GE.10.AND.
     &   cluster_n(128).EQ.max_cluster.AND.
     &   mean_activity(icomp).GE.4.1.AND.
     &   number_over_mean(icomp).GE.10) then
	 write(*,*) 'This compound is strong antimitotic!'
	 n_antimitotics=n_antimitotics+1
	 write(2,*) n_antimitotics, NSC_pointer_array(icomp),
     &	 max_pop,false_pop,number_over_mean(icomp),
     &     mean_activity(icomp) 
      endif



   10 continue  
      enddo


      write(*,*) 'Find ', n_antimitotics, ' antimitotics!'
      close(2)

	return
	end