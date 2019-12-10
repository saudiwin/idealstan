 vector overT(vector allparams, vector s_pars,
               real[] allcontdata, int[] allintdata) {
                 
              // first do IDs that are all in fixed positions
              int S_type = allintdata[1];
              int N_cont = allintdata[2];
              int N_int = allintdata[3];
              int y_int_miss = allintdata[4];
              int y_cont_miss = allintdata[5];
              int num_legis = allintdata[6];
              int num_bills = allintdata[7];
              int num_bills_grm = num_bills;
              int num_ls = allintdata[8];
              int T = allintdata[9];
              int mod_count = allintdata[10];
              int LX = allintdata[11];
              int SRX = allintdata[12];
              int SAX = allintdata[13];
              int n_cats_rat[8] = allintdata[14:21];
              int n_cats_grm[8] = allintdata[22:29];
              
              int skip = 29; // IDs to skip in integer array
              int skip_param; // number to skip depending on which param is mapped over
              
              // pull outcomes 
              
              int Y_int[N_int] = allintdata[(skip+1):(N_int+skip)];
              int bb[S_type ? N_int: 0]; // one of these will be zero depending on how we mapped it
              int ll[S_type ? N_int: 0];
              int time[N_int] = allintdata[((2*N_int+skip)+1):(3*N_int+skip)];
              int mm[N_int] = allintdata[((3*N_int+skip)+1):(4*N_int+skip)];
              int order_cats_rat[N_int] = allintdata[((4*N_int+skip)+1):(5*N_int+skip)];
              int order_cats_grm[N_int] = allintdata[((5*N_int+skip)+1):(6*N_int+skip)];
              int pad_id[N_int] = allintdata[((6*N_int+skip)+1):(7*N_int+skip)];
              int discrete[N_int] = allintdata[((7*N_int+skip)+1):(8*N_int+skip)];
                 
              // pull real data
              
              real Y_cont[N_cont] = allcontdata[1:N_cont];
              matrix[N_cont,LX] legis_pred = to_matrix(allcontdata[(N_cont+1):(LX*N_cont+N_cont)],N_cont,LX);
              matrix[N_cont,SRX] srx_pred = to_matrix(allcontdata[(LX*N_cont + N_cont+1):(LX*N_cont + SRX*N_cont + N_cont)],N_cont,SRX);
              matrix[N_cont,SAX] sax_pred = to_matrix(allcontdata[(LX*N_cont + SRX*N_cont + N_cont+1):(LX*N_cont + SRX*N_cont + SAX*N_cont + N_cont)],N_cont,SAX);
              
              // create covariates
              // depends on whether persons or items are mapped over
              // use conditional operator to determine size of vectors
              
              real log_prob = 0; // for incrementing log posterior within the shard
              
              vector[S_type ? T : (num_legis*T)] L_full; // T is always 1 or greater
              matrix[T,S_type ? 0 : num_legis] L_tp1; // does not exist if persons are mapped over
              vector[S_type ? num_bills : 1] sigma_reg_free; // only 1 if items are mapped over
              vector[S_type ? num_bills : 1] sigma_abs_free;
              vector[S_type ? num_bills : 1] B_int_free;
              vector[S_type ? num_bills : 1] A_int_free;
              vector[LX] legis_x;
              vector[SRX] sigma_reg_x;
              vector[SAX] sigma_abs_x;
              vector[num_ls] ls_int;
              
              // all of our ordered categories, oh hurray
              
              vector[n_cats_rat[1]-1] steps_votes3;
              vector[n_cats_rat[2]-1] steps_votes4;
              vector[n_cats_rat[3]-1] steps_votes5;
              vector[n_cats_rat[4]-1] steps_votes6;
              vector[n_cats_rat[5]-1] steps_votes7;
              vector[n_cats_rat[6]-1] steps_votes8;
              vector[n_cats_rat[7]-1] steps_votes9;
              vector[n_cats_rat[8]-1] steps_votes10;
              vector[n_cats_grm[1]-1] steps_votes_grm3[num_bills_grm];
              vector[n_cats_grm[2]-1] steps_votes_grm4[num_bills_grm];
              vector[n_cats_grm[3]-1] steps_votes_grm5[num_bills_grm];
              vector[n_cats_grm[4]-1] steps_votes_grm6[num_bills_grm];
              vector[n_cats_grm[5]-1] steps_votes_grm7[num_bills_grm];
              vector[n_cats_grm[6]-1] steps_votes_grm8[num_bills_grm];
              vector[n_cats_grm[7]-1] steps_votes_grm9[num_bills_grm];
              vector[n_cats_grm[8]-1] steps_votes_grm10[num_bills_grm];
              
              real extra_sd; // marginal variance for continuous distributions
              
              int skip0; // convenience counter for ordinal variables
              
              if(S_type==1) {
                
                bb = allintdata[(N_int+skip+1):(2*N_int+skip)];
                
                L_full = s_pars;
                
                sigma_reg_free = allparams[1:num_bills];
                B_int_free = allparams[(num_bills+1):(2*num_bills)];
                sigma_abs_free = allparams[((2*num_bills)+1):(3*num_bills)];
                A_int_free = allparams[((3*num_bills)+1):(4*num_bills)];
                
                skip_param = 4*num_bills;
                
              } else {
                
                ll = allintdata[(N_int+skip+1):(2*N_int+skip)];
                
                // items mapped over
                sigma_reg_free[1] = s_pars[1];
                B_int_free[1] = s_pars[2];
                sigma_abs_free[1] = s_pars[3];
                A_int_free[1] = s_pars[4];
                
                if(T==1) {
                  L_full = allparams[(num_elements(allparams)-num_legis-1):num_elements(allparams)];
                } else {
                  L_tp1 = to_matrix(allparams[(num_elements(allparams)-(T*num_legis)-1):num_elements(allparams)],T,num_legis);
                }
                
                skip_param = 0; // person parameters appended to the end of the vector
              }
              
              //only used for latent space modeling
              if(num_ls>0) {
                ls_int = allparams[(skip_param+1):(skip_param+num_ls)];
              }
              // hierarchical covariates
              legis_x = allparams[(1+skip_param+num_ls+sum(n_cats_rat)-8):(skip_param+num_ls+sum(n_cats_rat)-8+LX)];
              sigma_reg_x = allparams[(1+skip_param+num_ls+sum(n_cats_rat)-8+LX):(skip_param+num_ls+sum(n_cats_rat)-8+LX+SRX)];
              sigma_abs_x = allparams[(1+skip_param+num_ls+sum(n_cats_rat)-8+LX+SRX):(skip_param+num_ls+sum(n_cats_rat)-8+LX+SRX+SAX)];
              
              extra_sd = allparams[(skip_param+num_ls+sum(n_cats_rat)-8+LX+SRX+SAX)+1];
              // put the ordered category reconstruction in a separate file because it's such a pain
#include /chunks/map_reconstruct_ord.stan
              
                 
            // now we are going to use the actual modeling code except adjust it to return a log probability vector
            // increment with addition on the log scale
            
            if(S_type==1) {
              // map over person IDs
#include /chunks/model_types_mm_map_persons.stan              
            } else {
              //map over items
#include /chunks/model_types_mm_map_items.stan              
            }
                  


                // must return as a row vector
                // log probability for shard
                 return [log_prob]';
               }