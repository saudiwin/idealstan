functions {
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
              int time_proc = allintdata[31];
              int const_type = allintdata[32];
              int restrict_high = allintdata[33];
              int restrict_low = allintdata[34];
              int fix_high = allintdata[35];
              int fix_low = allintdata[36];
              
              
              int skip = 36; // IDs to skip in integer array
              int skip_param; // number to skip depending on which param is mapped over
              
              // pull outcomes 
              
              int Y_int[N_int] = allintdata[(skip+1):(N_int+skip)];
              int bb[S_type ? N_int: 1]; // one of these will be zero depending on how we mapped it
              int ll[S_type ? 1: N_int];
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
              real time_ind[N_cont] = allcontdata[(LX*N_cont + SRX*N_cont + SAX*N_cont + N_cont+1):(LX*N_cont + SRX*N_cont + SAX*N_cont + 2*N_cont)];
              int cont_skip = LX*N_cont + SRX*N_cont + SAX*N_cont + 2*N_cont;
              real ar_sd = allcontdata[cont_skip+1];
              real diff_reg_sd = allcontdata[cont_skip+2];
              real diff_abs_sd = allcontdata[cont_skip+3];
              real discrim_reg_sd = allcontdata[cont_skip+4];
              real discrim_abs_sd = allcontdata[cont_skip+5];
              real legis_sd = allcontdata[cont_skip+6];
              real restrict_sd = allcontdata[cont_skip+7];
              real gp_sd_par = allcontdata[cont_skip+8];
              real m_sd_par = allcontdata[cont_skip+9];
              real num_diff = allcontdata[cont_skip+10];
              real min_length = allcontdata[cont_skip+11];
              
              // create covariates
              // depends on whether persons or items are mapped over
              // use conditional operator to determine size of vectors
              
              real log_prob = 0; // for incrementing log posterior within the shard
              
              vector[S_type ? 1 : num_legis] L_full; // Always equal to 1 if persons are mapped over regardless of T
              matrix[T,S_type ? 0 : num_legis] L_tp1; // does not exist if persons are mapped over
              vector[(S_type && T>1) ? T : 0] L_tp2; // hold computed time series
              vector[(S_type && T>1) ? T : 0] L_tp1_var; // for non-centering
              vector[S_type ? num_bills : 1] sigma_reg_free; // only 1 if items are mapped over
              vector[S_type ? num_bills : 1] sigma_abs_free;
              vector[S_type ? num_bills : 1] B_int_free;
              vector[S_type ? num_bills : 1] A_int_free;
              vector[LX] legis_x;
              vector[SRX] sigma_reg_x;
              vector[SAX] sigma_abs_x;
              vector[S_type ? 1 : num_ls] ls_int;
              real time_var_full = 0;
              real L_AR1 = 0;
              real m_sd_full = 0;
              real gp_sd_full = 0;
              real time_var_gp_full = 0;
              
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
              
                 return [log_prob]';
               }
}

data {
  int N; // total number of observations
  int N_int; // if outcome is an integer
  int N_cont; // if outcome is continuous
  int T; // number of time points
  int Y_int[N_int]; // integer outcome
  int within_chain; // whether to use map_rect
  real Y_cont[N_cont]; // continuous outcome
  int y_int_miss; // missing value for integers
  real y_cont_miss; // missing value for continuous data
  int S; // number of shards
  int S_int; // shard length for integers
  int S_cont; // shard length for reals
  int S_type; // whether shards are based on persons or items
  int int_shards[S,S_int]; // integer shards
  real cont_shards[S,S_cont]; // real shards
}

transformed data {
	int m;                         // missing value
	real m_cont; // missing value if continuous
	int m_step; // number of ordinal categories
	int num_constrain_l;
	//int num_var_free; // whether to restrict variance parameters
	//int num_var_restrict;
	real num_legis_real; // used to adjust jacobian for mean restriction
	int gp_N; // use for creating zero-length arrays if gp not used
	int gp_N_fix; // same but for fixed parameters
	int gp_1; // zero-length gp-related scalars
	int gp_nT; // used to make L_tp1 go to model block if GPs are used
	int gp_oT; // used to make L_tp1 go to model block if GPs are used
	vector[1] gp_length; 
	int vP;
	int dP;
	
	// set mean of log-normal distribution for GP length-scale prior
	
	if(time_proc==4) {
	  gp_length = gp_prior_mean(time_ind,num_diff,min_length);
	} else {
	  gp_length = [0.0]';
	}
	
	
	//reset these values to use GP-specific parameters
	if(time_proc!=4) {
	  gp_N=0;
	  gp_N_fix=0;
	  gp_1=0;
	  gp_oT=T;
	  gp_nT=0;

	} else {
	  gp_N=num_legis;
	  gp_N_fix=num_legis-1;
	  gp_1=1;
	  gp_nT=T;
	  gp_oT=0;
	}
	
	if(within_chain==1) {
	  if(S_type==1) {
	    // if we map over people, then only one varying parameter per shard
	    if(time_proc==4) {
	      vP = 1*T + 4;
	    } else if(time_proc==3) {
	      vP = 1*T + 3;
	    } else if(time_proc==2) {
	      vP = 1*T + 2;
	    } else if(T==1) {
	      if(num_ls==0) {
	         vP = 1;
	      } else {
	        vP = 2;
	      }
	    }
	    
	    // include one extra for the extra_sd parameter
	    dP = 4*num_bills + (sum(n_cats_rat) - 8) + (sum(n_cats_grm) - 8)*num_bills_grm + LX + SRX + SAX + 1;
	  } else {
	    // 6 parameters for all item parameters
	    vP = 4; 
	    dP = num_legis*T + num_ls + (sum(n_cats_rat) - 8) + (sum(n_cats_grm) - 8)*num_bills_grm + LX + SRX + SAX + 1;
	  }
	  
	} else {
	  vP = 0;
	  dP = 0;
	}

}

parameters {
  vector[num_bills] sigma_abs_free;
  vector[num_legis] L_full; // first T=1 params to constrain
  vector<lower=0>[gp_N_fix] m_sd_free; // marginal standard deviation of GP
  vector<lower=0>[gp_N_fix] gp_sd_free; // residual GP variation in Y
  vector[num_legis] L_tp2[gp_nT]; // additional L_tp1 for GPs only
  vector[num_ls] ls_int; // extra intercepts for non-inflated latent space
  vector[T>1 ? num_legis : 0] L_tp1_var[T-1]; // non-centered variance
  vector<lower=-.99,upper=.99>[(T>1 && time_proc==3) ? num_legis : 0] L_AR1; // AR-1 parameters for AR-1 model
  vector[num_bills] sigma_reg_free;
  //vector[1] restrict_high;
  vector[LX] legis_x;
  vector[SRX] sigma_reg_x;
  vector[SAX] sigma_abs_x;
  vector[num_bills] B_int_free;
  vector[num_bills] A_int_free;
  ordered[n_cats_rat[1]-1] steps_votes3;
  ordered[n_cats_rat[2]-1] steps_votes4;
  ordered[n_cats_rat[3]-1] steps_votes5;
  ordered[n_cats_rat[4]-1] steps_votes6;
  ordered[n_cats_rat[5]-1] steps_votes7;
  ordered[n_cats_rat[6]-1] steps_votes8;
  ordered[n_cats_rat[7]-1] steps_votes9;
  ordered[n_cats_rat[8]-1] steps_votes10;
  ordered[n_cats_grm[1]-1] steps_votes_grm3[num_bills_grm];
  ordered[n_cats_grm[2]-1] steps_votes_grm4[num_bills_grm];
  ordered[n_cats_grm[3]-1] steps_votes_grm5[num_bills_grm];
  ordered[n_cats_grm[4]-1] steps_votes_grm6[num_bills_grm];
  ordered[n_cats_grm[5]-1] steps_votes_grm7[num_bills_grm];
  ordered[n_cats_grm[6]-1] steps_votes_grm8[num_bills_grm];
  ordered[n_cats_grm[7]-1] steps_votes_grm9[num_bills_grm];
  ordered[n_cats_grm[8]-1] steps_votes_grm10[num_bills_grm];
  real<lower=0> extra_sd;
  //real<lower=-.9,upper=.9> ar_fix;
  vector[gp_N_fix] time_var_gp_free;
  vector<lower=0>[(T>1 && time_proc!=4) ? num_legis-1 : 0] time_var_free;
  
}

transformed parameters {

  vector[T>1 ? num_legis : 0] L_tp1[T];
  vector[T>1 ? num_legis : 0] time_var_full;
  vector[gp_N] time_var_gp_full;
  vector[gp_N] m_sd_full;
  vector[gp_N] gp_sd_full;
  vector[vP] varparams[S]; // varying parameters if map_rect
  vector[dP] dparams; // stacked (constant parameters) if map_rect
  
  if(T>1) {
    time_var_full = append_row([time_sd]',time_var_free);
  }
  
    //combine constrained and unconstrained parameters
    //#include /chunks/build_params_v2.stan

  if(within_chain==0 || (within_chain==1 && S_type==0)) {
    if(T>1) {
    if(time_proc==3) {
      // in AR model, intercepts are constant over time
#include /chunks/l_hier_ar1_prior.stan
    } else if(time_proc==2){
      // in RW model, intercepts are used for first time period
#include /chunks/l_hier_prior.stan
    } else if(time_proc==4) {
      L_tp1 = L_tp2; // just copy over the variables, saves code if costs a bit of extra memory
                      // should be manageable memory loss
      m_sd_full = append_row([m_sd_par]',
                              m_sd_free);
      gp_sd_full = append_row([gp_sd_par]',
                              gp_sd_free);
      time_var_gp_full = append_row(gp_length,
                                      time_var_gp_free);
    }  
  } 
  }

  
#include /chunks/map_build_params.stan

}

model {

  //all model types

// use map_rect or don't use map_rect

  target += sum(map_rect(overT,dparams,varparams,cont_shards,int_shards));

}


