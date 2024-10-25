// all print statements for parameters for debug mode

// Print statements for each parameter

print("sigma_abs_free (vector with bounds -0.999 to 0.999, length: ", num_bills, "): ", sigma_abs_free);
print("L_full (vector length: ", num_legis, "): ", L_full);
print("m_sd_free (vector with lower bound 0, length: ", gp_N_fix, "): ", m_sd_free);
print("gp_sd_free (vector with lower bound 0, conditional on time_proc==4): ", gp_sd_free);
print("ls_int (vector length: ", num_ls, "): ", ls_int);
print("ls_int_abs (vector length: ", num_ls, "): ", ls_int_abs);
print("L_tp1_var (array of vectors with conditional length on T and time_proc): ", L_tp1_var);
print("L_AR1 (vector with bounds ar1_down to ar1_up, conditional on T>1 and time_proc==3): ", L_AR1);
print("sigma_reg_free (vector with bounds -0.999 to 0.999, conditional on pos_discrim): ", sigma_reg_free);
// print("sigma_reg_pos (vector with lower bound 0, conditional on pos_discrim): ", sigma_reg_pos); // Uncomment if needed
print("legis_x (vector length: ", LX, "): ", legis_x);
print("sigma_reg_x (vector length: ", SRX, "): ", sigma_reg_x);
print("sigma_abs_x (vector length: ", SAX, "): ", sigma_abs_x);
print("B_int_free (vector length: ", num_bills, "): ", B_int_free);
print("A_int_free (vector length: ", num_bills, "): ", A_int_free);
print("steps_votes3 (ordered vector, length: ", n_cats_rat[1]-1, "): ", steps_votes3);
print("steps_votes4 (ordered vector, length: ", n_cats_rat[2]-1, "): ", steps_votes4);
print("steps_votes5 (ordered vector, length: ", n_cats_rat[3]-1, "): ", steps_votes5);
print("steps_votes6 (ordered vector, length: ", n_cats_rat[4]-1, "): ", steps_votes6);
print("steps_votes7 (ordered vector, length: ", n_cats_rat[5]-1, "): ", steps_votes7);
print("steps_votes8 (ordered vector, length: ", n_cats_rat[6]-1, "): ", steps_votes8);
print("steps_votes9 (ordered vector, length: ", n_cats_rat[7]-1, "): ", steps_votes9);
print("steps_votes10 (ordered vector, length: ", n_cats_rat[8]-1, "): ", steps_votes10);
print("steps_votes_grm3 (array of ordered vectors, length: ", n_cats_grm[1]-1, ", num_bills: ", num_bills, "): ", steps_votes_grm3);
print("steps_votes_grm4 (array of ordered vectors, length: ", n_cats_grm[2]-1, ", num_bills: ", num_bills, "): ", steps_votes_grm4);
print("steps_votes_grm5 (array of ordered vectors, length: ", n_cats_grm[3]-1, ", num_bills: ", num_bills, "): ", steps_votes_grm5);
print("steps_votes_grm6 (array of ordered vectors, length: ", n_cats_grm[4]-1, ", num_bills: ", num_bills, "): ", steps_votes_grm6);
print("steps_votes_grm7 (array of ordered vectors, length: ", n_cats_grm[5]-1, ", num_bills: ", num_bills, "): ", steps_votes_grm7);
print("steps_votes_grm8 (array of ordered vectors, length: ", n_cats_grm[6]-1, ", num_bills: ", num_bills, "): ", steps_votes_grm8);
print("steps_votes_grm9 (array of ordered vectors, length: ", n_cats_grm[7]-1, ", num_bills: ", num_bills, "): ", steps_votes_grm9);
print("steps_votes_grm10 (array of ordered vectors, length: ", n_cats_grm[8]-1, ", num_bills: ", num_bills, "): ", steps_votes_grm10);
print("extra_sd (vector with lower bound 0, length: ", num_var, "): ", extra_sd);
print("time_var_gp_free (vector with lower bound 0, length: ", gp_N, "): ", time_var_gp_free);
print("time_var_free (vector with lower bound 0, conditional on T and time_proc): ", time_var_free);
print("a_raw (array of row_vectors conditional on num_basis > 1): ", a_raw);
