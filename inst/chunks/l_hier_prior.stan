// chunk giving a hiearchical TS prior to legislators/persons

//add basic integrated time-series prior
for(t in 1:T) {
  if (t==1) {
  L_free ~normal(legis_pred[1, 1:(num_legis - num_constrain_l), ] * legis_x, legis_sd);
  } else if(t==2) {
    L_tp1[1] ~normal(L_full + legis_pred[t, 1:(num_legis), ] * legis_x, legis_sd);
  } else if(t>2) {
    
    L_tp1[t-1] ~normal(L_tp1[t - 2] + legis_pred[t, 1:(num_legis), ] * legis_x, legis_sd);

  }
}
