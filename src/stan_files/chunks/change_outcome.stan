if(model_type==4||model_type==6||model_type==8) {
  //count down one if model is inflated
  m = max(Y_int) - 1;
  // define m_step
  if(model_type!=8) {
    m_step = m;
  } else {
    m_step = 2;
  }
} else if(model_type==10||model_type==12) {
  //count down one if model is inflated
  m = max(Y_cont) - 1;
  m_step = 2;
} else if(model_type==1||model_type==2) {
  //binary models
  m = 2;
  m_step = 2;
} else {
  m= max(Y_int);
  m_step = m;
}

for(n in 1:N) {
  
  if(model_type<9) {
    if(Y_int[n]>m) {
      absence[n]=1;
    } else {
      absence[n]=0;
    }
  } else {
    if(Y_cont[n]>m) {
      absence[n]=1;
    } else {
      absence[n]=0;
    }
  }
  if(model_type==1) {
    //need to change outcome for binomial models
    if(max(Y)==2) {
      Y_new[n] = Y[n] - 1;
    } else {
      Y_new[n] = Y[n];
    }
  } else if(model_type==2) {
    if(max(Y)==3) {
      Y_new[n] = Y[n] - 1;
    } else {
      Y_new[n] = Y[n];
    }
  }
}