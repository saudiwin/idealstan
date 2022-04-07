

// set values so that we don't end up with undefined data.
m_cont = max(Y_cont);
m = max(Y_int);

if(model_type==4||model_type==6||model_type==8||model_type==14) {
  //integer-based inflated
  m = max(Y_int) - 1;
  // define m_step
  if(model_type<7) {
    m_step = m;
  } else {
    m_step = 2;
  }
  } else if(model_type==10||model_type==12) {
    //continuous inflated
    m_cont = max(Y_cont) - 1;
    m_step = 2;
    m = 2;
  } else if(model_type==3||model_type==5) {
  //normal ordinal models  
    m= max(Y_int);
    m_step = m;
    
  } else if(model_type==1||
            model_type==2||
            model_type==7||
            model_type==9||
            model_type==11||
            model_type==13||model_type==14) {
    //Poisson & continuous non-inflated & binary
    m= 2;
    m_step = 2;
  } else {
    m = 2;
    m_step = 2;
  }

for(n in 1:N) {
  
  if(model_type<9 || model_type==13||model_type==14) {
    //integer models
    if(Y_int[n]>m) {
      absence[n]=1;
    } else {
      absence[n]=0;
    }
  } else {
    //continuous models
    if(Y_cont[n]>m_cont) {
      absence[n]=1;
    } else {
      absence[n]=0;
    }
  }
  if(model_type==1||model_type==2||model_type==13||model_type==14) {
    //need to change outcome for binomial models
    if(min(Y_int)!=0) {
      Y_new[n] = Y_int[n] - 1;
    } else {
      Y_new[n] = Y_int[n];
    }
  }
}