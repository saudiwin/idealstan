// if(r_in(3,{1,2,3,4})) will evaluate as 1
int r_in(int pos,int[] pos_var) {
  
  for (p in 1:(size(pos_var))) {
    if (pos_var[p]==pos) {
      // can return immediately, as soon as find a match
      return 1;
    } else {
      return 0;
    }
  }

}