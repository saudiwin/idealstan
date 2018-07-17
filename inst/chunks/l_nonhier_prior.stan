// chunk giving a non-hiearchical TS prior to legislators/persons

      L_free ~normal(0, legis_sd);
      //add basic integrated time-series prior
      if (T > 1) {
        if(T==2) {
          L_tp1[1] ~normal(L_free, legis_sd);
        }
        for (t in 3:T) {
          L_tp1[t-1] ~normal(Ltp1[t - 2], legis_sd);
        }
      }