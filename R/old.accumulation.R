#old.accumulation = function(window = 0, codes, group, units, sList,gList,uList, stanzas, binary = T) {
old.accumulation = function(
  group, stanzas, codes, units,
  gList, uList, sList,
  forwardValues,
  binary=T, accumTraj=T, sphereNorm=T,
  vmeta, plus1, window,
  correction,
  envName, windowSort=NULL
){
  clean.factors= function(df) {
    for(i in 1:ncol(df)) {
      z=df[,i]
      if(class(z)=="factor") {
        z=as.character(z)
      }
      if(!is.numeric(z)) {
        if(grepl("^[[:digit:].]*$", paste(z,collapse=""))) {
          z=as.numeric(z)
        }
      }
      df[,i] = z
    }
    return(df)
  }

  group = clean.factors(as.data.frame(group));
  for(i in 1:ncol(group)) {
    group[,i] = as.character(group[,i]);
  }
  concatenatedGroup = apply(group, 1, paste, collapse = " & ");

  units = clean.factors(as.data.frame(units));
  for(i in 1:ncol(units)) {
    units[,i] = as.character(units[,i]);
  }
  concatenatedUnit = apply(units, 1, paste, collapse = " & ");

  stanzas = clean.factors(as.data.frame(stanzas));
  for(i in 1:ncol(stanzas)) {
    stanzas[,i] = as.character(stanzas[,i]);
  }
  concatenatedStanza = apply(stanzas, 1, paste, collapse = " & ");

  codes = clean.factors(as.data.frame(codes));

  output = list();
  # save inputs
  # note: some of these aren't used or are duplicate.
  output$more.info$data.used$meta.group = group;
  output$more.info$data.used$meta.stanzas = stanzas;
  output$more.info$data.used$meta.units = units;
  output$more.info$data.used$codes = codes;
  output$more.info$data.used$groupList = gList;
  output$more.info$data.used$unitsList = uList;
  output$more.info$data.used$stanzasList = sList;
  output$stanzaWindowSize = window;
  output$stanzaWindowSortBy = windowSort;
  output$plus1 = FALSE;
  output$adjacency.key = make.adjacency.labels(colnames(codes));


  if(window >= 0) {
    # co-occur each stanza based on window size
    coOccuredCodes = matrix(0, nrow(codes), choose(ncol(codes), 2));
    for(stanzaValue in sList) {
      stanzaCodes = codes[which(concatenatedStanza == stanzaValue),];

      for(i in 1:nrow(stanzaCodes)) {
        contextRows = (i - window):(i - 1);
        contextRows = contextRows[contextRows > 0];
        contextCodes = colSums(data.matrix(stanzaCodes[c(contextRows,i),]));
        contextCodes[contextCodes > 0] = 1;
        coOccuredMatrix = as.numeric(stanzaCodes[i,]) %*% t(contextCodes);
        # all the data is in the matrix, but we need to move all of it to the upper triangle
        coOccuredMatrix = coOccuredMatrix + t(coOccuredMatrix);

        coOccuredVector = coOccuredMatrix[upper.tri(coOccuredMatrix)];
        if(binary) {
          coOccuredVector[coOccuredVector > 0] = 1;
        }
        if(stanzaValue == "3 & Electric") {
        }
        # the input data is not necessarily ordered by conversation so when we compute the cooccurances
        # we keep them in the same order as the input data.
        coOccuredCodes[which(concatenatedStanza == stanzaValue)[i],] = coOccuredVector;
      }
    }
    # accumulate co-occured codes saving sums for each group + unit pair.
    output$data = as.data.frame(matrix(0, 0, choose(ncol(codes), 2)));
    #output$meta = as.data.frame(matrix("", 0, ncol(group) + ncol(units)), stringsAsFactors = FALSE);

    for(groupValue in gList) {
      groupRows = which(concatenatedGroup == groupValue);

      if(!(identical(group, units) && identical(gList, uList))) { # trajectory set
        # don't want to interpolate points if non-trajectory set
        unitVals = uList;
      } else { # non-trajectory set
        unitVals = groupValue;
      }

      for(unitValue in unitVals) {
        #print(paste("uV:", unitValue))

        if(unitValue == unitVals[1] || !accumTraj)	{
          unitCoOccuredCodes = numeric(ncol(coOccuredCodes));
        }
        unitRows = which(concatenatedUnit == unitValue);
        if(length(unitRows[unitRows %in% groupRows]) > 0) {
          unitCoOccuredCodes = unitCoOccuredCodes + colSums(data.matrix(as.data.frame(coOccuredCodes[unitRows[unitRows %in% groupRows],])));
        }
        output$data = rbind(output$data, unitCoOccuredCodes);
        unitMetadata = rbind(c(group[groupRows[1],], units[unitRows[1],]));
        colnames(unitMetadata) = c(colnames(group), colnames(units));
        rownames(unitMetadata) = unitRows[1];
        output$meta = rbind(output$meta, unitMetadata);
      }
    }
  } else {
    # no stanza window used, co-occur codes from each unit with each other utterance from that group in the given unit
    output$data = as.data.frame(matrix(0, 0, choose(ncol(codes), 2)));
    output$meta = as.data.frame(matrix("", 0, ncol(group) + ncol(units)), stringsAsFactors = FALSE);
    for(groupValue in gList) {
      groupRows = which(concatenatedGroup == groupValue);

      if(!(identical(group, units) && identical(gList, uList))) { # trajectory set
        # don't want to interpolate points if non-trajectory set
        unitVals = uList;
      } else { # non-trajectory set
        unitVals = groupValue;
      }
      for(unitValue in unitVals) {
        if(unitValue == unitVals[1] || !accumTraj)	{
          coOccuredVector = numeric(choose(ncol(codes), 2));
        }
        unitRows = which(concatenatedUnit == unitValue);

        for(stanzaValue in sList) {
          stanzaRows = which(concatenatedStanza == stanzaValue);
          stanzaRows = stanzaRows[stanzaRows %in% unitRows[unitRows %in% groupRows]];

          summedStanzaCodes = colSums(data.matrix(as.data.frame(codes[stanzaRows,])));
          coOccuredMatrix = summedStanzaCodes %*% t(summedStanzaCodes);
          # all the data is in the matrix, but we need to move all of it to the upper triangle
          coOccuredMatrix = coOccuredMatrix + t(coOccuredMatrix);
          if(binary) {
            coOccuredMatrix[coOccuredMatrix > 0] = 1;
          }
          coOccuredVector = coOccuredVector + coOccuredMatrix[upper.tri(coOccuredMatrix)];
        }
        output$data = rbind(output$data, coOccuredVector);

        unitMetadata = rbind(c(group[groupRows[1],], units[unitRows[1],]));
        colnames(unitMetadata) = c(colnames(group), colnames(units));
        rownames(unitMetadata) = unitRows[1];
        output$meta = rbind(output$meta, unitMetadata);
      }
    }
  }

  return(output);
}
