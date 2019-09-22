require(ggplot2); require(dplyr); require(ggsci); require(gridExtra); require(grid); require(tidyr)

insert_minor <- function(major_labs, n_minor) {
  labs <- c(sapply(major_labs, function(x) c(x, '')))
  labs[1:(length(labs)-n_minor)]
}

mytheme1 <- theme_bw() +  theme(axis.line = element_line(colour = "black"),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                panel.background = element_blank())

hist_by_ROH_length = function(data, skip = 0, readFromFile = T){
  KB_group = function(x){
    if(x < 1)
      return('0-1')
    else if(x < 2)
      return('1-2')
    else if(x < 4)
      return('2-4')  
    else if(x < 8)
      return('4-8')
    else if(x < 16)
      return('8-16')
    else 
      return('16+')
  }
  
if(readFromFile == T){
  
  # grab colnames
  names = readLines(data, n = 1)
  names = unlist(strsplit(names, split = '\\s+'))
  names = names[names != '']
  
  # read in table and get rid of excess rows 
  data = read.table(file = data, col.names = names, skip = skip)
  knock = grep(data[,1], pattern = 'FID') 
  data = data[-knock, ]
  for(i in unique(data[1]))
  
  # plot hist 
  data[,9] = as.numeric(as.character(data[,9]))/1000
  
  data$KB_grouped = sapply(data[,9], KB_group)
  KB_mean = group_by(data, KB_grouped, FID)
  KB_mean = data.frame(summarize(KB_mean, length(KB)))
  
  FID = vector(); KB_g = NULL; ROH_count = NULL
  for(i in 1:length(unique(KB_mean[,1]))){
    for(j in 1:length(unique(KB_mean[,2]))){
      test1 = data[data[,1] == unique(KB_mean[,2])[j], ] 
      ROH_len = length(test1[test1$KB_grouped == unique(KB_mean[,1])[i], 9])
      FID = append(FID, as.character(unique(KB_mean[,2]))[j])
      KB_g = append(KB_g, unique(KB_mean[,1])[i])
      ROH_count = append(ROH_count, ROH_len)
    }
  }
  KB_data = data.frame(FID, KB_g, ROH_count)
  
  ggplot(KB_data, aes(y = ROH_count, x = KB_g, fill = FID)) + 
    geom_bar(stat = "identity", position = 'dodge') + scale_fill_npg() + mytheme1 + 
    ylab('Number of ROHs') + xlab('Length of ROH (Mb)') + 
    scale_y_continuous(expand = c(0,0), limits = c(0, max(KB_data[,3*1.1])))
  
}
  
# if data.frame is in R already
else{
  data[,9] = as.numeric(as.character(data[,9]))/1000
  data$KB_grouped = sapply(data[,9], KB_group)
  KB_mean = group_by(data, KB_grouped, FID)
  KB_mean = data.frame(summarize(KB_mean, length(KB)))
  
  FID = vector(); KB_g = NULL; ROH_count = NULL
  for(i in 1:length(unique(KB_mean[,1]))){
    for(j in 1:length(unique(KB_mean[,2]))){
      test1 = data[data[,1] == unique(KB_mean[,2])[j], ] 
      ROH_len = length(test1[test1$KB_grouped == unique(KB_mean[,1])[i], 9])
      FID = append(FID, as.character(unique(KB_mean[,2]))[j])
      KB_g = append(KB_g, unique(KB_mean[,1])[i])
      ROH_count = append(ROH_count, ROH_len)
    }
  }
  KB_data = data.frame(FID, KB_g, ROH_count)
  
  ggplot(KB_data, aes(y = ROH_count, x = KB_g, fill = FID)) + 
    geom_bar(stat = "identity", position = 'dodge') + scale_fill_npg() + mytheme1 + 
    ylab('Number of ROHs') + xlab('Length of ROH (Mb)') + 
    scale_y_continuous(expand = c(0,0), limits = c(0, max(KB_data[,3*1.1])))
}
}

hist_by_ROH_length('species.txt')

names = readLines('species.txt', n = 1)
names = unlist(strsplit(names, split = '\\s+'))
names = names[names != '']

# read in table and get rid of excess rows 
test = read.table(file = 'species.txt', col.names = names)
knock = grep(test[,1], pattern = 'FID') 
test = test[-knock, ]

test[,9] = as.numeric(as.character(test[,9]))/1000

test$KB_grouped = sapply(test[,9], KB_group)

KB_mean = group_by(test, KB_grouped, FID)
KB_mean = data.frame(summarize(KB_mean, length(KB)))

KB_mean

FID = vector(); KB_g = NULL; ROH_count = NULL
for(i in 1:length(unique(KB_mean[,1]))){
  print(unique(KB_mean[,1])[i])
  for(j in 1:length(unique(KB_mean[,2]))){
    test1 = test[test[,1] == unique(KB_mean[,2])[j], ] 
    ROH_len = length(test1[test1$KB_grouped == unique(KB_mean[,1])[i], 9])
    print(ROH_len)
    FID = append(FID, as.character(unique(KB_mean[,2]))[j])
    KB_g = append(KB_g, unique(KB_mean[,1])[i])
    ROH_count = append(ROH_count, ROH_len)
    #ROH_count[i*j] = ROH_len
  }
}
FID
KB_g
ROH_count
KB_data = data.frame(FID, KB_g, ROH_count)
KB_data

lengths = NULL
for(i in unique(KB_mean[,1])){
  #print(i)
  #for(i in KB_mean[,2]){
   # if()
  #}
  for(j in unique(KB_mean[,2])){
   #print(KB_mean[,1] == i & KB_mean[,2] == j,)
   if(KB_mean[KB_mean[,2] == j,] %in% i)
     print(j)
   # if(KB_mean[,3] )
  }
}

KB_mean[KB_mean[,2],] == 'camel'
        
KB_mean[KB_mean[,2] == 'camel' & KB_mean[,1] == '0-1', ]

test = test[test[,1] == 'camel', ] 
length(test[test$KB_grouped == '1-2', 9])

# F in relation to # of ROHs

F_vals = rnorm(100, .15, .05)
ROHs = sample(25:400, 100)
species = sample(c('L. lutra', 'P. macrocephalus', 'D. delphinus', 'D. dugong'),
                 100, replace = T)
IUCN = sample(c('DD', 'VU', 'EN', 'LE'), 100, replace = T)
test = data.frame(species, sort(ROHs), sort(F_vals), IUCN)

group_ROH = function(data, facet, col){
  # data is data frame
  # grouping is how plots will be split (by species, taxonomic group ,etc)
    # needs to be in facet form
  # color is to tell what factor will be for color (IUCN, location, etc)
  ggplot(data, aes(x = F_vals, y = ROHs, color = col)) + 
    facet_grid(facet) + geom_point() + theme_bw() + scale_color_npg() +
    ylab('Number of autozygos regions') + xlab('F_val')
}

group_ROH(test, facet = ~species, col = IUCN)

# heterzygosity and length of ROHs

species = c(rep(c('L. lutra', 'P. macrocephalus', 'D. delphinus', 'D. dugong'), 2))
hetero = c(.001, .003, .009, .004, .003, .006, .0160, .005)
ROH_tf = c(rep('T', 4), rep('F', 4)) 
spec1 = c(rep('L. lutra', 5), rep('P. macrocephalus', 5), rep('D. delphinus', 5),
        rep('D. dugong', 5))
ROHs = rep(c(200, 100, 50, 150),5)

hetDat = data.frame(species, hetero, ROH_tf)
ROHd = data.frame(spec1, ROHs)

ROHd = group_by(ROHd, spec1)
ROHd = data.frame(summarize(ROHd, sum(ROHs)))
ROHd$sum.ROHs. = ROHd$sum.ROHs./100000 

hetDat = data.frame(species, hetero, ROH_tf, length = ROHd$sum.ROHs.)

heterozygosity = function(hetero.dat){
  p = ggplot(hetDat) +
    geom_bar(aes(x = species, y = hetero, fill = ROH_tf),
             stat = "identity", position = 'dodge') + scale_fill_npg() +
    geom_line(aes(x = species, y = length), group = 1, size = 1) + 
    mytheme1 + scale_y_continuous('Heterozygousity',
                                  sec.axis = sec_axis(~ . * 100000, 
                                          name = 'Total length of ROHs (Mb)')) +
    xlab('Species') 
  p
}

heterozygosity(hetDat)  
