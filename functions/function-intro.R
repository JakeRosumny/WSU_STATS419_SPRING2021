is.wholenumber = function(x, tol = .Machine$double.eps^0.5)
{
  abs(x - round(x)) < tol;
}

handShake = function(n=1, plotMe=TRUE) 
{
  if(n < 1) { stop ("n must be greater than 0");}
  if(!is.wholenumber(n)) { stop("n must be an integer"); }
  h = n*(n-1)/2;
  if(plotMe) 
  {
    plot(n,h, main = "Unique Handshakes per # of People", xlab = "# of People", ylab = "# of Handshakes");  
  }
}

which.github = "local"; 
github.local = "C:/_git_/github/JakeRosumny/WSU_STATS419_SPRING2021/" 
github.remote = "https://github.com/JakeRosumny/WSU_STATS419_SPRING2021/" 
if(which.github == "remote")
{
  include.me = paste0(github.remote, "functions/function-intro.R");
  library(devtools);
  source_url(include.me);
} else {
  include.me = paste0(github.local, "functions/function-intro.R");
  source(include.me);
}
path.declaration = paste0(github.local, "datasets/declaration/"); 
charInstanceCounter = function(str,letter)
{
  nchar(as.character(str)) -nchar(gsub(letter, "", str,fixed=TRUE));
}
atozCounter = function(str)
{
  str=gsub("[[:space:]]", "", str) 
  str=tolower(str)
  df=data.frame(matrix(0, nrow=1, ncol=27, byrow=TRUE)) 
  colnames(df)=c(letters,"OTHER") 
  for (letter in letters)
  {
    idx = which(letters == letter); 
    df[1,idx] = charInstanceCounter(str,letter); 
    str = gsub(letter,"", str, fixed=TRUE);
  }
  df[1,27] = nchar(str); 
}

computeDeterminant = function(x = matrix)
{
  for (i in 1:ncol(x))
  {
    if (i == 1)
    {
      a = x[1,1] * ((x[2,2] * x[3,3])-(x[2,3] * x[3,2]));
    }
    if (i == 2)
    {
      b = x[1,2] * ((x[2,1] * x[3,3])-(x[2,3] * x[3,1]));
    }
    if (i == 3)
    {
      c = x[1,3] * ((x[2,1] * x[3,2])-(x[3,1] * x[2,2]));
    }
  }
  det3 = a - b + c;
  print(det3)
}