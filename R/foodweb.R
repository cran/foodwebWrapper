#' foodwebWrapper
#'
#' @import tools
#' @import utils
#' @import tibble
#' @import dplyr
#' @import R2HTML
#' @import textshaping
#' @import magrittr
#' @import tidyverse
#' @import stringr
#' @import grDevices
#' @import graphics
#'
#' @description wrapper for the function foodweb()
#' concatenate the R package name for each retrieved R function
#'
#' @param where position(s) on search path, or an environment, or a list of environments
#' @param ofile character string containing path name for output file
#' @param zeros Boolean if TRUE delete rows and cols that contain all 0's
#' @param pawn Boolean if TRUE use chess symbols rather than rectangles in html table
#' @param verbose Boolean if TRUE output several user messages
#'
#' @details if where is missing, then the user is presented with the option
#'  of choosing from a list of attached packages
#'
#' @examples
#' if(interactive()){
#' #load("data/x_packages.RData")
#' ofile<-sprintf("%s/foodwebWrapper.html",tempdir())
#' foodwebWrapper(ofile=ofile)
#' foodwebWrapper(where=x_packages,ofile=ofile)
#' }
#'
#' @return foodweb returns an object of (S3) class foodweb. This has three components:
#' \itemize{
#'    \item funmat a matrix of 0s and 1s showing what (row) calls what (column). The dimnames are the function names.
#'    \item x shows the x-axis location of the centre of each function's name in the display, in par("usr") units
#'    \item level shows the y-axis location of the centre of each function's name in the display, in par("usr") units. For small numbers of functions, this will be an integer; for larger numbers, there will some adjustment around the nearest integer
#' }
#'
#' @export
foodwebWrapper<-
  function(where=character(0),ofile="~/foodwebWrapper.html",zeros=TRUE,pawn=FALSE,verbose=TRUE) {

    if(verbose)
      message(sprintf("foodwebWrapper(): ofile is %s",ofile))

    if(length(where)>0)
    	l<-attachedFunctionsBatch(where)
    else
    	l<-attachedFunctions(verbose=verbose)

    where<-l$where
    x_where<-where
    #save(x_where,file="x_where.RData")
    packages<-setdiff(unique(unlist(strsplit(where,"package:",TRUE))),"")
    x_packages<-packages
    #save(x_packages,file="x_packages.RData")

	# packages is like
	# [1] "relaxDriver"                   "playWholeHandDriverPassParams" "heartsCIM"

    # next list is the original list that I used until around December 8, 2023
    #colors<-c("darkmagenta","darkolivegreen","darkorange3","brown4","red","blue","black","magenta","chocolate4")
	# next list comes from https://sashamaps.net/docs/resources/20-colors/
	# I omit white from the original list
	colors<-c("red","green","blue","orange","purple","magenta","lime","pink","teal",
		"brown","beige","maroon","mint","olive","apricot","navy","grey","black","lavender","yellow","cyan")

	# value of where should be like where="package:logos" NOT like where="logos"
    x<-foodweb(where=where,plotting=FALSE)
    x_x<-x
    #save(x_x,file="x_x.RData")

    funs<-colnames(x$funmat)
    x_funs<-funs
    #save(x_funs,file="x_funs.RData")

    if(length(funs)<2)
      stop("Not able to complete the wrapper portion:\nThe number of functions found by foodweb() was too low!")

    v<-find_funz(packages,funs)
    x_v<-v
    #save(x_v,file="x_v.RData")

	# v is like
    #                           augmentedProbs                            checkConverge
    #                      "relaxDriver"                            "relaxDriver"

    m<-concatPackFunc2(x$funmat,v)
    x_m<-m
    #save(x_m,file="x_m.RData")

    v2<-consolidate(v)
    x_v2<-v2
    #save(x_v2,file="x_v2.RData")

    m<-rearrangeM(m,v2)
    x_m2<-m
    #save(x_m2,file="x_m2.RData")
    m<-t(rearrangeM(t(m),v2))
    x_m3<-m
    #save(x_m3,file="x_m3.RData")

    f<-sprintf("%s/r2html",dirname(ofile))
    x_f<-f
    #save(x_f,file="x_f.RData")
    rownames(m)<-NULL
    colnames(m)<-NULL
    x_m4<-m
    #save(x_m4,file="x_m4.RData")

    if(zeros) {
      m<-removeZeroRowsCols(m)
      x_m5<-m
      #save(x_m5,file="x_m5.RData")
    }

    colorMap<-mapFunctionsColors(m[1,c(-1,-2)],m[c(-1,-2),1],colors)
    x_colorMap<-colorMap
    #save(x_colorMap,file="x_colorMap.RData")

    R2HTML::HTML(m,file=f,append=FALSE)
    x<-readLines(f)
    x_x2<-x
    #save(x_x2,file="x_x2.RData")

    y<-addStyle(x,m,colorMap,pawn)
    x_y<-y
    #save(x_y,file="x_y.RData")

    writeLines(y,ofile)

    sys<-sprintf("open %s",ofile)
    system(sys)
  }

#' attachedPackages
#'
#' @description print a list of attached packages for the user to select from
#'
#' @examples
#' if(interactive()){
#' attachedPackages()
#' }
#'
#' @return returns a character vector of selected packages
#'
#' @export
attachedPackages<-
  function() {
    where<-vector("character")
    s<-search()
    print(s)
    for(i in 1:length(s)) {
      x<-as.integer(readline("Enter number of desired package to search or 0 when finished: "))
      if(x>=1 & x<=length(s)) {
        where[i]<-s[x]
        message(s[x])
      }
      else
        break
    }
  return(where)
  }

#' attachedFunctions
#'
#' @description print a list of attached packages and
#'  their functions for the user to select from
#'
#' @param verbose Boolean if TRUE output several user messages
#'
#' @examples
#' if(interactive()){
#' attachedFunctions(verbose=TRUE)
#' }
#'
#' @return returns a list whose components are
#' \itemize{
#'    \item l list of user-selected packages and corresponding functions
#'    \item where character vector of selected packages
#'  }
#' @export
attachedFunctions<-
  function(verbose) {
    l<-list()
    l2<-list()
    s<-search() # retrieve attached packages
    if(verbose) {
      message("\nPackages and environments to be included in the search\n")
      message("If a package that you want to include is missing,\nthen run install.packages() and/or library()\n")
    }
    print(s)
    for(i in 1:length(s)) {
      x<-as.integer(readline("Enter number of desired package to list functions or 0 when finished: "))
      if(x>=1 & x<=length(s)) {
        ind<-0
        l[[s[x]]]<-vector("character")
        f<-ls(s[x])
        print(f)
        for(j in 1:length(f)) {
          y<-as.integer(readline("Enter number of desired function or 0 when finished: "))
          if(y>=1 & y<=length(f)) {
            ind<-ind+1
            l[[s[x]]][ind]<-f[y]
          }
          else
            break
        }
      }
      else
        break
    }
    l2$l<-l
    l2$where<-names(l)

    return(l2)
  }

#' attachedFunctionsBatch
#'
#' @description same as attachedFunctions() but no user interaction needed
#'
#' @param packs list of character strings containing the names of packages
#'	package name is like "pack", not like "package:pack"
#'
#' @examples
#' if(interactive()){
#' attachedFunctionsBatch(c("SherlockHolmes","textBoxPlacement"))
#' }
#'
#' @return returns a list whose components are
#' \itemize{
#'    \item l list of user-selected packages and corresponding functions
#'    \item where character vector of selected packages
#'  }
#' @export
attachedFunctionsBatch<-
  function(packs) {
  	l<-list()
  	l2<-list()
    for(pack in packs)
        l[[sprintf("package:%s",pack)]]<-list()

    l2$l<-l
    l2$where<-names(l)

    return(l2)
  }

#' find_funz
#'
#' @description determine in which R package a function ‘resides’
#'
#' @param packs list of character strings containing the names of the packages
#' @param rfuns list of character strings containing the names of functions in
#'  packs to which the result is to be restricted
#'
#' @examples
#' if(interactive()){
#' #load("data/x_packages.RData")
#' #load("data/x_funs.RData")
#' find_funz(packs=x_packages,rfuns=x_funs)
#' }
#'
#' @return returns vector of character strings, names are functions and values are packages
#'
#' @export
find_funz<-
  function(packs,rfuns) {
    v<-vector("character")
    for(pack in packs) {
      funs<-getNamespaceExports(pack)
      for(f in funs)
        if(f %in% rfuns)
          v[f]<-pack
    }
    return(v)
  }

#' concatPackFunc2
#'
#' @description match the package names with the function names
#'
#' @param m character matrix return value component $funmat of foodweb()
#' @param v character vector of package names returned by find_funz()
#'
#' @examples
#' if(interactive()){
#' #load("data/x_x.RData")
#' #load("data/x_v.RData")
#' m<-concatPackFunc2(x_x$funmat,x_v)
#' }
#'
#' @return returns augmented character matrix m
#'
#' @export
concatPackFunc2<-
  function(m,v) {
    m<-rbind(colnames(m),m)
    m<-rbind(v,m)
    m<-cbind(c("",rownames(m)[-1]),m)
    m<-cbind(c("","",v),m)

    return(m)
  }

#' consolidate
#'
#' @description create a permutation list of package names for re-ordering
#'  rows and columns of matrix m, in decreasing order of function counts per package
#'
#' @param v character vector of package names component of return value of find_funz()
#'
#' @examples
#' if(interactive()){
#' #load("data/x_v.RData")
#' l<-consolidate(x_v)
#' }
#'
#' @return returns a list whose components are character vector for permuting order of m
#'
#' @export
consolidate<-
  function(v) {
    l<-list()
    tv<-table(v)
    stv<-sort(tv,decreasing=TRUE)
    nv<-names(stv)
    for(x in nv)
      l[[x]]<-which(v==x)

    return(l)
  }

#' rearrangeM
#'
#' @description rearrange the order of rows or columns of matrix based on entries in a vector
#'
#' @param m character matrix return value of concatPackFunc2()
#' @param v2 list whose components are package names for permuting order of m,
#'  return value of consolidate()
#'
#' @examples
#' if(interactive()){
#' #load("data/x_m.RData")
#' #load("data/x_v2.RData")
#' m2<-rearrangeM(x_m,x_v2)
#' }
#'
#' @return returns rearranged version of m
#'
#' @export
rearrangeM<-
  function(m,v2) {
    m3<-m[,c(-1,-2)]
    m2<-matrix(nrow=nrow(m),ncol=2)
    m2[,1:2]<-m[,1:2]

    for(i in 1:length(v2))
      m2<-cbind(m2,m3[,v2[[i]]])

    return(m2)
  }

#' removeZeroRowsCols
#'
#' @description delete rows and cols of matrix m that contain all "0"s
#'
#' @param m character matrix whose entries are either "0" or "1"
#'
#' @examples
#' if(interactive()){
#' #load("data/x_m2.RData")
#' m3<-removeZeroRowsCols(x_m2)
#' }
#'
#' @return returns an altered version of character matrix m with removed rows and columns
#'
#' @export
removeZeroRowsCols<-
  function(m) {
    m2<-m[c(-1,-2),c(-1,-2)]
    w<-which(m2=="1",arr.ind=TRUE)

    rows<-sort(unique(w[,"row"]))
    cols<-sort(unique(w[,"col"]))

    return(m[c(1,2,(rows+2)),c(1,2,(cols+2))])
  }

#' mapFunctionsColors
#'
#' @description map functions to color coding
#'
#' @param row1 character vector containing names of packages
#' @param col1 character vector containing names of packages
#' @param colors character vector containing names of colors
#'
#' @examples
#' if(interactive()){
#' colors<-c("darkmagenta","darkolivegreen","darkorange3","brown4","red","blue")
#' #load("data/x_m3.RData")
#' colorMap<-mapFunctionsColors(x_m3[1,c(-1,-2)],x_m3[c(-1,-2),1],colors)
#' }
#'
#' @return returns a character vector mapping colors to package names
#'
#' @export
mapFunctionsColors<-
  function(row1,col1,colors) {
    u<-unique(c(row1,col1))
    n<-length(u)
    if(n>length(colors))
      stop(sprintf("Not enough different colors %d %d. Try again with fewer packages",n,length(colors)))

    v<-colors[1:n]
    names(v)<-u

    return(v)
  }

#' addStyle
#'
#' @description insert tags into HTML code to implement rotating table text
#'
#' @param x character vector containing HTML code
#' @param m character matrix containing table that is represented in x
#' @param colorMap character array of colors
#' @param pawn Boolean if TRUE use chess symbols rather than rectangles in html table
#'
#' @examples
#' if(interactive()){
#' #load("data/x_x2.RData")
#' #load("data/x_m5.RData")
#' #load("data/x_colorMap.RData")
#' y<-addStyle(x_x2,x_m5,x_colorMap,pawn=TRUE)
#' }
#'
#' @return returns modified HTML code
#'
#' @export
addStyle<-
  function(x,m,colorMap,pawn) {
    nm<-ncol(m)

    rs<-rotStyle()
    l<-spanTag(m[1,c(-1,-2)],"COLSPAN",colorMap) # package names row 1
    st<-l$v3
    rt<-rotTag(m[1,c(-1,-2)],m[2,c(-1,-2)],colorMap) # rotate function names in row 2

    l<-spanTag(m[c(-1,-2),1],"ROWSPAN",colorMap) # package names col 1
    if(length(l$tab)>1)
      x<-replaceRotTag(x,l,dim(m)) # package names col 1
    x_x3<-x
    #save(x_x3,file="x_x3.RData")

    x<-colorTag(m[c(-1,-2),1],m[c(-1,-2),2],ncol(m),x,colorMap,pawn)

    nx<-length(x)

    x<-gsub("<tr","<tr ALIGN=CENTER",x)

    # st takes up from line 8 to line 8+n-1
    # rt takes up from line 8+nm-1 + 2 to line 8+nm-1 + 2 + nm -1
    # when n is 24, these are 8 to 31 and 33 to 56
    #return(c(rs,x[1:7],st,rt,x[57:nx]))
    return(c(rs,x[1:7],st,rt,x[(8+nm-1+2+nm):nx]))
  }

#' rotStyle
#'
#' @description add html style definition for rotation
#'
#' @examples
#' r<-rotStyle()
#'
#' @return returns character string containing html style definition for rotation
#'
#' @export
rotStyle<-
  function() {
    # add style definition for rotation
    # https://stackoverflow.com/questions/47261100/how-to-rotate-text-90-degrees-inline

    return(c("<style>","#rotate-text {","width: 25px;",
             "transform: rotate(-90deg);","}","</style>"))
  }

#' spanTag
#'
#' @description Add html tag for package name to span multiple columns.
#' Also insert hyperlink to CRAN package and function documentation.
#'
#' @param v character vector representing first row of m (excluding first 2 entries of m)
#' @param direction character string COLSPAN or ROWSPAN
#' @param colorMap character array of colors
#'
#' @examples
#' if(interactive()){
#' #load("data/x_m5.RData")
#' #load("data/x_colorMap.RData")
#' l<-spanTag(x_m5[1,c(-1,-2)],"COLSPAN",x_colorMap)
#' }
#'
#' @details see https://www.pierobon.org/html/span.htm#:~:text=Cells%20within%20HTML%20tables%20can,span%20more%20than%20one%20column.
#'
#' @return returns a list whose components are
#' \itemize{
#'    \item u return value of unique(v)
#'    \item tab return value of table(v)
#'    \item v2 character vector modified version of v containing html span tags
#'  }
#'
#' @export
spanTag<-
  function(v,direction="COLSPAN",colorMap) {
    # add tag for package name to span multiple columns
    # https://www.pierobon.org/html/span.htm#:~:text=Cells%20within%20HTML%20tables%20can,span%20more%20than%20one%20column.
    # v is first row of m (excluding first 2 entries of m)

    l<-list()

    u<-unique(v)
    n<-length(u)
    tab<-table(v)
    v2<-vector("character",n)
    for(i in 1:n) {
      url<-sprintf("https://cran.r-project.org/web/packages/%s/%s.pdf",u[i],u[i])
      if(direction=="ROWSPAN") { # rotate package names in column 1
        v2[i]<-sprintf("<TD HEIGHT=10 %s='%d'><a href='%s'><font color='%s'>%s</font></a></TD>",
            direction,tab[u[i]],url,colorMap[u[i]],u[i])
        #v2[i]<-sprintf("<TD HEIGHT=10 %s='%d'><font color='%s'>%s</font></TD>",
        #direction,tab[u[i]],colorMap[u[i]],u[i])
      }
      else { # process [no rotation] package names in row 1
        v2[i]<-sprintf("<TD %s='%d'><div><a href='%s'><center><font color='%s'>%s</font></center></a></div></TD>",
                       direction,tab[u[i]],url,colorMap[u[i]],u[i])
        #v2[i]<-sprintf("<TD %s='%d'><div><center><font color='%s'>%s</font></center></div></TD>",
        #direction,tab[u[i]],colorMap[u[i]],u[i])
      }
    }

    l$u<-u
    l$tab<-tab
    l$v2<-v2
    l$v3<-c("<tr><td></td><td></td>",v2,"</tr>")

    return(l)
  }

#' rotTag
#'
#' @description add html tag to rotate function name
#'
#' @param v1 character vector containing first row of matrix m (excluding first 2 entries of m)
#' @param v2 character vector containing second row of matrix m (excluding first 2 entries of m)
#' @param colorMap character array of colors
#'
#' @examples
#' if(interactive()){
#' #load("data/x_m5.RData")
#' #load("data/x_colorMap.RData")
#' rt<-rotTag(x_m5[1,c(-1,-2)],x_m5[2,c(-1,-2)],x_colorMap)
#' }
#'
#' @details see https://stackoverflow.com/questions/47261100/how-to-rotate-text-90-degrees-inline
#'  also need to increase height of row to accommodate rotated text
#'  see https://resultuniversity.com/html/html-table-width-height#:~:text=To%20set%20the%20height%20of%20a%20specific%20row%20in%20an,property%20in%20pixels%20or%20percentages.
#'
#' @return returns character vector containing inserted html tags
#'
#' @export
rotTag<-
  function(v1,v2,colorMap) {
    # add tag to rotate function name
    # https://stackoverflow.com/questions/47261100/how-to-rotate-text-90-degrees-inline
    # <td class=cellinside><div id='rotate-text'>concatPackFunc2                  </div></td>
    # also need to increase height of row to accommodate rotated text
    # https://resultuniversity.com/html/html-table-width-height#:~:text=To%20set%20the%20height%20of%20a%20specific%20row%20in%20an,property%20in%20pixels%20or%20percentages.
    # v is second row of m (excluding first 2 entries of m)

    tv<-table(v1)

    n<-length(v2)
    v3<-vector("character",n)
    for(i in 1:n) { # rotated function names in row 2
      color<-colorMap[v1[i]]
      # December 9 2023 try truncating long function names
      #v3[i]<-sprintf("<td class=cellinside><div id='rotate-text'><font color='%s'>%s</font></div></td>",color,v2[i])
      v3[i]<-sprintf("<td class=cellinside><div id='rotate-text'><font color='%s'>%s</font></div></td>",color,str_trunc(v2[i],30))
    }

    # alignment of rotated function names in row 2
    # https://www.uwec.edu/kb/article/html-tables-alignment-within-a-table/#:~:text=In%20order%20to%20change%20the,the%20code%20for%20that%20row.

    n<-nchar(v2)
    m<-max(n)

    if(m<=15)
      return(c("<tr ALIGN=CENTER VALIGN=BOTTOM style='height:120px;'><td></td><td></td>",v3,"</tr>"))
    if(m<=20)
      return(c("<tr ALIGN=CENTER VALIGN=BOTTOM style='height:140px;'><td></td><td></td>",v3,"</tr>"))
    if(m<=30)
      return(c("<tr ALIGN=CENTER VALIGN=BOTTOM style='height:200px;'><td></td><td></td>",v3,"</tr>"))

      return(c("<tr ALIGN=CENTER VALIGN=BOTTOM style='height:250px;'><td></td><td></td>",v3,"</tr>"))
  }

#' replaceRotTag
#'
#' @description insert html tags for rotating text
#'
#' @param x return value of readLines(), HTML code containing data table
#' @param l return values of spanTag()
#' @param dims return value of dim()
#'
#' @examples
#' if(interactive()){
#' #load("data/x_x.RData")
#' #load("data/x_l.RData")
#' #load("data/x_m3.RData")
#' x<-replaceRotTag(x_x,x_l,dim(x_m3))
#' }
#'
#' @return returns modified version of HTML code containing data table
#'
#' @export
replaceRotTag<-
  function(x,l,dims) {
    u<-l$u
    tab<-l$tab
    v2<-l$v2 # v2[i]<-sprintf("<TD %s='%d'><center>%s</center></TD>",direction,tab[u[i]],u[i])

    nr<-dims[1]
    nc<-dims[2]

    start<-10 +2*nc # package name in col 1
    allCol1<-vector("integer",nr) # every row in x referencing col 1 in m
    for(i in 1:nr)
      allCol1[i]<-start+(i-1)*(nc+1)

    lt<-length(tab)

    inserts<-vector("integer",lt) # rows in x for inserting span tags
    inserts[1]<-start
    x[inserts[1]]<-l$v2[1]

    for(i in 2:lt) {
      inserts[i]<-inserts[i-1]+tab[u[i-1]]*(nc+1)
      x[inserts[i]]<-l$v2[i]
    }

    allCol1<-setdiff(allCol1,inserts)

    xx<-x[-allCol1]

    return(xx)
  }

#' colorTag
#'
#' @description add tag to color function name in column 2, based on package in column 1
#'
#' @param v1 character vector first column of m (excluding first 2 entries of m)
#' @param v2 character vector second column of m (excluding first 2 entries of m)
#' @param nc integer number of columns of m
#' @param x return value of replaceRotTag()
#' @param colorMap character array of colors
#' @param pawn Boolean if TRUE use chess symbols rather than rectangles in html table
#'
#' @examples
#' #load("data/x_m5.RData")
#' #load("data/x_colorMap.RData")
#' #load("data/x_x3.RData")
#' x<-colorTag(x_m5[c(-1,-2),1],x_m5[c(-1,-2),2],ncol(x_m5),x_x3,x_colorMap,pawn=TRUE)
#'
#' @details
#' v1 is first column of m (excluding first 2 entries of m)
#' v2 is second column of m (excluding first 2 entries of m)
#'
#' @return returns
#'
#' @export
colorTag<-
  function(v1,v2,nc,x,colorMap,pawn) {
    # add tag to color function name in column 2, based on package in column 1
    # v1 is first column of m (excluding first 2 entries of m)
    # v2 is second column of m (excluding first 2 entries of m)

    # https://www.toptal.com/designers/htmlarrows/symbols/
    chess<-c("&#9818;","&#9819;","&#9820;","&#9821;","&#9822;","&#9823;")

    n<-length(v2)
    nx<-length(x)
    u<-unique(v1)

    g<-grep("^<TD HEIGHT=",x)

    if(length(g)==0)
      return(x)

    g<-c(g,nx-4)
    # g is vector of line numbers in x such that x[g[i]] is
    # <TD HEIGHT=128 ROWSPAN='4'><div id='rotate-text'><center><font color='darkmagenta'>No_results_found</font></center></div></TD>

    v2Index<-0
    for(i in 1:(length(g)-1)) {
      ss<-strsplit(x[g[i]],"color='")
      ss2<-strsplit(ss[[1]][2],"'>")
      color<-ss2[[1]][1]

      nfuncs<-round((g[i+1]-g[i]-1)/nc,0)

      for(k in 1:nfuncs) {
        # annotate the function line in x
        v2Index<-v2Index+1
        line<-g[i]+(k-1)*nc+1
        #x[line]<-sprintf("<td class=cellinside><font color='%s'>%s</font></td>",colorMap[u[i]],v2[v2Index])
        x[line]<-sprintf("<td class=cellinside><font color='%s'>%s</font></td>",colorMap[u[i]],  str_trunc(v2[v2Index],25))

        # annotate the data lines for that function
        for(l in (line+1):(line+1+nc-3)) {
          ss<-strsplit(x[l],"<td class=cellinside>")
          ss2<-strsplit(ss[[1]][2]," +</td>")
          if(!is.na(ss2[[1]][1]))
          if(ss2[[1]][1]=="1") {
            if(pawn) {
              symbol<-chess[sample(1:(length(chess)),1)]
              x[l]<-sprintf("<td class=cellinside><font size=5 color='%s'>%s</font></td>",colorMap[u[i]],symbol)
            }
            else {
              x[l]<-sprintf("<td class=cellinside><font size=5 color='%s'>&#10074;</font></td>",colorMap[u[i]])
            }
            #x[l]<-sprintf("<td class=cellinside><font color='%s'>&#9818;</font></td>",colorMap[u[i]])
            #x[l]<-sprintf("<td class=cellinside bgcolor='%s'>1</td>",colorMap[u[i]])
          }
          else {
            x[l]<-sprintf("<td class=cellinside><font color='%s'></font></td>",colorMap[u[i]])
          }
        }
      }
    }

    return(x)
  }

# all of the functions below here were copied and pasted from the CRAN package
# mvbutils. When necessary, I performed some manual tweaking, in particular
# (1) I shortened the original version of globalVariables() to include those
# small number of entries that are required here
# (2) I commented out several lines that referred to "as.env".
# A more up to date version of mvbutils is maintained at
# markbravington.r-universe.dev by Mark Bravington

"foodweb" <- function(funs, where = 1, charlim = 80, prune = character(0),
    rprune, ancestors = TRUE, descendents = TRUE, plotting = TRUE,
    plotmath = FALSE, generics = c("c", "print", "plot", "["),
    lwd = 0.5, xblank = 0.18, border = "transparent", boxcolor = "white",
    textcolor = "black", color.lines = TRUE, highlight = "red",
    ...) {

  # copied and pasted from the CRAN package mvbutils

    oldpar <- par(..., no.readonly = TRUE)
    #####on.exit(par(oldpar))
    charlim <- charlim/par("cex")
    par(lwd = lwd)
    skip.computations <- FALSE
    if (missing(funs)) {
        if (is.environment(where))
            where <- list(where)
        funs <- unique(unlist(lapply(where, find.funs)))
    }
    else if (funs %is.a% "foodweb") {
        skip.computations <- TRUE
        extract.named(funs)
        funs <- names(level)
        n <- length(level)
    }
    if (!skip.computations) {
        if (!missing(rprune))
            prune <- funs %matching% rprune
        funs <- unique(c(funs, prune))
        if (!length(funs))
            return(structure(list(funmat = matrix(0, 0, 0), x = numeric(0),
                level = numeric(0)), class = "foodweb"))
        find.web()
        organize.web.display(plotmath = plotmath)
    }
    answer <- list(funmat = funmat, x = x, level = level)
    class(answer) <- "foodweb"
    if (plotting) {
        opar <- par("ps")
        if (names(dev.cur()) == "windows") {
            on.exit(par(ps = opar + 1L))
        }
        plot(answer, border = border, boxcolor = boxcolor, xblank = xblank,
            textcolor = textcolor, color.lines = color.lines,
            plotmath = plotmath, ...)
    }
    invisible(answer)
}
"find.funs" <- function(pos = 1, ..., exclude.mcache = TRUE,
    mode = "function") {

  # copied and pasted from the CRAN package mvbutils

    findo <- function(pos2) {
        o <- named(lsall(pos = pos2, ...))
        if (exclude.mcache)
            o <- o %except% mcachees(pos2)
        if (!length(o))
            return(character(0))
        old.warn <- options(warn = -1)$warn
        on.exit(options(warn = old.warn))
        keep <- sapply(o, exists, where = pos2, mode = mode,
            inherits = FALSE)
        if (!any(keep))
            return(character(0))
        names(o) <- NULL
        o[keep]
    }
    if (is.environment(pos))
        pos <- list(pos)
    ###else pos <- lapply(pos, as.env)
    unlist(lapply(pos, findo), use.names = FALSE)
}
"%is.a%" <- function(x, what) inherits(x, what, FALSE)
"extract.named" <- function(l, to = parent.frame()) {

  # copied and pasted from the CRAN package mvbutils

    n <- names(l)
    for (i in n[nchar(n) > 0]) assign(i, l[[i]], envir = to)
}
"%matching%" <- function(x, patt) unique(unlist(lapply(patt,
    grep, x = as.character(x), value = TRUE)))
"find.web" <- function(nlocal = sys.parent()) mlocal({

  # copied and pasted from the CRAN package mvbutils

    funs <- unique(c(funs, generics))
    n <- length(funs)
    if (!n)
        stop("Nothing there!")
    funmat <- matrix(0, n, n, dimnames = list(MASTER = funs,
        SLAVE = funs))
    master.of <- lapply(funs, called.by, can.match = funs, where = where)
    n.master <- unlist(lapply(master.of, length))
    if (!sum(n.master))
        stop("Bo-RING! No food chain here!")
    setup <- c(rep(1:length(funs), n.master), unlist(master.of))
    dim(setup) <- c(sum(n.master), 2)
    funmat[setup] <- 1
    diag(funmat) <- 0
    funmat[, generics] <- 0
    drop.generics <- funmat[generics, ] %**% rep(1, n) == 0
    if (any(drop.generics)) {
        funs <- funs[-match(generics[drop.generics], funs)]
        funmat <- funmat[funs, funs]
        n <- n - sum(drop.generics)
    }
    color <- rep(textcolor, n)
    if (length(prune)) {
        prunio <- matrix(0, length(prune), n)
        prunio <- sapply(to.regexpr(prune), regexpr, text = funs)
        prunio <- as.logical((prunio != -1) %**% rep(1, length(prune)))
        color[prunio] <- highlight
        if (descendents) {
            old.descendents <- rep(FALSE, n)
            descendents <- prunio
            while (sum(descendents) != sum(old.descendents)) {
                old.descendents <- descendents
                descendents <- descendents | (descendents %**%
                  funmat > 0)
            }
        }
        else descendents <- prunio
        if (ancestors) {
            old.ancestors <- rep(FALSE, n)
            ancestors <- prunio
            while (sum(ancestors) != sum(old.ancestors)) {
                old.ancestors <- ancestors
                ancestors <- ancestors | (funmat %**% ancestors >
                  0)
            }
        }
        else ancestors <- prunio
        color <- color[ancestors | descendents]
        funs <- funs[ancestors | descendents]
        funmat <- funmat[funs, funs, drop = FALSE]
        n <- length(funs)
    }
    if (!n)
        stop("Nothing there!")
    level <- rep(0, n)
    names(level) <- funs
    current.level <- 1
    while (any(level == 0)) {
        tops <- rep(1, sum(level == 0)) %**% funmat[level ==
            0, level == 0] == 0
        if (!any(tops))
            tops <- least.mutual.dependency(funmat, funs, level)
        level[dimnames(funmat)[[1]][level == 0][tops]] <- current.level
        current.level <- current.level + 1
    }
})
"organize.web.display" <- function(resequence = TRUE, merge01 = FALSE,
    plotmath = FALSE, nlocal = sys.parent()) mlocal({

      # copied and pasted from the CRAN package mvbutils

    level <- rep(0, n)
    names(level) <- funs
    current.level <- 1
    if (n > 1)
        while (any(level == 0)) {
            tops <- rep(1, sum(level == 0)) %**% funmat[level ==
                0, level == 0] == 0
            if (!any(tops))
                tops <- least.mutual.dependency(funmat, funs,
                  level)
            level[(1:n)[level == 0][tops]] <- current.level
            current.level <- current.level + 1
        }
    else level[] <- 1
    x <- numeric(n)
    n.masters <- sum(level == 1)
    if (!merge01) {
        level[level == 1 & ((funmat %*% rep(1, n)) == 0)] <- 0
        if (!sum(level == 1))
            level[level == 0] <- 1
    }
    for (current.level in min(level):max(level)) {
        if (resequence) {
            if (current.level > 1) {
                slave.of <- funmat[funs[level < current.level],
                  funs[level == current.level], drop = FALSE]
                pos.order <- (x[level < current.level] %*% slave.of)/(rep(1,
                  sum(level < current.level)) %*% slave.of)
                pos.order <- jitter(c(0, 1, pos.order))[-(1:2)]
            }
            else if (current.level == 1) {
                pos.order <- rank(jitter(c(-2, -1, funmat[level ==
                  1, ] %*% rep(1, n)))[-(1:2)])
                pos.order[pos.order%%2 == 0] <- 2 * length(pos.order) -
                  pos.order[pos.order%%2 == 0]
            }
            else pos.order <- 1:sum(level == 0)
            pos.order <- order(pos.order)
        }
        else pos.order <- 1:sum(level == current.level)
        level.shift <- if (current.level %in% c(0, 1, max(level)))
            0
        else (current.level - 1)/(max(level) - 1) - 0.5
        if (plotmath) {
            fn <- lapply(funs[level == current.level], function(x) parse(text = x)[[1]])
            nch <- sapply(fn, strwidth)
            charlim <- strwidth(paste(rep("x", charlim), collapse = ""))
        }
        else nch <- nchar(funs[level == current.level])
        if (exists("minstrl", frame = sys.nframe()))
            nch <- pmax(nch, minstrl)
        nch <- cumsum(nch[pos.order])
        x[level == current.level][pos.order] <- (c(0, clip(nch)) +
            nch + level.shift)/(2 * nch[length(nch)])
        layers <- nch[length(nch)]%/%charlim
        if (layers)
            layers <- rep(0.1 * seq(from = -layers, to = layers,
                by = 2), sum(level == current.level)/(1 + layers) +
                1)[1:sum(level == current.level)]
        level[level == current.level][pos.order] <- level[level ==
            current.level][pos.order] + layers
    }
    level <- 1 + max(round(level)) - level
})
"named" <- function(x) {

  # copied and pasted from the CRAN package mvbutils

    if (!length(x))
        return(x)
    names(x) <- as.character(x)
    x
}
"lsall" <- function(...) {

  # copied and pasted from the CRAN package mvbutils

    mc <- match.call(expand.dots = TRUE)
    mc$all.names <- TRUE
    mc[[1]] <- as.name("ls")
    eval(mc, parent.frame())
}
"%except%" <- function(vector, condition) vector[match(vector,
    condition, 0) == 0]
"mcachees" <- function(envir = .GlobalEnv) if (is.null(mcache <- attr(as.environment(envir),
    "mcache"))) character(0) else names(mcache)
"pos" <- function(substrs, mainstrs, any.case = FALSE, names.for.output) {

  # copied and pasted from the CRAN package mvbutils

    ls <- length(substrs)
    lm <- length(mainstrs)
    .pos <- function(substr, mainstr) {
        ns <- nchar(substr)
        nm <- nchar(mainstr)
        if (ns > nm)
            return(0)
        mainstr <- substring(mainstr, 1:(nm - ns + 1), ns:nm)
        t <- (1:length(mainstr))[mainstr == substr]
        if (length(t) == 0)
            0
        else t
    }
    if (any.case) {
        substrs <- upper.case(substrs)
        mainstrs <- upper.case(mainstrs)
    }
    if ((ls == 1) && (lm == 1))
        return(matrix(.pos(substrs, mainstrs), 1))
    if ((ls%%lm) * (lm%%ls))
        warning("Length of longer not a multiple of length of shorter")
    if (ls < lm) {
        if (missing(names.for.output))
            names.for.output <- names(mainstrs)
        substrs <- rep(substrs, (lm%/%ls) + 1)
    }
    else if (ls > lm) {
        if (missing(names.for.output))
            names.for.output <- names(substrs)
        mainstrs <- rep(mainstrs, (ls%/%lm) + 1)
    }
    else if (missing(names.for.output))
        names.for.output <- names(mainstrs)
    ls <- max(ls, lm)
    j <- vector("list", ls)
    for (i in (1:ls)) j[[i]] <- .pos(substrs[i], mainstrs[i])
    max.n.pos <- max(sapply(j, length))
    if (max.n.pos == 1)
        jj <- matrix(unlist(j), 1)
    else {
        jj <- sapply(j, function(x, w) c(x, rep(0, w - length(x))),
            w = max.n.pos)
    }
    dimnames(jj) <- list(character(0), names.for.output)
    t(jj)
}

"upper.case" <- function(s) {

  # copied and pasted from the CRAN package mvbutils

    a <- attributes(s)
    if (exists("casefold", mode = "function"))
        s <- casefold(s, upper = TRUE)
    else {
        s <- strsplit(s, "")
        lets <- LETTERS
        names(lets) <- letters
        transfer <- function(x) {
            change <- x %in% letters
            x[change] <- lets[x[change]]
            paste(x, collapse = "")
        }
        s <- sapply(s, transfer)
    }
    do.call("structure", c(list(.Data = s), a))
}
"mlocal" <- function(expr) {
    sp <- sys.parent()
    sp.env <- sys.frame(sp)
    nlocal <- get("nlocal", envir = sp.env)
    nlocal.env <- if (is.numeric(nlocal))
        sys.frame(nlocal)
    else as.environment(nlocal)
    on.exit({
        remove(list = names(params) %that.are.in% (lsall(env = nlocal.env) %except%
            names(savers)), envir = nlocal.env)
        for (i in names(savers)) assign(i, savers[[i]], envir = nlocal.env)
    })
    eval(expression(on.exit())[[1]], envir = nlocal.env)
    params <- formals(sys.function(sp))
    params <- params[names(params) != "nlocal"]
    savers <- names(params)
    if (length(params)) {
        names(savers) <- savers
        savers <- sapply(savers, exists, envir = nlocal.env,
            inherits = FALSE)
        savers <- names(savers)[savers]
        if (length(savers)) {
            names(savers) <- savers
            savers <- lapply(savers, function(x) mget(x, envir = nlocal.env)[[1]])
        }
        for (i in names(params)) {
            if (eval(call("missing", i), envir = sp.env)) {
                if (is.symbol(params[[i]]) && !nzchar(as.character(params[[i]])) &&
                  exists(i, envir = nlocal.env, inherits = FALSE))
                  remove(list = i, envir = nlocal.env)
                else assign(i, params[[i]], envir = nlocal.env)
            }
            else assign(i, sp.env[[i]], envir = nlocal.env)
        }
    }
    expr <- substitute(repeat {
        assign("answer", expr, envir = env)
        break
    }, list(expr = substitute(expr), env = sys.frame(sys.nframe())))
    on.exit.code <- quote(NULL)
    eval(expr, envir = nlocal.env, enclos = sys.frame(sys.nframe()))
    eval(on.exit.code, envir = nlocal.env, enclos = sys.frame(sys.nframe()))
    if (exists("override.answer", envir = sys.frame(sys.nframe()),
        inherits = FALSE))
        answer <- override.answer
    if (exists("answer", envir = sys.frame(sys.nframe()), inherits = FALSE))
        answer
}
"called.by" <- function(fname, can.match, where) {

  # copied and pasted from the CRAN package mvbutils

    where <- if (is.environment(where))
        list(where)
    else as.list(where)
    which <- unlist(lapply(where, exists, x = fname), use.names = FALSE)
    if (!any(which)) {
        f <- if (exists(fname))
            get(fname)
        else list()
    }
    else f <- get(fname, pos = where[[index(which)[1]]])
    flist <- char.unlist(f)
    if (!length(flist))
        return(numeric(0))
    everything <- flist
    everything <- match(everything, can.match, nomatch = 0)
    everything <- everything[everything > 0]
    everything
}
"%**%" <- function(x, y) {

  # copied and pasted from the CRAN package mvbutils

    dimnames(x) <- NULL
    dimnames(y) <- NULL
    if (length(dim(x)) == 2 && length(dim(y)) == 2 && dim(x)[2] ==
        1 && dim(y)[1] == 1)
        return(c(x) %o% c(y))
    if ((!is.null(dim(x)) && any(dim(x) == 1)))
        dim(x) <- NULL
    if ((!is.null(dim(y)) && any(dim(y) == 1)))
        dim(y) <- NULL
    if (is.null(dim(x)) && is.null(dim(y))) {
        if (length(x) == length(y))
            x <- x %*% y
        else {
            if ((length(x) != 1) && (length(y) != 1))
                stop("lengths of x (" %&% length(x) %&% ") and y (" %&%
                  length(y) %&% ") are incompatible")
            else x <- x * y
        }
    }
    else x <- x %*% y
    if ((!is.null(dim(x)) && any(dim(x) == 1)))
        dim(x) <- NULL
    x
}
"to.regexpr" <- function(x) {

  # copied and pasted from the CRAN package mvbutils

    x <- strsplit(x, "")
    repfun <- function(xx) {
        m <- match(xx, c("&", ".", "%", "\\", "[", "]", "(",
            ")", "^", "{", "}", "+", "|", "$", "?", "*"), 0)
        xx[m > 0] <- "\\" %&% xx[m > 0]
        paste(xx, collapse = "")
    }
    sapply(x, repfun)
}
"least.mutual.dependency" <- function(funmat, funs, level) {

  # copied and pasted from the CRAN package mvbutils

    group <- funmat[level == 0, level == 0, drop = FALSE]
    mode(group) <- "logical"
    old.group <- group & FALSE
    while (any(group != old.group)) {
        old.group <- group
        for (i in funs[level == 0]) {
            newbies <- group[, group[, i], drop = FALSE] %*%
                rep(1, sum(group[, i]))
            group[, i] <- group[, i] | (newbies > 0)
        }
    }
    nn <- sum(level == 0)
    keep <- c(TRUE, rep(FALSE, nn - 1))
    for (i in 2:nn) {
        old.group <- matrix(as.vector(group[, i]) == as.vector(group[,
            keep]), nrow = nn)
        keep[i] <- !any(rep(1, nn) %*% old.group == nn)
    }
    group <- group[, keep, drop = FALSE]
    if (ncol(group) > 1) {
        nn <- ncol(group)
        old.group <- matrix(0, nn, nn)
        for (i in 1:nn) for (j in (1:nn)[1:nn != i]) {
            old.group[i, j] <- set.test(group[, i], group[, j])
            old.group[j, i] <- -old.group[i, j]
        }
        old.group[old.group < 0] <- 0
        not.keep <- old.group %*% rep(1, nn) > 0
        group <- group[, !not.keep, drop = FALSE]
    }
    group <- dimnames(group)[[1]][apply(group, 1, any)]
    match(group, funs[level == 0])
}
"%that.are.in%" <- function(a, b) a[a %in% b]
"index" <- function(lvector) seq_along(lvector)[lvector]
"char.unlist" <- function(x) {

  # copied and pasted from the CRAN package mvbutils

    if (!(listable <- is.list(x))) {
        if (isS4(x) && (".Data" %in% names(getSlots(class(x)))))
            x <- x@.Data
        if (listable <- (!is.atomic(x) && !is.symbol(x))) {
            xx <- try(as.list(x), silent = TRUE)
            if (x %is.a% "try-error") {
                listable <- FALSE
            }
            else {
                x <- xx
            }
        }
    }
    if (listable)
        unlist(lapply(x, char.unlist), use.names = FALSE)
    else paste(deparse(x), collapse = "\n")
}
"%&%" <- function(a, b) paste(a, b, sep = "")
"group" <- function(m, ...) {

  # copied and pasted from the CRAN package mvbutils

    l <- list(...)
    if (length(l) == 1 && is.list(l))
        l <- l[[1]]
    rep(names(l), sapply(l, length))[match(m, unlist(l), NA)]
}
"set.test" <- function(a, b) {

  # copied and pasted from the CRAN package mvbutils

    r <- range(a - b)
    if (all(r == c(-1, 0)))
        -1
    else if (all(r == c(0, 1)))
        1
    else 0
}
"clip" <- function(x, n = 1) x[1 %upto% (length(x) - n)]
"%upto%" <- function(from, to) if (from <= to) from:to else numeric(0)

globalVariables(package = "foodwebWrapper", names =
c("funmat","generics","getSlots","highlight","level","minstrl","override.answer","prune","textcolor","x"))


