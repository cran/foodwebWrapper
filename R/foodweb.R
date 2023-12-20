#' foodwebWrapper
#'
#' @importFrom mvbutils foodweb
#' @import utils
#' @import tibble
#' @import dplyr
#' @import R2HTML
#' @import textshaping
#' @import magrittr
#' @import tidyverse
#' @import stringr
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
#' load("data/x_packages.RData")
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
    x<-mvbutils::foodweb(where=where,plotting=FALSE)
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
#' load("data/x_packages.RData")
#' load("data/x_funs.RData")
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
#' load("data/x_x.RData")
#' load("data/x_v.RData")
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
#' load("data/x_v.RData")
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
#' load("data/x_m.RData")
#' load("data/x_v2.RData")
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
#' load("data/x_m2.RData")
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
#' load("data/x_m3.RData")
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
#' load("data/x_x2.RData")
#' load("data/x_m5.RData")
#' load("data/x_colorMap.RData")
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
#' load("data/x_m5.RData")
#' load("data/x_colorMap.RData")
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
#' load("data/x_m5.RData")
#' load("data/x_colorMap.RData")
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
#' load("data/x_x.RData")
#' load("data/x_l.RData")
#' load("data/x_m3.RData")
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
#' load("data/x_m5.RData")
#' load("data/x_colorMap.RData")
#' load("data/x_x3.RData")
#' x<-colorTag(x_m5[c(-1,-2),1],x_m5[c(-1,-2),2],ncol(x_m5),x_x3,x_colorMap,pawn=TRUE)
#' }
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
