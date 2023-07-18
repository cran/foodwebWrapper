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
#'
#' @description wrapper for the function foodweb()
#' concatenate the R package name for each retrieved R function
#'
#' @param funs character vector OR (in foodweb only) the result of a previous foodweb call
#' @param where position(s) on search path, or an environment, or a list of environments
#' @param prune character vector. If omitted, all funs will be shown; otherwise, only ancestors and descendants of functions in prune will be shown. Augments funs if required.
#' @param rprune regexpr version of prune; prune <- funs %matching% rprune. Does NOT augment funs. Overrides prune if set.
#' @param ancestors show ancestors of prune functions?
#' @param descendents show descendents of prune functions?
#' @param ofile character string containing path name for output file
#' @param zeros Boolean if TRUE delete rows and cols that contain all 0's
#' @param pawn Boolean if TRUE use chess symbols rather than rectangles in html table
#' @param verbose Boolean if TRUE output several user messages
#'
#' @details If where=0, then the user is presented with the option
#'  of choosing from a list of attached packages
#'
#'  Many examples are given in the documentation for foodweb()
#'
#' @examples
#'
#' if(interactive()){
#'   foodwebWrapper(where=0,prune="cbind",
#'   ofile=sprintf("%s/foodwebWrapper.html",tempdir()))
#'   }
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
  function(funs,where=character(0),prune=character(0),rprune,ancestors=TRUE,descendents=TRUE,ofile="~/foodwebWrapper.html",zeros=TRUE,pawn=FALSE,verbose=TRUE) {

    if(verbose)
      message(sprintf("foodwebWrapper(): ofile is %s",ofile))

    l<-attachedFunctions(verbose=verbose)

    where<-unique(c(where,l$where))
    prune<-unique(c(prune,l$prune))

    writeLines(c(where,prune),sprintf("%s.txt",ofile))

    colors<-c("darkmagenta","darkolivegreen","darkorange3","brown4","red","blue","black","magenta","chocolate4")

    sink(sprintf("%s/fwjunk.txt",dirname(ofile)),type="output")
    #sink("~/fwjunk",type="output")
    on.exit(sink())
    x<-mvbutils::foodweb(funs=funs,where=where,prune=prune,rprune=rprune,ancestors=ancestors,descendents=descendents)
    #x_examples<-x
    #save(x_examples,file="~/x_examples.RData")
    funs<-colnames(x$funmat)
    #funs_examples<-funs
    #save(funs_examples,file="~/funs_examples.RData")

    if(length(funs)<2)
      stop("Not able to complete the wrapper portion:\nThe number of functions found by find_funs() was too low!")

    v<-vector("character")
    for(fun in funs)
      v[fun]<-find_funs(fun)$package_name[1]

    m<-concatPackFunc2(x$funmat,v)

    v2<-consolidate(v)

    m<-rearrangeM(m,v2)
    m<-t(rearrangeM(t(m),v2))

    f<-sprintf("%s/r2html",dirname(ofile))
    #f<-"~/r2html.txt"
    rownames(m)<-NULL
    colnames(m)<-NULL

    if(zeros)
      m<-removeZeroRowsCols(m)

    colorMap<-mapFunctionsColors(m[1,c(-1,-2)],m[c(-1,-2),1],colors)

    R2HTML::HTML(m,file=f,append=FALSE)
    x<-readLines(f)

    y<-addStyle(x,m,colorMap,pawn)

    writeLines(y,ofile)
  }

#' attachedPackages
#'
#' @description print a list of attached packages for the user to select from
#'
#' @examples if(interactive()){attachedPackages()}
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
#' @examples if(interactive()){attachedFunctions(verbose=TRUE)}
#'
#' @return returns a list whose components are
#' \itemize{
#'    \item l list of user-selected packages and corresponding functions
#'    \item where character vector of selected packages
#'    \item prune character vector of selected functions
#'  }
#' @export
attachedFunctions<-
  function(verbose) {
    l<-list()
    l2<-list()
    s<-search() # retrieve attached packages
    if(verbose) {
      message("\nPackages and environments to be included in the search\n")
      message("Only ancestors and descendants of functions in prune will be shown\n")
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
    l2$prune<-unlist(l)

    return(l2)
  }

#' find_funs
#'
#' @description determine in which R package a function ‘resides’
#' copied and pasted from https://sebastiansauer.github.io/finds_funs/
#'
#' @param f name of function for which the package(s) are to be identified.
#'
#' @examples find_funs("cbind")
#'
#' @details copied and pasted from https://sebastiansauer.github.io/finds_funs/
#'
#' @return returns dataframe with two columns:
#'  package_name packages(s) which the function is part of (chr)
#'  builtin_package  whether the package comes with standard R (a 'builtin'  package)
#'
#' @export
find_funs <- function(f) {
  if ("tidyverse" %in% rownames(installed.packages()) == FALSE) {
    stop("tidyverse is needed for this function. Please install. Stopping")
    }

  ###suppressMessages(library(tidyverse))


  # search for help in list of installed packages
  help_installed <- help.search(paste0("^",f,"$"), agrep = FALSE)

  # extract package name from help file
  pckg_hits <- help_installed$matches[,"Package"]

  if (length(pckg_hits) == 0) pckg_hits <- "No_results_found"

  # get list of built-in packages

  pckgs <- installed.packages()  %>% as_tibble
  pckgs %>%
    dplyr::filter("Priority" %in% c("base","recommended")) %>%
    dplyr::select("Package") %>%
    distinct -> builtin_pckgs_df

  # check for each element of 'pckg hit' whether its built-in and loaded (via match). Then print results.

  results <- data_frame(
    package_name = pckg_hits,
    builtin_pckage = match(pckg_hits, builtin_pckgs_df$Package, nomatch = 0) > 0,
    loaded = match(paste("package:",pckg_hits, sep = ""), search(), nomatch = 0) > 0
  )

  return(results)
}

#' concatPackFunc2
#'
#' @description match the package names with the function names
#'
#' @param m character matrix return value component $funmat of foodweb()
#' @param v character vector of package names returned by find_funs()
#'
#' @examples
#' v<-vector("character")
#' for(fun in funs_examples)
#'   v[fun]<-find_funs(fun)$package_name[1]
#'
#' m<-concatPackFunc2(x_examples$funmat,v)
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
#' @param v character vector of package names component of return value of find_funs()
#'
#' @examples
#' v<-vector("character")
#' for(fun in funs_examples)
#'   v[fun]<-find_funs(fun)$package_name[1]
#'
#' l<-consolidate(v)
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
#' v<-vector("character")
#' for(fun in funs_examples)
#'   v[fun]<-find_funs(fun)$package_name[1]
#'
#' m<-concatPackFunc2(x_examples$funmat,v)
#' v2<-consolidate(v)
#'
#' m2<-rearrangeM(m,v2)
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
#' v<-vector("character")
#' for(fun in funs_examples)
#'   v[fun]<-find_funs(fun)$package_name[1]
#'
#' m<-concatPackFunc2(x_examples$funmat,v)
#' v2<-consolidate(v)
#'
#' m2<-rearrangeM(m,v2)
#'
#' m3<-removeZeroRowsCols(m2)
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
#' v<-vector("character")
#' for(fun in funs_examples)
#'   v[fun]<-find_funs(fun)$package_name[1]
#'
#' m<-concatPackFunc2(x_examples$funmat,v)
#' v2<-consolidate(v)
#'
#' m2<-rearrangeM(m,v2)
#'
#' m3<-removeZeroRowsCols(m2)
#'
#' colors<-c("darkmagenta","darkolivegreen","darkorange3","brown4","red","blue")
#'
#' colorMap<-mapFunctionsColors(m3[1,c(-1,-2)],m3[c(-1,-2),1],colors)
#'
#' @return returns a character vector mapping colors to package names
#'
#' @export
mapFunctionsColors<-
  function(row1,col1,colors) {
    u<-unique(c(row1,col1))
    n<-length(u)
    if(n>length(colors))
      stop(sprintf("Not enough different colors %d %d. Try again with more stringent value for 'prune'",n,length(colors)))

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
#' v<-vector("character")
#' for(fun in funs_examples)
#'   v[fun]<-find_funs(fun)$package_name[1]
#'
#' m<-concatPackFunc2(x_examples$funmat,v)
#' v2<-consolidate(v)
#' m2<-rearrangeM(m,v2)
#'
#' m3<-removeZeroRowsCols(m2)
#'
#' colors<-c("darkmagenta","darkolivegreen","darkorange3","brown4","red","blue")
#' colorMap<-mapFunctionsColors(m3[1,c(-1,-2)],m3[c(-1,-2),1],colors)
#'
#' f<-sprintf("%s/r2html",tempdir())
#' R2HTML::HTML(m3,file=f,append=FALSE)
#' x<-readLines(f)

#' y<-addStyle(x,m3,colorMap,pawn=TRUE)
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
#'
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
#' v<-vector("character")
#' for(fun in funs_examples)
#'   v[fun]<-find_funs(fun)$package_name[1]
#'
#' m<-concatPackFunc2(x_examples$funmat,v)
#' v2<-consolidate(v)
#' m2<-rearrangeM(m,v2)
#'
#' m3<-removeZeroRowsCols(m2)
#'
#' colors<-c("darkmagenta","darkolivegreen","darkorange3","brown4","red","blue")
#' colorMap<-mapFunctionsColors(m3[1,c(-1,-2)],m3[c(-1,-2),1],colors)
#'
#' l<-spanTag(m3[1,c(-1,-2)],"COLSPAN",colorMap)
#'
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
#' v<-vector("character")
#' for(fun in funs_examples)
#'   v[fun]<-find_funs(fun)$package_name[1]
#'
#' m<-concatPackFunc2(x_examples$funmat,v)
#' v2<-consolidate(v)
#' m2<-rearrangeM(m,v2)

#' m3<-removeZeroRowsCols(m2)

#' colors<-c("darkmagenta","darkolivegreen","darkorange3","brown4","red","blue")
#' colorMap<-mapFunctionsColors(m3[1,c(-1,-2)],m3[c(-1,-2),1],colors)

#' rt<-rotTag(m[1,c(-1,-2)],m[2,c(-1,-2)],colorMap)
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
      v3[i]<-sprintf("<td class=cellinside><div id='rotate-text'><font color='%s'>%s</font></div></td>",color,v2[i])
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
#' if(interactive()) {
#' v<-vector("character")
#' for(fun in funs_examples)
#'   v[fun]<-find_funs(fun)$package_name[1]
#' v2<-consolidate(v)
#'
#' m<-concatPackFunc2(x_examples$funmat,v)
#' m2<-rearrangeM(m,v2)
#' m3<-removeZeroRowsCols(m2)
#'
#' f<-sprintf("%s/r2html",tempdir())
#' R2HTML::HTML(m3,file=f,append=FALSE)
#' x<-readLines(f)
#'
#' colors<-c("darkmagenta","darkolivegreen","darkorange3","brown4","red","blue")
#' colorMap<-mapFunctionsColors(m3[1,c(-1,-2)],m3[c(-1,-2),1],colors)
#'
#' l<-spanTag(m[1,c(-1,-2)],"COLSPAN",colorMap)
#'
#' x<-replaceRotTag(x,l,dim(m3))
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
#' if(interactive()){
#' v<-vector("character")
#' for(fun in funs_examples)
#'   v[fun]<-find_funs(fun)$package_name[1]
#'
#' m<-concatPackFunc2(x_examples$funmat,v)
#' v2<-consolidate(v)
#' m2<-rearrangeM(m,v2)
#'
#' m3<-removeZeroRowsCols(m2)
#' f<-sprintf("%s/r2html",tempdir())
#' R2HTML::HTML(m3,file=f,append=FALSE)
#' x<-readLines(f)
#'
#' colors<-c("darkmagenta","darkolivegreen","darkorange3","brown4","red","blue")
#' colorMap<-mapFunctionsColors(m3[1,c(-1,-2)],m3[c(-1,-2),1],colors)
#'
#' l<-spanTag(m[1,c(-1,-2)],"COLSPAN",colorMap)
#'
#' v<-vector("character")
#' for(fun in funs_examples)
#'   v[fun]<-find_funs(fun)$package_name[1]
#'
#' m<-concatPackFunc2(x_examples$funmat,v)
#' v2<-consolidate(v)
#' m2<-rearrangeM(m,v2)
#'
#' m3<-removeZeroRowsCols(m2)
#'
#' x<-replaceRotTag(x,l,dim(m3))
#'
#' colors<-c("darkmagenta","darkolivegreen","darkorange3","brown4","red","blue")
#' colorMap<-mapFunctionsColors(m3[1,c(-1,-2)],m3[c(-1,-2),1],colors)
#'
#' x<-colorTag(m3[c(-1,-2),1],m3[c(-1,-2),2],ncol(m3),x,colorMap,pawn=TRUE)
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
        x[line]<-sprintf("<td class=cellinside><font color='%s'>%s</font></td>",colorMap[u[i]],v2[v2Index])

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


#' leftJustifyHack
#'
#' @description insert &nbs escape sequences into text string to pad html table data cell
#'
#' @param v character vector
#'
#' @examples
#' v<-vector("character")
#' for(fun in funs_examples)
#'   v[fun]<-find_funs(fun)$package_name[1]
#'
#' v2<-leftJustifyHack(v)
#'
#' @details we need to generate a set of character strings to insert into html table data cells.
#'  we want the strings to be of equal length (matching the length of the longest string),
#'  by left justifying the strings and padding them on the right with spaces.
#'
#' @return returns a character vector whose components are padded with &nbs escape sequences
#'
#' @export
leftJustifyHack<-
  function(v) {
    n<-nchar(v)
    m<-max(n)
    del<-m-n

    tmp<-sprintf("%-*.30s",m,v)
    v<-gsub(" ","&nbs",tmp)

    return(v)
  }






