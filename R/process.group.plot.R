process.group.plot <- function(
  fx
  ,data = NULL
  ,FUN = hist.grouped
  ,interval.size = NA
  ,anchor.value  = NA
  ,width.consider = lolcat.default.width.consider 
  ,right = F
  ,hist.correct.label = isTRUE(all.equal(FUN,hist.grouped))
  #,call.dev.off = T
) {
  par.orig <- par(no.readonly = T)
  par(mar=c(2.5,2.5,1,1))
  
  fx.terms<-terms(fx)
  
  response<-all.vars(fx)[attributes(fx.terms)$response]
  iv.names<-attributes(terms(fx))$term.labels[which(attributes(fx.terms)$order == 1)]
  
  cell.codes <- compute.group.cell.codes(fx =fx, data = data)
  
  fd.overall <- frequency.dist.grouped(
    x = data[[response]]
    ,interval.size = interval.size
    ,anchor.value = anchor.value
    ,width.consider = width.consider
    ,right = right
  )
  
  anchor.value  <- fd.overall$midpoint[1]
  interval.size <- fd.overall$min[2] - fd.overall$min[1] 
  
  all.fd <- lapply(split(data[[response]], cell.codes), FUN = function(x) {
    frequency.dist.grouped(
      x = x
      ,interval.size = interval.size
      ,anchor.value = anchor.value
      ,right = right
    )
  } )
  
  xlim <- c(min(fd.overall$midpoint)-interval.size, max(fd.overall$midpoint) + interval.size)
  ylim <- c(0, max(unlist(lapply(all.fd, FUN = function(y) { max(y$freq)}))) + 1)

  if (length(iv.names) == 0) {
    
  } else if (length(iv.names) == 1) {
    layout_matrix <- matrix(1:length(all.fd), ncol=1)
    layout(layout_matrix)
    
    response.split <- split(data[[response]], cell.codes)
    iv.split <- split(data[[iv.names[1]]], cell.codes)
    
    # Plot rxc ...
    correction <- if (hist.correct.label) {
      .5*interval.size
    } else {
      0
    }
    at     <- fd.overall$midpoint-correction
    labels <- fd.overall$midpoint
    
    for ( i in 1:length(response.split)) {

      x <- response.split[[i]]
      
      if (length(x) > 0) {
        
        FUN(x
            ,xlim=xlim
            ,ylim=ylim
            ,main=paste(response, " (",iv.names[1]," = ",iv.split[[i]][1],")", sep="")
            ,xlab=NULL
            ,ylab=NULL
            ,anchor.value=anchor.value
            ,interval.size=interval.size
            ,xaxt="n"
            ,right = right)
        axis(1, at=at ,labels = labels )
        
      } else {
        plot.new()
        text(.5,.5, "N/A")
      }
    }
        
  } else if (length(iv.names) == 2) {
    row_var = iv.names[1]
    col_var = iv.names[2]
    
    unique_row = unique(as.character(data[[row_var]]))
    unique_col = unique(as.character(data[[col_var]]))
    
    #Compute layout matrix and widths/heights
    
    layout_mat <- matrix(1:(length(unique_row)*length(unique_col)) , nrow=length(unique_row), byrow=T) + 1 + 2 + length(unique_row) + length(unique_col)
    layout_mat <- cbind(1, layout_mat)
    layout_mat <- cbind(1, layout_mat)
    layout_mat <- rbind(1, layout_mat)
    layout_mat <- rbind(1, layout_mat)
    
    layout_mat[1,3:ncol(layout_mat)] <- 2 # Column Title Index
    layout_mat[3:nrow(layout_mat),1] <- 3 # Row Title Index
    
    layout_mat[2,3:ncol(layout_mat)] <- 3+1:(ncol(layout_mat)-2) # Column Titles Index
    layout_mat[3:nrow(layout_mat),2] <- layout_mat[2,ncol(layout_mat)]+1:(nrow(layout_mat)-2) # Column Titles Index
    
    widths  <- c(.1,.1,rep(.8/(ncol(layout_mat)-2),ncol(layout_mat)-2))
    heights <- c(.1,.1,rep(.8/(nrow(layout_mat)-2),nrow(layout_mat)-2))
    
    layout(layout_mat, widths = widths, heights = heights)
    
    #Start with labels
    plot.new() #Blank -
    
    plot.new() #Column variable
    text(0.5,0.5,col_var,cex=2,font=2)
    plot.new() #Row variable
    text(0.5,0.5,row_var,cex=2,font=2,srt=90)
    
    for (i in unique_col) {
      plot.new()
      text(0.5,0.5,i,cex=1.5,font=2)
    }

    for (i in unique_row) {
      plot.new()
      text(0.5,0.5,i,cex=1.5,font=2, srt=90)
    }

    # Plot rxc ...
    correction <- if (hist.correct.label) {
      .5*interval.size
    } else {
      0
    }
    at     <- fd.overall$midpoint-correction
    labels <- fd.overall$midpoint
    
    #print(correction)
    #print(at)
    #print(labels)
    
    for (i in unique_row) {
      for (j in unique_col) {
        x <- data[[response]][which(data[[row_var]] == i & data[[col_var]] == j)]
        
        if (length(x) > 0) {
        
        FUN(x
            ,xlim=xlim
            ,ylim=ylim
            ,main=NULL
            ,xlab=NULL
            ,ylab=NULL
            ,anchor.value=anchor.value
            ,interval.size=interval.size
            ,xaxt="n"
            ,right = right)
        axis(1, at=at ,labels = labels )
        
        } else {
          plot.new()
          text(.5,.5, "N/A")
        }
        
      }
    }
        
    
  } else {
    stop("Not supported yet...")
  }
  
    
  # x<-1:10
  # par(mar=c(2.5,2.5,1,1))
  # layout(matrix(c(6,6,6,1,2,3,1,4,5),ncol=3),heights=c(1,3,3), widths = c(1,3,3))
  # plot.new()
  # text(0.5,0.5,"First title",cex=2,font=2)
  # hist.grouped(rnorm(50), main = NULL)
  # hist.grouped(rnorm(50), main = NULL)
  # hist.grouped(rnorm(50), main = NULL)
  # hist.grouped(rnorm(50), main = NULL)
  # plot.new()
  # text(0.5,0.5,"Third title",cex=2,font=2,srt=90)
  
  
  #if (call.dev.off) {
  #  dev.off()
  #}
  
  par(par.orig)
}