##pie_()
#' A more customizable pie chart function built upon the base graphics version
#'
#' @param  x a vector of non-negative numerical quantities.  The values in         ‘x’ are displayed as the areas of pie slices.
#' @param  labels one or more expressions or character strings giving names for          the slices.  Other objects are coerced by ‘as.graphicsAnnot’.For empty or ‘NA’ (after coercion to character) labels, no label nor pointing line is drawn.
#' @param edges The circular outline of the pie is approximated by a polygon with this many edges.
#' @param radius the pie is drawn centered in a square box whose sides range from -1 to 1.  If the character strings labeling the slices are long it may be necessary to use a smaller radius.
#' @param clockwise logical indicating if slices are drawn clockwise or counter clockwise (i.e., mathematically positive direction), the latter is default.
#' @param init.angle number specifying the _starting angle_ (in degrees) for the slices. Defaults to 0 (i.e., ‘3 o'clock’) unless ‘clockwise’ is true where ‘init.angle’ defaults to 90 (degrees), (i.e., ‘12 o'clock’).
#' @param density the density of shading lines, in lines per inch.  The default value of ‘NULL’ means that no shading lines are drawn. Non-positive values of ‘density’ also inhibit the drawing of shading lines.
#' @param angle the slope of shading lines, given as an angle in degrees (counter-clockwise).
#' @param col a vector of colors to be used in filling or shading the slices. If missing a set of 6 pastel colours is used, unless ‘density’ is specified when ‘par("fg")’ is used.
#' @param border (possibly vectors) arguments passed to ‘polygon’ which draws each slice.
#' @param lty (possibly vectors) arguments passed to ‘polygon’ which draws each slice.
#' @param main an overall title for the plot.
#' @param ... graphical parameters can be passed on as arguments to text() (for labels), title() (for titles) or polygon (for drawing slices).
#'
#' @param total (optional) total, which, if different from the sum of all values in x, used to calculate and draw the difference as an additional segment. An additional label "other" is appended to labels if length(labels)==length(x) and not length(x)+1
#' @param othercol (optional) color for difference between value for total and sum(x), defaults to "grey"
#' @param col.lab color for labels and pointers, defaults to col
#' @param italicize labels or indices of labels to italicize
#' @param cex label size
#' @param inner_radius radius of inner circle (if donut plot is desired, defaults to 0, for pie chart)
#' @param width width of outer circle (if donut plot is desired). Defaults to radius, for pie chart.
#' @param cross draw cross? if FALSE (default) no centered cross is drawn, otherwise the parameter can be a numeric vector with up to 3 elements which specify (in this order) the linetype, line width and color for the cross
#' @return plots pie chart
#' @seealso [graphics::pie()]
#' @references
#' R Core Team (2024). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
#' @details This function is a customized version of graphics::pie(), whose general usage and parameters and code are mostly a direct copy of that function. The present fork adds a number of parameters that enable control of label color (col.lab), italicization (italicize), plotting of an additional segment based on the difference between sum(x) and a specified total (total, othercol), drawing of a centered cross over the plot (cross) and turning the pie chart into a doughnut plot (width, inner_radius).
#' @export pie_
#' @importFrom grDevices as.graphicsAnnot dev.flush dev.hold
#' @importFrom graphics abline plot.new plot.window title
#' @importFrom paleoDiv add.alpha
#' @examples
#' pie_(c(1,3,5), total=10, col=c("red","blue","green"), labels=c("A","B","C"))
#' pie_(c(1,3,5), total=10, col=c("red","blue","green"), labels=c("A","B","C"), width=0.3)

pie_<-function (x, total=NULL, othercol="grey", labels = names(x), edges = 200, radius = 1, inner_radius=NULL, clockwise = FALSE, init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, col = NULL, border = NULL, lty = NULL, main = NULL, col.lab=col, italicize=NULL, cex=1, width=radius, cross=FALSE,...){

if(inherits(col,"function")) col(length(x))->col
if(inherits(col.lab,"function")) col.lab(length(x))->col.lab

if(!is.null(total)) if(total>sum(x,na.rm=T)) x<-c(x,(total-sum(x,na.rm=T)))
if(length(labels)==(length(x)-1)) labels<-c(labels,"other")


if(is.null(col.lab)) col.lab<-par("fg")
if(length(col.lab)<(length(x)-1)) rep(col.lab,length(x))[1:length(x)]->col.lab

if(!is.null(total) & length(col.lab)==(length(x)-1)) col.lab<-c(col.lab,othercol)
if(!is.null(total) & length(col)==(length(x)-1)) col<-c(col,othercol)

if(is.null(inner_radius) & !is.null(width)){ 
inner_radius<-radius-width
}


    if (!is.numeric(x) || any(is.na(x) | x < 0)) 
        stop("'x' values must be positive.")
    if (is.null(labels)) 
        labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- radius*1.05*c(-1, 1)
#    if (pin[1L] > pin[2L]) 
#        xlim <- (pin[1L]/pin[2L]) * xlim
#    else ylim <- (pin[2L]/pin[1L]) * ylim
    dev.hold()
    on.exit(dev.flush())
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col)) 
        col <- if (is.null(density)) 
            c("white", "lightblue", "mistyrose", "lightcyan", 
                "lavender", "cornsilk")
        else par("fg")
    if (!is.null(col)) 
        col <- rep_len(col, nx)
    if (!is.null(border)) 
        border <- rep_len(border, nx)
    if (!is.null(lty)) 
        lty <- rep_len(lty, nx)
    angle <- rep(angle, nx)
    if (!is.null(density)) 
        density <- rep_len(density, nx)
    twopi <- if (clockwise) 
        -2 * pi
    else 2 * pi
    t2xy <- function(t) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radius * cos(t2p), y = radius * sin(t2p))
    }#this part defines the x and y values for plotting each segment
    
    t2xy_inner <- function(t) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = inner_radius * cos(t2p), y = inner_radius * sin(t2p))
    }
    
        if(cross[1]!=FALSE){#draw cross
    
    if(length(cross)==1){ if(is.numeric(cross)){cross->lt}else{2->lt}
    lw<-1
    cl<-paleoDiv::add.alpha("black")
    }
    if(length(cross)==2){ 
		if(is.numeric(as.numeric(cross[1]))){ as.numeric(cross[1])->lt
		}else{2->lt}
		
		if(is.numeric(as.numeric(cross[2]))){ as.numeric(cross[2])->lw
		}else{1->lw}
		cl<-paleoDiv::add.alpha("black")
		}
    if(length(cross)>2){ 
		if(is.numeric(as.numeric(cross[1]))){ as.numeric(cross[1])->lt
		}else{2->lt}
		
		if(is.numeric(as.numeric(cross[2]))){ as.numeric(cross[2])->lw
		}else{1->lw}
		
		if(!is.numeric(cross[3])){ cross[3]->col
		}else{cl<-paleoDiv::add.alpha("black")}
		
		}    #cross[1] is lty, cross[2] lwd, cross[3] is color
    
    abline(v=0,lty=lt,lwd=lw,col=cl)
	abline(h=0,lty=lt,lwd=lw,col=cl)
    }
    
    for (i in 1L:nx) { #for-loop to draw the pie chart
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
        
		P_inner <- t2xy_inner(seq.int(x[i], x[i + 1], length.out = n))

if(inner_radius==0) polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], border = border[i], col = col[i], lty = lty[i], ...) ##plot pie

if(inner_radius>0) polygon(c(P$x,rev(P_inner$x)), c(P$y, rev(P_inner$y)), density = density[i], angle = angle[i], border = border[i], col = col[i], lty = lty[i], ...) ##plot donut
           
        P <- t2xy(mean(x[i + 0:1]))
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) {
            lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y, col=col.lab[i])
            
            if (labels[i] %in% italicize | i %in% italicize) {
				text(1.1 * P$x, 1.1 * P$y, bquote(italic(.(labels[i]))), xpd = TRUE, 
                adj = ifelse(P$x < 0, 1, 0), col=col.lab[i], cex=cex,...)
                message("italicizing ",labels[i])
                  }else{

            text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE, 
                adj = ifelse(P$x < 0, 1, 0), col=col.lab[i], cex=cex,...)
                }
        }
    }
    title(main = main, ...)
#    invisible(P)
}



##tally_div()
#' Produce a summary plot of the diversity of different groups
#' 
#' @param taxsel phylogeny or character vector containing the taxon names to be included
#' @param occ list() object containing the taxon-range tables
#' @param interval numeric vector giving the time interval for which to produce the plot
#' @param prefix character string giving prefix for names of taxon-range tables
#' @param type type of summary plot to produce can be either "barplot", "piechart", or any unambiguous substring of them
#' @param labels labels or names.arg pass to the plotting function (defaults to taxsel)
#' @param ... additional parameters to pass on to pie_() or barplot()
#' @importFrom graphics barplot
#' @importFrom paleoDiv divdistr_int
#' @export tally_div

tally_div<-function(taxsel, occ, interval=NULL, prefix="sptab_", type="barplot", labels=taxsel,...){

if(inherits(taxsel, "phylo")) taxsel<-taxsel$tip.label
if(!inherits(taxsel, "character")) stop("taxsel must be a character vector or object of class phylo")

names<-paste0(prefix,taxsel)#generate object names within occ


n<-numeric(length(names))
for(i in 1:length(names)){
occ[[names[i]]]->selection

#message(names[i])

nrow(selection)->n[i]
if(!is.null(interval)){

if(!is.numeric(interval) | length(interval)<2){ stop("interval must be a numeric vector of length 2!")}

paleoDiv::divdistr_int(x=interval,table=selection)->n[i]
}
#message(n[i])
}

names(n)<-taxsel

if(ua_substr(type, c("piechart","barplot"), match="piechart") | type=="p") pie_(n, labels=labels,...)
if(ua_substr(type, c("piechart","barplot"), match="barplot") | type=="b") barplot(n, names.arg=labels,...)->p

invisible(n)

}
