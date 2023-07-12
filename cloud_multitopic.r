#this alters stm cloud to do wordcloud on merged multiple topics

cloud_multitopic<-function (stmobj, topic = NULL, type = c("model", "documents"), 
    documents, thresh = 0.9, max.words = 100, ...) 
{
    if (!requireNamespace("wordcloud", quietly = TRUE)) {
        stop("wordcloud package required to use this function.")
    }
	if (!requireNamespace("ggwordcloud", quietly = TRUE)) {
        stop("ggwordcloud package required to use this function.")
    }
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot package required to use this function.")
    }
    else {
        if (!inherits(stmobj, "STM")) 
            stop("cloud function only works for STM models.  See wordcloud package for general tools.")
        # if (length(topic) > 1) 
            # stop("Please only select 1 topic.")
        mod <- stmobj
        type <- match.arg(type)
        vocab <- mod$vocab
        if (is.null(topic)) 
            type <- "documents"
        if (type == "model") {
            if (length(mod$beta$logbeta) == 1) {
               # vec <- exp(mod$beta$logbeta[[1]])[topic, ] * 
				vec<- apply(exp(mod$beta$logbeta[[1]])[topic,],2,sum) *
				 sum(mod$settings$dim$wcounts$x)
            }
            else {
                levels <- table(mod$settings$covariates$betaindex)
                weights <- levels/sum(levels)
               # vec <- weights[1] * exp(mod$beta$logbeta[[1]])[topic, 
               #   ]
			   vec <- weights[1] * apply(exp(mod$beta$logbeta[[1]])[topic,],2,sum)
                for (i in 2:length(mod$beta$logbeta)) {
                  #vec <- vec + weights[i] * exp(mod$beta$logbeta[[i]])[topic, 
                  #  ]
				  vec <- vec + weights[i] * apply(exp(mod$beta$logbeta[[1]])[topic,],2,sum)
                }
                vec <- vec * sum(mod$settings$dim$wcounts$x)
            }
        }
        else {
            if (is.null(topic)) {
                vec <- mod$settings$dim$wcounts$x
            }
            else {
                if (is.null(documents)) 
                  stop("documents needed to give topic specific document values.")
                docnums <- which(mod$theta[, topic] > thresh)
                if (length(docnums) == 0) 
                  stop(sprintf("No documents have a topic loading higher than %s", 
                    thresh))
                subdoc <- documents[docnums]
                indices <- unlist(lapply(subdoc, "[", 1, 
                  ))
                counts <- unlist(lapply(subdoc, "[", 2, 
                  ))
                out <- aggregate(counts, by = list(indices), 
                  FUN = sum)
                vec <- rep(0, length(vocab))
                vec[out$Group.1] <- out$x
            }
        }
		df<-data.frame(words=vocab,freq=vec)
		df<-df[sort(df$freq,decreasing=TRUE,index.return=TRUE)$ix,]
		df<-df[1:max.words,]
        # wordcloud::wordcloud(words = vocab, freq = vec, max.words = max.words, 
            # ...)
		ggplot2::ggplot(df,aes(label=words,size=freq)) +
		ggwordcloud::geom_text_wordcloud() +
		#scale_size_area(max_size=20)+
		theme_minimal()
		
    }
}
