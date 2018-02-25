if(FALSE) {
startU = "https://www.amazon.com/gp/product/B00LZS5EEI/ref=s9u_simh_gw_i3?ie=UTF8&fpl=fresh&pd_rd_i=B00LZS5EEI&pd_rd_r=a36fbc1e-1a4e-11e8-ba50-2f42e5cb9555&pd_rd_w=z0Oy6&pd_rd_wg=rpkHs&pf_rd_m=ATVPDKIKX0DER&pf_rd_s=&pf_rd_r=26BM0R2CY75QR8D81WR1&pf_rd_t=36701&pf_rd_p=0411ffec-c026-40ae-aac5-2cd3d48aeeac&pf_rd_i=desktop"



startU = "https://www.amazon.com/Brother-HL-L2340DW-Monochrome-Connectivity-Replenishment/product-reviews/B00LZS5EEI/ref=cm_cr_dp_d_show_all_top?ie=UTF8&reviewerType=all_reviews"
z = reviewInfo(startU, max = 30)
#rvs = do.call(rbind, lapply(z, getReviewInfo))


startU = "https://www.amazon.com/Samsung-Printer-SL-M2825DW-Wireless-Monochrome/product-reviews/B00C1TF4FY/ref=cm_cr_dp_d_show_all_top?ie=UTF8&reviewerType=all_reviews"
samsung = reviewInfo(startU)
}

getReviews = reviewInfo =
    # https://www.amazon.com/Brother-HL-L2340DW-Monochrome-Connectivity-Replenishment/dp/B00LZS5EEI?psc=1&SubscriptionId=0K76CZ6RCX2Y05HSNPR2&tag=cs-blog-20&linkCode=xm2&camp=2025&creative=165953&creativeASIN=B00LZS5EEI&ascsubtag=laser-printers#customerReviews
function(u, nodeFun = getReviewInfo, max = NA, curl = getConnection(), verbose = TRUE)
{
    doc = getDoc(u, curl)
    reviews = getReviews(doc)
    while(is.na(max) || (length(reviews) < max)) {
        nx = getNextURL(doc, u)
        if(length(nx) == 0)
            break
        if(verbose)
            message("next page:", nx)
        doc = getDoc(nx, curl)
        reviews = c(reviews, getReviews(doc))
        u = nx
    }
    if(length(nodeFun)) {
        reviews = lapply(reviews, nodeFun)
        if(all(sapply(reviews, is.data.frame)) && length(unique(sapply(reviews, ncol))) == 1)
           reviews = do.call(rbind, reviews)
    } 

    reviews
}

getDoc =
function(u, curl)
{
    txt = getURLContent(u, curl = curl)
    doc = htmlParse(txt)
    docName(doc) = u
    doc
}

getConnection =
function(u = character(), ...)
{
    con = getCurlHandle(followlocation = TRUE,
                        useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.167 Safari/537.36",
                        cookiejar = "", ...)

    if(length(u)) # just visit this for the cookies.
        getURLContent(u, curl = con)
    
    con
}


getReviews =
function(doc)
{    
    rv = getNodeSet(doc, "//div[starts-with(@id, 'customer_review')]")
}    

getNextURL=
function(doc, baseURL = docName(url))
{
    u =  getNodeSet(doc, "//a[contains(., 'Next')]/@href")
    if(length(u))
        getRelativeURL(u[[1]], baseURL)
    else
        character()
}


getReviewInfo =
function(node)
{
    stars = xpathSApply(node, ".//i[@data-hook='review-star-rating']", xmlValue)
    stars = gsub(" out of 5 stars", "", stars)
    author = xpathSApply(node, ".//a[@data-hook='review-author']", xmlValue)
    date = xpathSApply(node, ".//span[@data-hook='review-date']", xmlValue)
    title = xpathSApply(node, ".//a[@data-hook='review-title']", xmlValue)
    body = xpathSApply(node, ".//span[@data-hook='review-body']", xmlValue)

    data.frame(stars = as.numeric(stars), author = author, date = date, title = title, body = body,
               stringsAsFactors = FALSE)
}


getWords =
function(body, stopWords = tm::stopwords())
{
    words = strsplit(body, "[[:space:][:punct:]]+")
    lapply(words, function(x) { x = tolower(x) ; x[ !(x %in% stopWords)]})
}
