baseUrl <- "https://eastus2.api.cognitive.microsoft.com/face/v1.0/detect"
q <- "?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age,gender,smile,headPose,facialHair,glasses,emotion"
url1 <- paste(baseUrl, q, sep="")

img1Url <- "http://vis-www.cs.umass.edu/lfw/images/Jack_Nicholson/Jack_Nicholson_0002.jpg"
img2Url <- "http://vis-www.cs.umass.edu/lfw/images/Jack_Nicholson/Jack_Nicholson_0003.jpg"

f1 <- tempfile()
download.file(img1Url, f1, mode="wb")
pic1 <- upload_file(f1)

f2<- tempfile()
download.file(img2Url, f2, mode="wb")
pic2 <- upload_file(f2)

#send the request to Face API
response = POST(url=url1, body=pic1, add_headers(.headers = 
                c('Content-Type'='application/octet-stream', 'Ocp-Apim-Subscription-Key'='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')))

result <- content(response)
df <- as.data.frame(result)

# pivot the data frame...you need to add package reshape2 for this
df2 <- melt(df, id=c("faceId"))

# plot the image
img1 <- readJPEG(f1)
plot(0:1, 0:1, xlim=range(c(0,150)), ylim=range(c(0,150)),type='n', asp=1)
rasterImage(img1, 0, 0, 150, 150, interpolate=F)

# tip of the nose
tip_nose_x = df[ ,c("faceLandmarks.noseTip.x")]
tip_nose_y = df[ ,c("faceLandmarks.noseTip.y")]

IMAGE_HEIGHT <- 150

#now let's draw the dot at that location
points(tip_nose_x, IMAGE_HEIGHT - tip_nose_y, pch=19, col="green")

# left pupil location
left_pupil_x = df[ ,c("faceLandmarks.pupilLeft.x")]
left_pupil_y = df[ ,c("faceLandmarks.pupilLeft.y")]
points(left_pupil_x, IMAGE_HEIGHT - left_pupil_y, pch=19, col="green")

# right pupil location
right_pupil_x = df[ ,c("faceLandmarks.pupilRight.x")]
right_pupil_y = df[ ,c("faceLandmarks.pupilRight.y")]
points(right_pupil_x, IMAGE_HEIGHT - right_pupil_y, pch=19, col="green")

# face rectangle
xleft <- df[ ,c("faceRectangle.left")]
ytop <- IMAGE_HEIGHT - df[ ,c("faceRectangle.top")]
ybottom <- ytop - df[ ,c("faceRectangle.height")]
xright <- df[ ,c("faceRectangle.left")] + df[ ,c("faceRectangle.width")]
rect(xleft, ybottom, xright, ytop, col=NA, border="magenta", lwd=2)

#send the second picture to Face API
response = POST(url=url1, body=pic2, add_headers(.headers = 
  c('Content-Type'='application/octet-stream', 'Ocp-Apim-Subscription-Key'='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')))

result2 <- content(response)
dfSecondPicture <- as.data.frame(result2)

# pivot the data frame
dfSecondPicturePivoted <- melt(dfSecondPicture, id=c("faceId"))

# detect similarity
pic1FaceId <- df[,c("faceId")]
pic2FaceId <- dfSecondPicture[ ,c("faceId")]

baseUrlFindSimilar <- "https://eastus2.api.cognitive.microsoft.com/face/v1.0/findsimilars"
bodyFindSimilar <- sprintf('{"faceId": "%s",
                           "faceIds": ["%s", "%s"],
                           "mode": "%s"}',
                           pic1FaceId,
                           pic1FaceId,
                           pic2FaceId,
                           "matchPerson")

#send pictures to Face API - Find Similar
responseSimilar = POST(url=baseUrlFindSimilar, body=bodyFindSimilar, add_headers(.headers = 
  c('Content-Type'='application/json', 'Ocp-Apim-Subscription-Key'='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')))

resultSimilar <- content(responseSimilar)
dfSimilar <- as.data.frame(resultSimilar)

cat("\n", "Similarity between first picture and itself: ", dfSimilar[,c("confidence")])
cat("\n", "Similarity between first and second picture: ", dfSimilar[,c("confidence.1")])

# cleanup
file.remove(f1)
file.remove(f2)
