library("neuralnet")

#Going to create a neural network to perform prediction
#Type ?neuralnet for more information on the neuralnet library

#Generate training data
#And store them as a dataframe
traininginput <- as.data.frame(matrix(c(24,350,1,
                                        24,250,1,
                                        23,250,1,
                                        27,250,4,
                                        24,250,1,
                                        21,200,5,
                                        27,250,4,
                                        21,250,5,
                                        32,250,5,
                                        23,250,5
                                        ), nrow=10, ncol=3))
trainingoutput <- c(1099, 689, 599, 849, 499, 339, 819, 389, 799, 599)

#Column bind the data into one variable
trainingdata <- cbind(traininginput, trainingoutput)

# Create Vector of Column Max and Min Values
maxs <- apply(trainingdata[,], 2, max)
mins <- apply(trainingdata[,], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.trainingdata <- as.data.frame(scale(trainingdata[,], center=mins, scale=maxs-mins))
trainingdata <- scaled.trainingdata

# Check out results
print(head(trainingdata, 10))

colnames(trainingdata) <- c("Matryca", "Iluminacja", "Reakcja", "Cena") 
print(trainingdata)

#Train the neural network
#Going to have C(5, 4, 3) hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.price <- neuralnet(Cena~Reakcja+Matryca+Iluminacja, trainingdata, hidden=c(3, 4, 3), threshold=0.001)
print(net.price)

#Plot the neural network
plot(net.price)

#Test the neural network on some training data
testdata <- as.data.frame(matrix(c(1583, 8135, 6421,
                                   2000, 8460, 1500,
                                   1712, 9445, 1819), nrow=3, ncol=3))
scaled.testdata <- as.data.frame(scale(testdata[,], center=mins[1:3], scale=maxs[1:3]-mins[1:3]))
net.results <- compute(net.price, scaled.testdata) #Run them through the neural network

#Lets see what properties net.price has
ls(net.results)

#Lets see the results
print(net.results$net.result)