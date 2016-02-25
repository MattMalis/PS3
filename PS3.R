getwd()

### Let's Make a Deal in S3!

## function randomDoor
## @param (no inputs)
## @return object of class "door", with randomly assigned value 1, 2, or 3
randomDoor<-function(){
  num<-sample(1:3, 1)
  class(num)<-"door"
  return(num)
}
## test: assign randomly chosen door to anyDoor, and verify that its class is "door"
anyDoor<-randomDoor()
anyDoor

## function chooseDoor
## @param x integer, door number that user chooses
## return object of class "door" with that integer value, if the value is 1, 2, or 3; otherwise, returns NULL 
chooseDoor<-function(x){
  if (x %in% 1:3){
    num<-structure(x, class="door")
    return(num)
  }
}
## test: assign desired door number to myDoor, verify that its class is "door"
myDoor<-chooseDoor(2.0)
class(myDoor)
## any value other than 1:3 assigns NULL to myDoor
myDoor<-chooseDoor(4)
myDoor<-chooseDoor("lamp")

## creating generic function, PlayGameS3
PlayGameS3<-function(x) UseMethod("PlayGameS3")

## default function for PlayGameS3, if PlayGameS3 is called on something besides a door
PlayGameS3.default<-function(x) "Can't play without a door!"
PlayGameS3(2)
PlayGameS3("Ham sandwich")


## adding a method for class "door" to the generic PlayGame function
PlayGameS3.door<-function(chosenDoor){
  carDoor<-sample(1:3,1) # randomly choose which door the car is behind
  if (carDoor==as.numeric(chosenDoor)){
    print("Congratulations! You won a car!")
  }
  else print("Sorry! You didn't win a car...")
}

## testing with randomly picked door
PlayGameS3(randomDoor())
## testing with user-assigned door
PlayGameS3(chooseDoor(3))
PlayGameS3("That door!")
PlayGameS3(chooseDoor("This door?"))


### Let's Make a Deal in S4!

## creating "Door" class; door number stored in "doorNumber" slot
setClass("Door", slots = list(doorNumber = "numeric"))

## creating generic function "pickDoor"
setGeneric("pickDoor", function(object){standardGeneric("pickDoor")})
## creating "pickDoor" method for objects of class "integer", that returns an object of class "Door
setMethod("pickDoor", signature(object = "numeric"), 
          function(object){return(new("Door", doorNumber = object)) })


## setting validity for class "Door"
setValidity("Door", function(object){
  # is there a door number?
  test1<-is.numeric(object@doorNumber)
  if(!test1){return ("Door doesn't have a number")}
  # is it an integer?
  test2<-(object@doorNumber%%1==0)
  if(!test2){return("Door number is not an integer")}
  # is it 1, 2, or 3?
  test3<-(object@doorNumber %in% 1:3)
  if(!test3){return("Door number is not in range")}
}
)

## test
thisDoor<-pickDoor(3)
thisDoor@doorNumber
thatDoor<-pickDoor("apple")
otherDoor<-pickDoor(4)
anotherDoor<-pickDoor(2.5)

## creating generic function "playGameS4"
setGeneric("playGameS4", function(object){standardGeneric("playGameS4")})
## creating "playGameS4" method for objects of class "Door"
setMethod("playGameS4", signature(object = "Door"), 
          function(object){
            winDoor<-sample(1:3, 1)
            ifelse(object@doorNumber==winDoor, 
                   "New car! Congrats!", 
                   "No car. Tough break, kid.")})
## test
playGameS4(pickDoor(1))
playGameS4(pickDoor(2.8))
playGameS4(pickDoor(11))
playGameS4(pickDoor("aardvark"))
