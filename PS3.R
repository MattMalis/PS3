getwd()

### Let's Make a Deal in S3!

## function randomDoor
## @param
## @return object with value 1, 2, or 3, of class "door"
randomDoor<-function(){
  num<-sample(1:3, 1)
  class(num)<-"door"
  return(num)
}
## test the function, assign randomly chosen door to anyDoor... its class is "door"
anyDoor<-randomDoor()
class(anyDoor)

## function chooseDoor
## @param integer 1, 2, or 3
## return object with that integer value, of class "door"
chooseDoor<-function(x){
  if (x %in% 1:3){
    num<-structure(x, class="door")
    return(num)
  }
}
## test the function, assign desired door to myDoor... its class is "door"
myDoor<-chooseDoor(3)
myDoor<-chooseDoor(2.0)
class(myDoor)
## any value besides 1:3 assigns NULL to myDoor
myDoor<-chooseDoor(4)
myDoor<-chooseDoor("lamp")

## creating generic function, PlayGame
PlayGame<-function(x) UseMethod("PlayGame")

## default funciton for PlayGame, if PlayGame is called on something besides a door
PlayGame.default<-function(x) "Can't play without a door!"
PlayGame(2)
PlayGame("Ham sandwich")


## adding a method for class "door" to the generic PlayGame function
PlayGame.door<-function(chosenDoor){
  carDoor<-sample(1:3,1) # randomly choose which door the car is behind
  if (carDoor==as.numeric(chosenDoor)){
    print("Congratulations! You won a car!")
  }
  else print("Sorry! You didn't win a car...")
}

## testing with randomly picked door
PlayGame(randomDoor())
## testing with user-assigned door
PlayGame(chooseDoor(3))



### Let's Make a Deal in S4!

## creating "Door" class
setClass("Door", slots = list(doorNumber = "numeric"))

## creating generic function "pickDoor"
setGeneric("pickDoor", function(object){standardGeneric("pickDoor")})
## creating "pickDoor" method for objects of class "integer", that returns an object of class "Door
setMethod("pickDoor", signature(object = "numeric"), 
          function(object){return(new("Door", doorNumber = object)) })
## test
myDoor<-pickDoor(3)
myDoor@doorNumber

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

## creating generic function "playGameS4"
setGeneric("playGameS4", function(object){standardGeneric("playGameS4")})
## creating "playGameS4" method for objects of class "Door"
setMethod("playGameS4", signature(object = "Door"), 
          function(object){
            winDoor<-sample(1:3, 1)
            ifelse(object@doorNumber==winDoor, 
                   print("New car! Congrats!"), 
                   print("No car. Tough break, kid."))})
playGameS4(pickDoor(1))

## why does it print the message twice.......???
