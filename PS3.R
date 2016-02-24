getwd()

### Let's Make a Deal in S3

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
  if ((x==1|x==2)|x==3){
    num<-structure(x, class="door")
    return(num)
  }
}
## test the function, assign desired door to myDoor... its class is "door"
myDoor<-chooseDoor(3)
class(myDoor)
## any value besides 1:3 assigns NULL to myDoor
myDoor<-chooseDoor(4)


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
PlayGame(chooseDoor(1))


### Let's Make a Deal in S4



