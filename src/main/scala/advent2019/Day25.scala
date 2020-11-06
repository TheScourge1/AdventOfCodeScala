package advent2019

object Day25 extends Day(25){
  override def testSetA = List()
  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {
    val prog = input.head.split(",").map(s => s.toLong)
    val startLocation = OpcodeProcessor.processDay5OppCode(Program(prog.clone(),0),List())
    val ship = visitShip(startLocation,new Ship(),List())
    printShip(ship)

    var program = OpcodeProcessor.processDay5OppCode(Program(prog.clone(),0),List())
    for(location  <- ship.getLocations){
      if(ship.getItems(location).nonEmpty && ship.getItems(location).intersect(Item.unpickables).isEmpty)
        program = pickItemsAt(program,location,ship)
    }

    val inventoryList = queryInventoryItems(program).toSet
    program = gotoLocation(program,Location.SecurityCheckpoint,ship)
    program = testCombo(inventoryList,program)._1

   "TODO"
 }

 override def solutionB(input: List[String], params: List[String]) = {

   "TODO"
 }

 def visitShip(initProg: Program,ship:Ship,path: List[Command]): Ship = {
   val output = printOutput(initProg.output)
   if(!output.contains("Command?")) throw new Exception("Error found:"+output)

   var resultShip = ship
   var program = initProg.clearOutput()

   val itemList = getItemList(output)
   val doorList = getDoorList(output)
   val location = getLocation(output)

   if(!resultShip.visited(location)){
     resultShip = resultShip.addLocation(location)
     resultShip = resultShip.setDoors(location,doorList)
     resultShip = resultShip.setItems(location,itemList)
     resultShip = resultShip.setPath(location,path)

     if(location != Location.SecurityCheckpoint)
       for(door <- doorList) {
         program = OpcodeProcessor.processDay5OppCode(program,door.getIntCode)
         if(printOutput(program.output).contains("You can't go that way"))
           throw new Exception(s"Invalid move found: ${door.name}"+printOutput(program.output))

         resultShip = visitShip(program,resultShip,path :+ door)
         program = OpcodeProcessor.processDay5OppCode(program, Door.invert(door).getIntCode)
         program = program.clearOutput()
       }
   }
   resultShip
 }

 def printShip(ship:Ship): Unit =
   for(loc <- ship.getLocations)
     println(s"$loc: ${ship.getItems(loc)}  => "+ship.getPathToLocation(loc).map(c => c.name))



 def printOutput(text: List[String]): String = text.map(i => i.toInt.toChar).fold("")(_.toString + _.toString).toString

 def getDoorList(str: String): List[Command] = getListOptions("Doors here lead:\\n",str).map(s => Command(s,"MOVE"))
 def getItemList(str: String): List[Item] = getListOptions("Items here:\\n",str).map(s => Item(s))
 def getInventoryList(str: String): List[Item] = getListOptions("Items in your inventory:\\n",str).map(s => Item(s))
 def getMessage(str: String): String = {
   val parse = "== [a-zA-Z ]+ ==\n[a-zA-Z ,.:;\\-']+".r
   parse.findFirstIn(str).getOrElse("Message Missing!!")
   str
 }

 def getLocation(str: String): Location = {
   val parse = "== ([a-zA-Z ]+) ==".r
   Location(parse.findFirstIn(str).getOrElse("Message Missing!!"))
 }

 def getListOptions(prefix:String, str: String): List[String] = {
   val listParse = (prefix+"((- [a-z0-9 ]+\\n)+)").r
   val cmd = "([a-z][ a-z0-9]*)".r
   val sequence = listParse.findAllMatchIn(str).map(r => r.group(1)).toList
   if(sequence.nonEmpty)
     cmd.findAllIn(sequence.head).toList
   else List()
 }

 def doAction(prog: Program,command:Command):Program = {
   var res= prog.clearOutput()
   res = OpcodeProcessor.processDay5OppCode(res, command.getIntCode)
   println(s"Action: ${command.name}"+printOutput(res.output))
   res
 }

  def gotoLocation(prog: Program, location: Location,ship:Ship): Program ={
    val path = ship.getPathToLocation(location)
    var resultProg = prog
    for(door <- path) resultProg = doAction(resultProg,door)
    resultProg
  }

  def getBackFromLocation(prog: Program, location: Location,ship:Ship): Program ={
    val path = ship.getPathToLocation(location)
    var resultProg = prog
    for(door <- path.reverse) resultProg = doAction(resultProg,Door.invert(door))
    resultProg
  }

  def pickItemsAt(prog: Program, location: Location,ship:Ship): Program ={
    var newProg = gotoLocation(prog,location,ship)
    for(item <- ship.getItems(location)) {
      newProg = doAction(newProg,item.take)
     // println(s"PickupResult ${item}: "+printOutput(newProg.output))
    }
    getBackFromLocation(newProg,location,ship)
  }

  def queryInventoryItems(prog:Program): List[Item] = {
    var program = prog.clearOutput()
    program = doAction(prog,Command("","INV"))
    getInventoryList(printOutput(program.output))
  }

  def testCombo(itemList: Set[Item],prog: Program): (Program,Boolean) ={
    var program = doAction(prog,Door.North)
    if(!printOutput(program.output).contains(Location.SecurityCheckpoint.name)) return (program,true)
    for(item <- itemList){
      program = doAction(program,item.drop)
      val testResult = testCombo(itemList - item,program)
      if(testResult._2) return testResult
      else program = testResult._1
      program = doAction(program,item.take)
    }

    (program,false)
  }

 case class Command(name: String,action: String){

   def getIntCode: List[Long] = {
     action match {
       case "MOVE" =>  moveAsLong
       case "TAKE" => takeAsLong
       case "DROP" => dropAsLong
       case "INV" => invAsLong
       case _ => throw new Exception("Unknown action: "+action)
     }
   }

   private def moveAsLong = name.toCharArray.map(c => c.toLong).toList :+10L
   private def takeAsLong = ("take "+name).toCharArray.map(c => c.toLong).toList :+10L
   private def dropAsLong = ("drop "+name).toCharArray.map(c => c.toLong).toList :+10L
   private def invAsLong = "inv".toCharArray.map(c => c.toLong).toList :+10L
 }

 object Door {
   val East:Command = Command("east","MOVE")
   val South: Command = Command("south","MOVE")
   val West:Command = Command("west","MOVE")
   val North:Command = Command("north","MOVE")

   def invert(command: Command): Command = command match {
       case Door.East => Door.West
       case Door.West => Door.East
       case Door.North => Door.South
       case Door.South => Door.North
       case _ => throw new Exception("Unknow door found: "+command)
     }
 }

 case class Item(name: String){
   def take:Command = Command(name,"TAKE")
   def drop:Command = Command(name,"DROP")
 }

  object Item {
    def unpickables = List(Item("giant electromagnet"),Item("molten lava"),Item("escape pod"),Item("photons"),Item("infinite loop"))
  }

  object Location{
    val SecurityCheckpoint:Location =  Location("== Security Checkpoint ==")
  }

 class Ship {
   private var locations: List[Location] = List()
   private var itemsAt: Map[Location,List[Item]] = Map()
   private var doorsAt: Map[Location,List[Command]] = Map()
   private var pathToLocation: Map[Location,List[Command]] = Map()

   def addLocation(loc: Location):Ship = {locations = locations :+ loc;this}
   def setItems(loc: Location,objects: List[Item]):Ship = { itemsAt = itemsAt + (loc -> objects);this}
   def setDoors(loc: Location,doors: List[Command]):Ship = { doorsAt = doorsAt + (loc -> doors);this}
   def setPath(loc: Location,path: List[Command]):Ship = { pathToLocation = pathToLocation + (loc -> path);this}
   def visited(loc: Location): Boolean = locations.contains(loc)

   def getLocations:List[Location]= locations
   def getDoors(loc: Location):List[Command] = doorsAt.getOrElse(loc,List())
   def getPathToLocation(loc: Location):List[Command] = pathToLocation.getOrElse(loc,List())
   def getItems(loc: Location):List[Item] = itemsAt.getOrElse(loc,List())
 }

 case class Location(name: String)
}
