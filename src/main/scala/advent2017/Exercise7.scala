package advent2017

import scala.collection.mutable
import scala.collection.mutable.HashSet

object Exercise7 extends AdventBase{

  override def ex1(input: Seq[String]): String = {
    getDiskTree(input).name
  }

  override def ex2(input: Seq[String]): String = {

    var parentNode = getDiskTree(input)
    var weightMap = mutable.HashMap.empty[Int,HashSet[Disk]]
    do{
      weightMap = mutable.HashMap.empty[Int,HashSet[Disk]]
      for(subDisk <- parentNode.subDisks){
        val weight = getTotalWeight(subDisk)

        var tempDiskSet = weightMap.getOrElseUpdate(weight,HashSet.empty)
        tempDiskSet += subDisk
      }
      if(weightMap.size > 1) // looking for single node with wrong weight and set this as new parent
        parentNode = weightMap.values.find((dSet: HashSet[Disk]) => dSet.size == 1).get.head

    }while(weightMap.size > 1)

    var correctDisk = (parentNode.parent.subDisks.clone() - parentNode).head
    var diff = getTotalWeight(parentNode) - getTotalWeight(correctDisk)
    return (parentNode.weigth - diff).toString
  }


  class Disk(val name: String,var weigth: Int){
    var subDisks:HashSet[Disk] = mutable.HashSet.empty
    var parent: Disk = null

    override def toString: String = {return "" + name + " ("+ weigth.toString  +")"}
  }


  def getDiskTree(input: Seq[String]): Disk= {
    var diskMap = mutable.HashMap.empty[String,Disk]
    for(line <- input){
      var diskName = line.substring(0,line.indexOf(' '))
      var disk = diskMap.getOrElseUpdate(diskName, new Disk(diskName,-1))
      disk.weigth = line.substring(line.indexOf("(") + 1, line.indexOf(")")).toInt

      if(line.indexOf(">") > 0){
        val subDiskNames = line.substring(line.indexOf(">")+2).split(", ")
        for(name <- subDiskNames){
          val subDisk = diskMap.getOrElseUpdate(name,new Disk(name,-1))
          subDisk.parent = disk
          disk.subDisks+=subDisk
        }
      }
    }
    diskMap.values.find((d:Disk) => d.parent == null).get
  }

  def getTotalWeight(disk: Disk): Int = {
    if(disk.weigth <0)
      throw new Exception("Missing disk weight from: "+disk.name)
    var result = disk.weigth
    for(subDisk <- disk.subDisks) result+=getTotalWeight(subDisk)
    result
  }
}
