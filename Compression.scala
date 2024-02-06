import scala.collection.mutable
import scala.io.Source
import java.io._

// 定义节点类
case class Node(char: Option[Char], freq: Int, left: Option[Node] = None, right: Option[Node] = None)

object HuffmanCoding {
  def main(args: Array[String]): Unit = {
    val inputFile = "Maupassant.txt"
    val outputFile = "MaupCprssnt.txt"
    val huffDictFile = "HuffDictionary.txt"

    // 1. 统计字符频率
    val frequency = countCharacterFrequency(inputFile)

    // 2. 构建哈夫曼树
    val root = buildHuffmanTree(frequency)

    // 3. 生成哈夫曼编码字典
    val huffmanCodeMap = generateHuffmanCodes(root)

    // 4. 打印字符频率和哈夫曼编码表
    printCharacterFrequencies(frequency)
    printHuffmanEncodingTable(huffmanCodeMap)

    // 5. 压缩文本并写入文件
    compressText(inputFile, outputFile, huffmanCodeMap)

    // 6. 输出哈夫曼树字典到文件（这里假设输出为txt格式）
    writeHuffmanDictionaryToFile(huffDictFile, huffmanCodeMap)
  }

  // 1. 统计字符频率
  def countCharacterFrequency(filePath: String): mutable.Map[Char, Int] = {
    var frequency = mutable.Map[Char, Int]()
    Source.fromFile(filePath, "UTF-8").getLines().foreach { line =>
      for(char <- line){
        if(char.isLetterOrDigit || char == ' ') { // 只统计字母、数字和空格
          frequency += (char -> (frequency.getOrElse(char, 0) + 1))
        }
      }
    }
    frequency
  }

  // 2. 构建哈夫曼树
  def buildHuffmanTree(frequency: mutable.Map[Char, Int]): Node = {
    import scala.collection.mutable.PriorityQueue

    val queue = PriorityQueue[(Int, Node)]()(Ordering.by(_._1))
    for ((char, freq) <- frequency) queue.enqueue((freq, Node(Some(char), freq)))

    while (queue.size > 1) {
      val (freq1, node1) = queue.dequeue()
      val (freq2, node2) = queue.dequeue()
      val newNode = Node(None, freq1 + freq2, Some(node1), Some(node2))
      queue.enqueue((newNode.freq, newNode))
    }

    queue.dequeue()._2
  }

  // 3. 生成哈夫曼编码字典
  def generateHuffmanCodes(root: Node, code: String = "", result: mutable.Map[Char, String] = mutable.Map()): mutable.Map[Char, String] = {
    if (root.char.isDefined) {
      result += (root.char.get -> code)
    } else {
      generateHuffmanCodes(root.left.get, code + "0", result)
      generateHuffmanCodes(root.right.get, code + "1", result)
    }
    result
  }

  // 4. 打印字符频率
  def printCharacterFrequencies(frequency: mutable.Map[Char, Int]): Unit = {
    val totalChars = frequency.values.sum.toDouble
    println("Character Frequencies:")
    for ((char, freq) <- frequency.toSeq.sortBy(-_._2)) {
      val percent = (freq / totalChars) * 100
      println(s"$char: ${"%.4f".format(percent)}%")
    }
  }

  // 5. 打印哈夫曼编码表
  def printHuffmanEncodingTable(codeMap: mutable.Map[Char, String]): Unit = {
    println("\nHuffman Encoding Table:")
    for ((char, code) <- codeMap.toSeq.sortBy(_._1)) {
      println(s"$char: $code")
    }
  }

  // 6. 压缩文本并写入文件
  def compressText(inputFile: String, outputFile: String, huffmanCodeMap: mutable.Map[Char, String]): Unit = {
    val outputStream = new FileOutputStream(outputFile)
    for (line <- Source.fromFile(inputFile, "UTF-8").getLines()) {
      val compressedLine = line.flatMap{charr => 
        if (charr.isLetterOrDigit || charr == ' ') {
           // 编码数字、字母和空格
           huffmanCodeMap.getOrElse(charr, "").toString
        } else {
          ""
       }
      }.mkString
      
      outputStream.write(compressedLine.getBytes("utf-8"))
      outputStream.write('\n')
    }
    outputStream.close
  }

  // 7. 输出哈夫曼树字典到文件
  def writeHuffmanDictionaryToFile(dictFile: String, huffmanCodeMap: mutable.Map[Char, String]): Unit = {
    val writer = new PrintWriter(new File(dictFile))
    for ((char, code) <- huffmanCodeMap) {
      writer.println(s"$char: $code")
    }
    writer.close()
  }
}