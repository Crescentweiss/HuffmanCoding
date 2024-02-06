import scala.io.Source

object Main extends App {
  // 文件路径，这里假设文件和程序在同一目录下
  val filePath = "Maupassant.txt"

  // 使用Source.fromFile打开文件
  val source = Source.fromFile(filePath)

  // 尝试读取文件内容，并捕获可能的异常
  try {
    // 获取文件的第一行（或前几个字符），这里读取前10个字符
    val firstChars = source.take(1000).mkString
    println(firstChars)
  } catch {
    case ex: Exception => println(s"Error reading file: ${ex.getMessage}")
  } finally {
    // 关闭资源以释放系统资源
    source.close()
  }
}
