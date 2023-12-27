extension[A, B](a: A)
  infix def |>(f: A => B): B = f(a)
  
extension[A](a: A)
  infix def <|>(f: A => Unit): A =
    f(a)
    a

extension(map: Map[Int, String])
  infix def toMatrix: Map[Int, Map[Int, Char]]=
    map.map((x, line) => x -> line.toList.zipWithIndex.map((char, y) => y -> char).toMap)
    
extension(number: Int)
  def between(left: Int, right: Int): Boolean = number >= left && number <= right
  
extension (n: BigInt)
  def between(l: BigInt, r: BigInt): Boolean = n >= l && n <= r
  
extension[A](list: List[A])
  def safeTail: List[A] = if list.isEmpty then List() else list.tail