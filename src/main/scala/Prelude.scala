extension[A, B](a: A)
  infix def |>(f: A => B): B = f(a)

extension(map: Map[Int, String])
  infix def toMatrix: Map[Int, Map[Int, Char]]=
    map.map((x, line) => x -> line.toList.zipWithIndex.map((char, y) => y -> char).toMap)