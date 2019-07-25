
object MyClass {
/* The program ‘uniquePrefix’ examines a list of words and 
returns the shortest unique prefix for each word. 
It finds the shortest unique prefix by firstly building a list 
of all possible prefixes and counting the number of times 
each prefix is used. Any prefix used more than once is removed, 
resulting in a list of legal prefixes only.

Secondly, the original list is sorted in ascending alphabetical sequence.

Lastly, adjacent word pairs in the sorted list are compared 
to find the shortest legal prefix. The resulting 
word – prefix mapping is the output of the program. 
The program ignores duplicate words in the input.
A zero-length input results in a zero-length output.  */

import scala.collection.mutable.Map

    def shortestInPair(s: String, t: String, ok: Seq[String]): String = {
        val shortestLen = if (s.length < t.length) s.length else t.length
        for (i <- 0 to shortestLen-1) {
            if (s.slice(i, i+1) != t.slice(i, i+1) ) {
                if (ok contains s.slice(0,i+1) ) {
                    return s.slice(0,i+1)
                }
            }
        }
        s.slice(0,shortestLen) 
    }
    
    def lastWord(s: String, ok: Seq[String]): String = {
        for (i <- 0 to s.length-2) {
            if ((ok contains s.slice(0,i+1) ) ) {
                return s.slice(0,i+1)
            } 
        }
        return s
    }    
    
    def createPossiblePrefixes(s: List[String]): List[String] = {
        val possPrefixes = for { i <- 0 to s.length-1
                          j <- 1 to s(i).length-1
                         } yield s(i).slice(0,j);
        
        var prefixCount = collection.mutable.Map[String, Int]()
        for (i <- 0 to possPrefixes.length-1) {
            if (prefixCount contains possPrefixes(i)) {
                prefixCount(possPrefixes(i)) += 1 
            } else {
                prefixCount += possPrefixes(i) -> 1
            }
        }
        // Create a list of prefixes thgat are only used once
        (for (i <- 0 to possPrefixes.length-1 if prefixCount(possPrefixes(i)) == 1) yield possPrefixes(i)  ).toList
    }

    // Find unique prefix
    def uniquePrefix(s: List[String]): Map[String, String] = {
		// generate possible prefixes
        val prefixOK = createPossiblePrefixes(s)

        // Create a Map to hold the results
        val res = scala.collection.mutable.Map[String, String]()
        
        // Sort the incoming words and compare adjacent words in the sorted list
        val sortedS = s.toArray.sortWith(_ < _)
        for (i <- 0 to sortedS.length-2) {
           res += (sortedS(i) -> shortestInPair(sortedS(i), sortedS(i+1), prefixOK) )
        }

        // Add the last word
        res + (sortedS(sortedS.length-1) -> lastWord(sortedS(sortedS.length-1), prefixOK) )

    }
    
      def main(args: Array[String]) {
         val names1 = List("skemper", "schacko", "eweitz", "kfisher", "kfellman", "kgeller")
         println(uniquePrefix(names1) )
         println(uniquePrefix( List("foxing", "foxy", "foxed", "fox", "jumps") ) )
         println(uniquePrefix( List("") ) )
        
      }
   }
