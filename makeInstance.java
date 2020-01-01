import java.io.*;

class makeInstance
{
   public static void main(String args[]) throws IOException 
   {
    String suits[] = {"Clubs", "Diamonds", "Hearts", "Spades"};
    String ranks[] = {"Ace", "2", "3", "4", "5", "6", "7", "8", "9",
	              "10", "Jack", "Queen", "King"};
 
    PrintWriter fileOut = new PrintWriter("makeInstance.txt");

    for (String x : suits)
       {
          String suit = new String(x);
          String suitArg = suit.substring(0,1);
	  for (String  y : ranks)
	  {
            String rank = new String(y);
            StringBuilder rankArg = new StringBuilder();
	    if (y.equals("Ace"))
		    rankArg.append("A");
	    if (y.equals("Jack"))
		    rankArg.append("J");
	    if (y.equals("Queen"))
		    rankArg.append("Q");
	    if (y.equals("King"))
		    rankArg.append("K");
	    if (rankArg.length() == 0)
		    rankArg.append(y);

	    fileOut.print("(setq ");
	    fileOut.print(rankArg);
	    fileOut.print(suitArg);
	    fileOut.print(" ");
	  //To make an actual card, we would use something                               //like (setf jc (make-instance 'card :rank 'jack :suit 'clubs))
	     fileOut.print("(make-instance 'card :rank '");
	     fileOut.print(rank + " :suit ");
	     fileOut.print("'" + suit + "))\n");
	  }
       }
       fileOut.close();    
   }
}
