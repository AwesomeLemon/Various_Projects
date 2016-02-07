/*
 * A little program I wrote when I had no Internet for a couple of hours, but wanted to practice
 * my blind typing skills, which I was improving at the moment.
 * 
 * by Alexander Chebykin.
 */
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace KeyboardTrainer
{
    class Program
    {
        static string GetLine (char[] letters, int minInd, int maxInd)
        {
            var rand = new Random();
            const int wordCount = 5;
            const int wordMaxLength = 6;
            const int upperCaseThreshold = 6;
            string res = "";
            char curLetter;

            for (int i = 0; i < wordCount; i++ )
            {
                int wordLength = rand.Next(wordMaxLength) + 1;
                //Letter in upper case can be only at the beggining
                curLetter = letters[rand.Next(minInd, maxInd + 1)];
                while (!(Char.IsLetter(curLetter))) curLetter = letters[rand.Next(minInd, maxInd + 1)];
                int upperCaseChanse = rand.Next(10);
                if (upperCaseChanse > upperCaseThreshold) curLetter = Char.ToUpper(curLetter);
                res += curLetter.ToString();

                for (int j = 1; j < wordLength - 1; j++)
                {
                    curLetter = letters[rand.Next(minInd, maxInd + 1)];
                    while (!(Char.IsLetterOrDigit(curLetter))) curLetter = letters[rand.Next(minInd, maxInd + 1)];
                    res += curLetter.ToString();
                }
                //Punctuation mark can be only at the end
                curLetter = letters[rand.Next(minInd, maxInd + 1)];
                res += curLetter.ToString();

                res += ' '.ToString();
            }
            return res;
        }

        static int CheckSpell(string sample)
        {
            int i = 0;
            int mistakeNum = 0;

            while (i < sample.Length)
            {
                char cur = Console.ReadKey(true).KeyChar;
                if (cur == sample[i]) Console.Write(cur);
                else
                {
                    i--;
                    mistakeNum++;
                    char wrong = Char.ToUpper(cur);
                    Console.Write(wrong);
                    Console.CursorLeft--;
                }
                i++;
            }
            return mistakeNum;
        }
        static void Main(string[] args)
        {
            const int averageWordLen = 5;
            const int levelNum = 4;
            char[] letters = {'a', 's', 'd', 'f', 'j', 'k', 'l', ';', /**/'w', 'e', 'r', 'u', 'i', 'o', /**/ 'c', 'v', 'n', 'm', ',', '.', /**/ 'g', 'h', '"'};
            int[] levels = {7, 13, 19, 22};
            int wpm = 0;
            int mistakes = 0;
            int symbolNum = 0;

            var start = System.DateTime.Now;
            Console.WriteLine("Choose difficulty level from 0 to " + (levelNum - 1).ToString());
            int level = levels[Int32.Parse(Console.ReadLine()) % levelNum];
            while (true)
            {
                Console.Clear();
                Console.WriteLine("Words per minute: " + wpm.ToString());
                Console.WriteLine("Mistakes: " + mistakes.ToString());
                Console.WriteLine("Symbols number: " + symbolNum.ToString());

                string line = GetLine(letters, 0, level);
                Console.WriteLine(line);
                mistakes += CheckSpell(line);

                symbolNum += line.Length;
                int wordNum = symbolNum / averageWordLen;

                var currentTime = System.DateTime.Now;
                var time = (currentTime - start).TotalSeconds;

                wpm = (int) (wordNum / time * 60);
            }
        }
    }
}
