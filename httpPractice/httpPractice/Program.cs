/* 
 * Program for getting random film with desired rating and length via unofficial imdb.com api.
 * Unfortunately, there're too many empty pages and bad movie pages on imdb.com, 
 * so the idea to get good movie by randomly choosing id doesn't work.
 * 
 * by Alexander Chebykin
 */
namespace HttpPractice
{
    class Program
    {
        static void Main()
        {
            var core = new HttpPractice.Core();
            while (!core.TryMovie());
        }
    }
}
