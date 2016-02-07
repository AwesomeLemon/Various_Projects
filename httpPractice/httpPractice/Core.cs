/* 
 * Program for getting random film with desired rating and length via unofficial imdb.com api.
 * Unfortunately, there're too many empty pages and bad movie pages on imdb.com, 
 * so the idea to get good movie by randomly choosing id doesn't work.
 * 
 * by Alexander Chebykin
 */
using System;
using System.Threading.Tasks;
using System.Net.Http;
using System.Net.Http.Headers;
using Newtonsoft.Json;
using System.Collections.Generic;

namespace HttpPractice
{
    class Core
    {
        public Core()
        {
            _rand = new Random((int)DateTime.Now.Ticks);
            _client = new HttpClient();
            _client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        }
        private Random _rand;
        //private DateTime _time = DateTime.Now;
        private HttpClient _client;
        private int _triesCounter = 0;//just for fun (statistics IS fun)
        private const int _threadAmount = 10;
        private const int _imdbIdLength = 7;
        private const double _minRating = 7.0;
        private const int _minLength = 65;//no tv shows episodes allowed
        private const int _maxLength = 100;//and not too long

        async Task<bool> DownloadPageAsync(string id)
        {
            //  var id1 = "tt0110357";
            string page = "http://www.omdbapi.com/?i=" + id + "&y=&plot=short&r=json";
            using (HttpResponseMessage response = await _client.GetAsync(page))
            using (HttpContent content = response.Content)
            {
                string result   = await content.ReadAsStringAsync();
                dynamic resJson = JsonConvert.DeserializeObject(result);
                if (resJson.Response != "False")
                {
                    //string smth = resJson.Runtime;
                    int runtime = ToInt((string)resJson.Runtime);
                    string genre = (string)resJson.Genre;
                    if ((string)resJson.imdbRating == "N/A")
                    {
                        Console.WriteLine("Sry, movie is too unpopular");
                        return false;
                    }
                    double rating = resJson.imdbRating;// double.Parse(//toInt((string)resJson.imdbRating);
                    if (runtime > _minLength && runtime < _maxLength && rating > _minRating)
                    {
                        if (((string)resJson.Genre).Substring(0, 3) == "Doc")
                        {
                            Console.WriteLine("Documentary");
                            return false;
                        }
                        Console.WriteLine(resJson.Title + " " + rating);
                        Console.WriteLine(resJson.Plot);
                        return true;
                    }
                    Console.WriteLine("Sry, movie is long and/or bad");
                    return false;
                }
                Console.WriteLine("Oops, generated id doesn't represent a movie");
                return false;
            }
        }

        public bool TryMovie()
        {
            //var curTime = DateTime.Now;
            //if (curTime.Subtract(_time) > System.TimeSpan.FromMinutes(0.2))
            //{
            //    Console.WriteLine("Let's not make omdb.com work too hard... Sleeping for 40 seconds");
            //    System.Threading.Thread.Sleep(40000);
            //    Console.WriteLine("Sleep's over!");
            //    _time = DateTime.Now;
            //}

            List<Task<bool>> tasks = new List<Task<bool>>();
            for (int i = 0; i < _threadAmount; i++)
            {
                int tryId = _rand.Next();
                string idStr = tryId.ToString();
                var tryIdStr = idStr.Substring(0, Math.Min(_imdbIdLength, idStr.Length));
                while (tryIdStr.Length < _imdbIdLength)
                {
                    tryIdStr = "0" + tryIdStr;
                }
                tasks.Add(Task.Run(() => DownloadPageAsync("tt" + tryIdStr)));
            }
            Console.WriteLine("\nConsidering movies...");

            for (int i = 0; i < _threadAmount; i++)
            {
                try
                {
                    tasks[i].Wait();
                }
                catch (Exception e)
                {
                    Console.WriteLine("Okay, waiting");
                    System.Threading.Thread.Sleep(120000);
                    Console.WriteLine("Enough waiting!");

                    for (int j = i; j < 10; j++) tasks[j].Dispose();
                    return false;
                }

                bool res = tasks[i].Result;
                tasks[i].Dispose();
                if (res)
                {
                    if (Console.ReadLine() == "y") return true;
                }
            }
            _triesCounter++;
            return false;
        }

        static int ToInt(string str)
        {
            string temp = "";
            int i = 0;

            while (i <= str.Length && str[i] >= '0' && str[i] <= '9')
            {
                temp += str[i];
                i++;
            }
            if (temp == "") return -1;
            return int.Parse(temp);
        }
    }
}
