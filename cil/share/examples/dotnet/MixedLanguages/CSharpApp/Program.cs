using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpApp
{
    class Program
    {
        static int I = 0;

        static void Callback(int I)
        {
            Console.WriteLine(" "+I.ToString()+": C# callback");
        }

        static void Execute(adalib.cb_type Del)
        {
            System.Console.WriteLine("C# method calling a callback:");
            I = I + 1;
            Del(I);
        }
        
        static void Main(string[] args)
        {
            // Do not forget to first initialize the ada library
            ada_adalib_pkg.adainit();
            // Now call the local Execute method with a delegate pointing to an Ada subprogram
            Execute(new adalib.cb_type(adalib_pkg.a_callback));
            Execute(new adalib.cb_type(Callback));
            Execute(new adalib.cb_type(adalib_pkg.another_callback));
            adalib_pkg.execute(new adalib.cb_type(adalib_pkg.a_callback));
            adalib_pkg.execute(new adalib.cb_type(Callback));
            adalib_pkg.execute(new adalib.cb_type(adalib_pkg.another_callback));

            //adalibraryproject_pkg.execute((System.IntPtr)Callback);
            // At the end of execution, we finalize the ada library
            ada_adalib_pkg.adafinal();
        }


    }
}
