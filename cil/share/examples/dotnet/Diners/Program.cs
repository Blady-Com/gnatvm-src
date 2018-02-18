using System;
using System.Collections.Generic;
using System.Windows.Forms;

namespace Diners
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [MTAThread]
        static void Main()
        {
            ada_adalib_pkg.adainit();
            ada_adalib_pkg.adafinal();
        }
    }
}
