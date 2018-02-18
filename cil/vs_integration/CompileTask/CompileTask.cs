using System;
using System.Diagnostics;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;

namespace CompileTask
{
    public class CompileTask : Task
    {
        private string myProject;
        private string myMainFile;
        private string myWorkingDirectory;
        private string myConfiguration;
        private string myBuildOptions = "";
        private string myCompilerOptions = "";
        private string myBinderOptions = "";
        private string myLinkerOptions = "";
        private string myTargetPlatform = "";

        // The [Required] attribute indicates a required property.
        // If a project file invokes this task without passing a value
        // to this property, the build will fail immediately.
        [Required]
        public string Project
        {
            get
            {
                return myProject;
            }
            set
            {
                myProject = value;
            }
        }
        [Required]
        public string MainFile
        {
            get
            {
                return myMainFile;
            }
            set
            {
                myMainFile = value;
            }
        }
        [Required]
        public string WorkingDirectory
        {
            get
            {
                return myWorkingDirectory;
            }
            set
            {
                myWorkingDirectory = value;
            }
        }

        [Required]
        public string Configuration
        {
            get
            {
                return myConfiguration;
            }
            set
            {
                myConfiguration = value;
            }
        }

        public string BuildOptions
        {
            get { return myBuildOptions; }
            set { myBuildOptions = value; }
        }

        public string CompilerOptions
        {
            get { return myCompilerOptions; }
            set { myCompilerOptions = value; }
        }

        public string BinderOptions
        {
            get { return myBinderOptions; }
            set { myBinderOptions = value; }
        }

        public string LinkerOptions
        {
            get { return myLinkerOptions; }
            set { myLinkerOptions = value; }
        }

        public string TargetPlatform
        {
            get
            {
                return myTargetPlatform;
            }
            set
            {
                myTargetPlatform = value;
            }
        }

        private string[] sourceFiles;
        /// <summary>
        /// List of Python source files that should be compiled into the assembly
        /// </summary>
        [Required()]
        public string[] SourceFiles
        {
            get { return sourceFiles; }
            set { sourceFiles = value; }
        }

        private string outputAssembly;
        [Required()]
        public string OutputAssembly
        {
            get { return outputAssembly; }
            set { outputAssembly = value; }
        }
        
        private System.Diagnostics.Process process;
        private string[] split_array = { ":" };
        private void ParseMessage(string msg, out string filename, out int line, out int col)
        {
            string[] strings = msg.Split(split_array,StringSplitOptions.RemoveEmptyEntries);
            filename = strings[0];
            line = int.Parse(strings[1]);
            col = int.Parse(strings[2]);

            if (!System.IO.Path.IsPathRooted(filename))
            {
                for (int j = 0; j < SourceFiles.Length; j++)
                {
                    if (System.IO.Path.GetFileName(SourceFiles[j]).ToLower() == filename.ToLower())
                    {
                        filename = WorkingDirectory + '\\' + SourceFiles[j];
                        break;
                    }
                }
            }
        }

        void ReadOutput(string str)
        {
            string filename;
            int line, col;
            
            if (str.StartsWith("dotnet-gnat"))
            {
                Log.LogMessage(MessageImportance.High, str);
            }
            else if (str.Contains("warning: "))
            {
                try
                {
                    int loc = str.IndexOf(":");
                    string str2 = str.Substring(loc + 1);
                    int loc2 = str2.IndexOf(":");
                    string str3 = str2.Substring(loc2 + 1);
                    int loc3 = str3.IndexOf(":");
                    ParseMessage(str, out filename, out line, out col);
                    Log.LogWarning("", "", "", filename,
                        line, col - 1, line, col - 1, str.Substring(loc + loc2 + loc3 + 4));
                }
                catch
                {
                    Log.LogMessage(MessageImportance.High, str);
                }
            }
            else
            {
                try
                {
                    int loc = str.IndexOf(":");
                    string str2 = str.Substring(loc + 1);
                    int loc2 = str2.IndexOf(":");
                    string str3 = str2.Substring(loc2 + 1);
                    int loc3 = str3.IndexOf(":");
                    ParseMessage(str, out filename, out line, out col);
                    Log.LogError("", "", "", filename,
                        line, col - 1, line, col - 1, str.Substring(loc + loc2 + loc3 + 4));
                }
                catch
                {
                    Log.LogMessage(MessageImportance.High, str);
                }
            }
        }

        private void ReadStdOut(object sendingProcess, DataReceivedEventArgs outLine)
        {
           if (!String.IsNullOrEmpty(outLine.Data))
               ReadOutput(outLine.Data);
        }
        private void ReadStdErr(object sendingProcess, DataReceivedEventArgs outLine)
        {
           if (!String.IsNullOrEmpty(outLine.Data))
              ReadOutput(outLine.Data);
        }

        public override bool Execute()
        {
            string opts = "-x -P" + myProject + " " + myMainFile;
            if (myConfiguration != "")
            {
                if (myConfiguration == "Debug")
                    opts = "-g " + opts;
                opts = "-XCONFIGURATION=" + myConfiguration + " " + opts;
            }
            if (myTargetPlatform == "v2compact")
                opts += " --RTS=compact";

            if (myBuildOptions != "")
                opts += " " + this.myBuildOptions;

            if (myCompilerOptions != "")
                opts += " -cargs " + myCompilerOptions;

            if (myBinderOptions != "")
                opts += " -bargs " + myBinderOptions;

            if (myLinkerOptions != "")
                opts += " -largs " + myBinderOptions;

            // Log a high-importance comment

            process = new System.Diagnostics.Process();
            process.StartInfo.UseShellExecute = false;
            process.StartInfo.RedirectStandardOutput = true;
            process.StartInfo.RedirectStandardError = true;
            process.StartInfo.CreateNoWindow = true;
            process.StartInfo.FileName = "dotnet-gnatmake.exe";
            process.StartInfo.Arguments = opts;
            process.StartInfo.WorkingDirectory = myWorkingDirectory;

            process.OutputDataReceived += new DataReceivedEventHandler(ReadStdOut);
            process.ErrorDataReceived += new DataReceivedEventHandler(ReadStdErr);

            Log.LogMessage (MessageImportance.High, process.StartInfo.FileName + " " + opts);
            
            process.Start();
            process.BeginOutputReadLine();
            process.BeginErrorReadLine();

            process.WaitForExit();
            process.Close();

            return !Log.HasLoggedErrors;
        }
    }
}

