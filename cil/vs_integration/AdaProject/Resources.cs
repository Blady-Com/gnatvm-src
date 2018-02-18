﻿
/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

namespace AdaCore.AdaPackage
{
    using System;
    using System.Reflection;
    using System.Globalization;
    using System.Resources;
    using System.Text;
    using System.Threading;
    using System.ComponentModel;
    using System.Security.Permissions;

   [AttributeUsage(AttributeTargets.All)]
   internal sealed class SRDescriptionAttribute : DescriptionAttribute
   {

        private bool replaced;

        public SRDescriptionAttribute(string description) : base(description)
    	{
            replaced = false;
        }

        public override string Description
        {
            get
            {
                if (!replaced)
                {
                    replaced = true;
                    DescriptionValue = SR.GetString(base.Description);
                }
                return base.Description;
            }
        }
    }

    [AttributeUsage(AttributeTargets.All)]
    internal sealed class SRCategoryAttribute : CategoryAttribute
    {

        public SRCategoryAttribute(string category) : base(category)
        {
        }

        protected override string GetLocalizedString(string value)
        {
            return SR.GetString(value);
        }
    }
    internal sealed class SR
	{
        internal const string Application = "Application";
        internal const string GeneralCaption = "GeneralCaption";
        internal const string AssemblyName = "AssemblyName";
        internal const string AssemblyNameDescription = "AssemblyNameDescription";
        internal const string BuildCaption = "BuildCaption";
        internal const string OutputPath = "OutputPath";
        internal const string OutputPathDescription = "OutputPathDescription";
        internal const string BuildOptions = "BuildOptions";
        internal const string BuildOptionsDescription = "BuildOptionsDescription";
        internal const string CompilerOptions = "CompilerOptions";
        internal const string CompilerOptionsDescription = "CompilerOptionsDescription";
        internal const string BinderOptions = "BinderOptions";
        internal const string BinderOptionsDescription = "BinderOptionsDescription";
        internal const string LinkerOptions = "LinkerOptions";
        internal const string LinkerOptionsDescription = "LinkerOptionsDescription";
        internal const string OutputType = "OutputType";
        internal const string OutputTypeDescription = "OutputTypeDescription";
        internal const string DefaultNamespace = "DefaultNamespace";
        internal const string DefaultNamespaceDescription = "DefaultNamespaceDescription";
        internal const string StartupObject = "StartupObject";
        internal const string StartupObjectDescription = "StartupObjectDescription";
        internal const string Project = "Project";
        internal const string ProjectFile = "ProjectFile";
        internal const string ProjectFileDescription = "ProjectFileDescription";
        internal const string ProjectFolder = "ProjectFolder";
        internal const string ProjectFolderDescription = "ProjectFolderDescription";
        internal const string OutputFile = "OutputFile";
        internal const string OutputFileDescription = "OutputFileDescription";
        internal const string TargetPlatform = "TargetPlatform";
        internal const string TargetPlatformDescription = "TargetPlatformDescription";
        internal const string v2 = "v2";
        internal const string v2compact = "v2compact";

        static SR loader = null;
        ResourceManager resources;

        private static Object s_InternalSyncObject;
        private static Object InternalSyncObject
		{
            get
			{
                if (s_InternalSyncObject == null)
				{
                    Object o = new Object();
                    Interlocked.CompareExchange(ref s_InternalSyncObject, o, null);
                }
                return s_InternalSyncObject;
            }
        }
        
        internal SR()
		{
            resources = new System.Resources.ResourceManager("SRDescriptionAttribute", this.GetType().Assembly);
        }
        
        private static SR GetLoader()
		{
            if (loader == null)
			{
                lock (InternalSyncObject)
				{
                   if (loader == null)
				   {
                       loader = new SR();
                   }
               }
            }
            
            return loader;
        }

        private static CultureInfo Culture
		{
            get { return null/*use ResourceManager default, CultureInfo.CurrentUICulture*/; }
        }
        
        public static ResourceManager Resources
		{
            get
			{
                return GetLoader().resources;
            }
        }
        
        public static string GetString(string name, params object[] args)
		{
            SR sys = GetLoader();
            if (sys == null)
                return null;
            string res = sys.resources.GetString(name, SR.Culture);

            if (args != null && args.Length > 0)
			{
                return String.Format(CultureInfo.CurrentCulture, res, args);
            }
            else
			{
                return res;
            }
        }

        public static string GetString(string name)
		{
            SR sys = GetLoader();
            if (sys == null)
                return null;
            return sys.resources.GetString(name, SR.Culture);
        }
        
        public static object GetObject(string name)
		{
            SR sys = GetLoader();
            if (sys == null)
                return null;
            return sys.resources.GetObject(name, SR.Culture);
        }
}
}
