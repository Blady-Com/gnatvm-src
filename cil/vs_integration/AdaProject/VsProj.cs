
/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
 * 
 * Modified by Martin C. Carlisle for Ada.
 * WARNING: IDEBuildLogger.cs (MessageHandler) has also been modified!!!
 * WARNING: IDEBuildLogger.cs (WarningHandler) has been modified!!

***************************************************************************/

using System;
using System.ComponentModel.Design;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.Win32;
using EnvDTE;
using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;

namespace AdaCore.AdaPackage
{
    [DefaultRegistryRoot(@"Software\Microsoft\VisualStudio\8.0Exp")]
    [ProvideProjectFactory(typeof(AdaProjectFactory), "GNAT for .NET (Ada)", "Ada Project Files (*.adaproj);*.adaproj", "adaproj", "adaproj", @"Templates\Projects")]
    [InstalledProductRegistration(true, "#100", "#102", "1.0", IconResourceID = 400)]
    [ProvideProjectItem(typeof(AdaProjectFactory), "My Items", @"Templates\ProjectItems", 500)]
    [ProvideObject(typeof(GeneralPropertyPage))]
    [Microsoft.VisualStudio.Shell.PackageRegistration(UseManagedResourcesOnly = true)]
    [ProvideLoadKey("standard", "1.0.0.0", "GNAT for .NET", "AdaCore", 104)]
    [Guid(GuidList.guidAdaPackagePkgString)]
    [ProvideService(typeof(AdaLanguageService), ServiceName = "Ada")]
    [ProvideLanguageService(typeof(AdaLanguageService), "Ada", 100,
    CodeSense = true,
    EnableFormatSelection = true,
    EnableCommenting = true,
    EnableLineNumbers = false,
    RequestStockColors = false,
    MatchBraces = true,
    MatchBracesAtCaret = true,
    ShowMatchingBrace = true,
    ShowCompletion = true)]
    [ProvideLanguageExtension(typeof(AdaLanguageService), ".adb")]
    [ProvideLanguageExtension(typeof(AdaLanguageService), ".ads")]
    public class AdaPackage : ProjectPackage, IVsInstalledProduct, IOleComponent
    {
        private uint m_componentID = 0;

        protected override void Initialize()
        {
            base.Initialize();

            // Proffer the service.
            IServiceContainer serviceContainer = this as IServiceContainer;
            AdaLanguageService langService = new AdaLanguageService();
            langService.SetSite(this);
            serviceContainer.AddService
                (typeof(AdaLanguageService), langService, true);

            // Now register the project factory
            this.RegisterProjectFactory(new AdaProjectFactory(this));

            // Register a timer to call our language service during
            // idle periods.
            IOleComponentManager mgr = GetService(typeof(SOleComponentManager))
                                       as IOleComponentManager;
            if (m_componentID == 0 && mgr != null)
            {
                OLECRINFO[] crinfo = new OLECRINFO[1];
                crinfo[0].cbSize = (uint)Marshal.SizeOf(typeof(OLECRINFO));
                crinfo[0].grfcrf = (uint)_OLECRF.olecrfNeedIdleTime |
                                              (uint)_OLECRF.olecrfNeedPeriodicIdleTime;
                crinfo[0].grfcadvf = (uint)_OLECADVF.olecadvfModal |
                                              (uint)_OLECADVF.olecadvfRedrawOff |
                                              (uint)_OLECADVF.olecadvfWarningsOff;
                crinfo[0].uIdleTimeInterval = 1000;
                int hr = mgr.FRegisterComponent(this, crinfo, out m_componentID);
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (m_componentID != 0)
            {
                IOleComponentManager mgr = GetService(typeof(SOleComponentManager))
                                           as IOleComponentManager;
                if (mgr != null)
                {
                    int hr = mgr.FRevokeComponent(m_componentID);
                }
                m_componentID = 0;
            }

            base.Dispose(disposing);
        }

        #region IOleComponent Members

        public int FDoIdle(uint grfidlef)
        {
            bool bPeriodic = (grfidlef & (uint)_OLEIDLEF.oleidlefPeriodic) != 0;
            // Use typeof(MyLanguageService) because we need to
            // reference the GUID for our language service.
            LanguageService service = GetService(typeof(AdaLanguageService))
                                      as LanguageService;
            if (service != null)
            {
                service.OnIdle(bPeriodic);
            }
            return 0;
        }

        public int FContinueMessageLoop(uint uReason,
                                        IntPtr pvLoopData,
                                        MSG[] pMsgPeeked)
        {
            return 1;
        }

        public int FPreTranslateMessage(MSG[] pMsg)
        {
            return 0;
        }

        public int FQueryTerminate(int fPromptUser)
        {
            return 1;
        }

        public int FReserved1(uint dwReserved,
                              uint message,
                              IntPtr wParam,
                              IntPtr lParam)
        {
            return 1;
        }

        public IntPtr HwndGetWindow(uint dwWhich, uint dwReserved)
        {
            return IntPtr.Zero;
        }

        public void OnActivationChange(IOleComponent pic,
                                       int fSameComponent,
                                       OLECRINFO[] pcrinfo,
                                       int fHostIsActivating,
                                       OLECHOSTINFO[] pchostinfo,
                                       uint dwReserved)
        {
        }

        public void OnAppActivate(int fActive, uint dwOtherThreadID)
        {
        }

        public void OnEnterState(uint uStateID, int fEnter)
        {
        }

        public void OnLoseActivation()
        {
        }

        public void Terminate()
        {
        }

        #endregion

        #region IVsInstalledProduct Members

        public int IdBmpSplash(out uint pIdBmp)
        {
            pIdBmp = 0;
            return VSConstants.S_OK;
        }

        public int IdIcoLogoForAboutbox(out uint pIdIco)
        {
            pIdIco = 400;
            return VSConstants.S_OK;
        }

        public int OfficialName(out string pbstrName)
        {
            pbstrName = "GNAT for .NET";
            return VSConstants.S_OK;
        }

        public int ProductDetails(out string pbstrProductDetails)
        {
            pbstrProductDetails = "This integrates the GNAT for .NET compiler (www.adacore.com) with " +
                "Visual Studio.";
            return VSConstants.S_OK;
        }

        public int ProductID(out string pbstrPID)
        {
            pbstrPID = "1.0";
            return VSConstants.S_OK;
        }

        #endregion
    }

    [GuidAttribute("100EBF5E-DC65-4100-8574-7E950E755C1C")]
    // {100EBF5E-DC65-4100-8574-7E950E755C1C}
    public class AdaProjectFactory : Microsoft.VisualStudio.Package.ProjectFactory
    {
        public AdaProjectFactory(AdaPackage package) : base (package)
        {
        }

        protected override Microsoft.VisualStudio.Package.ProjectNode CreateProject()
        {
            VsProj project = new VsProj(this.Package as AdaPackage);
            project.SetSite((IOleServiceProvider)((System.IServiceProvider)this.Package).GetService(typeof(IOleServiceProvider)));
            return project;
        }
    }

    internal enum OutputFileExtension
    {
        exe,
        dll
    }

    [Guid("D6B3ABA2-F990-4ed7-BDEC-3969E0E08D17")]
// {D6B3ABA2-F990-4ed7-BDEC-3969E0E08D17}
    public class VsProj : Microsoft.VisualStudio.Package.ProjectNode
    {
        private Package package;

        public VsProj(AdaPackage pkg)
        {
            this.package = pkg;
            this.SupportsProjectDesigner = true;
            InitializeCATIDs();
        }
        public override Guid ProjectGuid
        {
            get
            {
                return typeof(AdaProjectFactory).GUID;
            }
        }
        public override string ProjectType
        {
            get
            {
                return "GNAT for .NET";
            }
        }

        /// <summary>
        /// Overriding to provide project general property page
        /// </summary>
        /// <returns></returns>
        protected override Guid[] GetConfigurationIndependentPropertyPages()
        {
            Guid[] result = new Guid[1];
            result[0] = typeof(GeneralPropertyPage).GUID;
            return result;
        }

        /// <summary>
        /// Returns the configuration dependent property pages.
        /// Specify here a property page. By returning no property page the configuartion dependent properties will be neglected.
        /// Overriding, but current implementation does nothing
        /// To provide configuration specific page project property page, this should return an array bigger then 0
        /// (you can make it do the same as GetPropertyPageGuids() to see its impact)
        /// </summary>
        /// <param name="config"></param>
        /// <returns></returns>
        protected override Guid[] GetConfigurationDependentPropertyPages()
        {
            Guid[] result = new Guid[1];
            result[0] = typeof(AdaBuildPropertyPage).GUID;
            return result;
        }

        /// <summary>
        /// Overriding to provide customization of files on add files.
        /// This will replace tokens in the file with actual value (namespace, class name,...)
        /// </summary>
        /// <param name="source">Full path to template file</param>
        /// <param name="target">Full path to destination file</param>
        public override void AddFileFromTemplate(string source, string target)
        {
            if (!System.IO.File.Exists(source))
            {
                throw new FileNotFoundException(String.Format("Template file not found: {0}", source));
            }

            // The class name is based on the new file name
            string className = Path.GetFileNameWithoutExtension(target);
            string namespce = this.FileTemplateProcessor.GetFileNamespace(target, this);
            // clear the replace list
            this.FileTemplateProcessor = null;
            this.FileTemplateProcessor.AddReplace("%className%", className);
            this.FileTemplateProcessor.AddReplace("%namespace%", namespce);
            System.Reflection.Assembly reflect=System.Reflection.Assembly.GetExecutingAssembly();
            try
            {
                //tokenReplacer.UntokenMiscFile(source, target);
                this.FileTemplateProcessor.UntokenFile(source, target);
            }
            catch (Exception e)
            {
                throw new FileLoadException("Failed to add template file to project", target, e);
            }
        }

        /// <summary>
        /// Evaluates if a file is an Ada code file based on is extension
        /// </summary>
        /// <param name="strFileName">The filename to be evaluated</param>
        /// <returns>true if is a code file</returns>
        public override bool IsCodeFile(string strFileName)
        {
            // We do not want to assert here, just return silently.
            if (String.IsNullOrEmpty(strFileName))
            {
                return false;
            }
            return ((String.Compare(Path.GetExtension(strFileName), ".ads", StringComparison.OrdinalIgnoreCase) == 0) ||
                    (String.Compare(Path.GetExtension(strFileName), ".adb", StringComparison.OrdinalIgnoreCase) == 0));

        }

        /// <summary>
        /// Returns the outputfilename based on the output type
        /// </summary>
        public string OutputFileName
        {
            get
            {
                string assemblyName = this.ProjectMgr.GetProjectProperty(GeneralPropertyPageTag.AssemblyName.ToString(), true);

                string outputTypeAsString = this.ProjectMgr.GetProjectProperty(GeneralPropertyPageTag.OutputType.ToString(), false);
                OutputType outputType = (OutputType)Enum.Parse(typeof(OutputType), outputTypeAsString);

                return assemblyName + GetOuputExtension(outputType);
            }
        }
        #region static methods
        public static string GetOuputExtension(OutputType outputType)
        {
            if (outputType == OutputType.Library)
            {
                return "." + OutputFileExtension.dll.ToString();
            }
            else
            {
                return "." + OutputFileExtension.exe.ToString();
            }
        }
        #endregion

        /// <summary>
        /// Provide mapping from our browse objects and automation objects to our CATIDs
        /// </summary>
        private void InitializeCATIDs()
        {
            this.AddCATIDMapping(typeof(AdaProjectNodeProperties), typeof(AdaProjectNodeProperties).GUID);
            this.AddCATIDMapping(typeof(GeneralPropertyPage), typeof(AdaProjectNodeProperties).GUID);
        }

    }
}
