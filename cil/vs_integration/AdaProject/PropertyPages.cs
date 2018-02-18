
/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Package;
using System.Runtime.InteropServices;
using System.ComponentModel;
using System.Diagnostics;
using System.Globalization;
using System.IO;

namespace AdaCore.AdaPackage
{
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Property | AttributeTargets.Field, Inherited = false, AllowMultiple = false)]
    internal sealed class LocDisplayNameAttribute : DisplayNameAttribute
    {
        private string name;

        public LocDisplayNameAttribute(string name)
        {
            this.name = name;
        }

        public override string DisplayName
        {
            get
            {
                string result = SR.GetString(this.name);

                if (result == null)
                {
                    Debug.Assert(false, "String resource '" + this.name + "' is missing");
                    result = this.name;
                }

                return result;
            }
        }
    }

    internal enum GeneralPropertyPageTag
    {
        AssemblyName,
        OutputType,
        RootNamespace,
        StartupObject,
        TargetPlatform
    }

    public class MyPlatformTypeConverter : EnumConverter
    {

        public MyPlatformTypeConverter()
            : base(typeof(MyPlatformType))
        {
        }

        public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
        {
            if (sourceType == typeof(string)) return true;

            return base.CanConvertFrom(context, sourceType);
        }

        public override object ConvertFrom(ITypeDescriptorContext context, CultureInfo culture, object value)
        {
            string str = value as string;

            if (str != null)
            {
                if (str == SR.GetString(SR.v2, culture)) return MyPlatformType.v2;

                if (str == SR.GetString(SR.v2compact, culture)) return MyPlatformType.v2compact;
            }

            return base.ConvertFrom(context, culture, value);
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType == typeof(string))
            {
                string result = null;
                // In some cases if multiple nodes are selected the windows form engine
                // calls us with a null value if the selected node's property values are not equal
                if (value != null)
                {
                    result = SR.GetString(((MyPlatformType)value).ToString(), culture);
                }
                else
                {
                    result = SR.GetString(MyPlatformType.v2.ToString(), culture);
                }

                if (result != null) return result;
            }

            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetStandardValuesSupported(System.ComponentModel.ITypeDescriptorContext context)
        {
            return true;
        }

        public override System.ComponentModel.TypeConverter.StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(new MyPlatformType[] { MyPlatformType.v2, MyPlatformType.v2compact });
        }
    }

    [Microsoft.VisualStudio.Package.PropertyPageTypeConverterAttribute(typeof(MyPlatformTypeConverter))]
    public enum MyPlatformType { v2, v2compact }

    /// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage"]/*' />
    [ComVisible(true), Guid("3c693da2-5bca-49b3-bd95-ffe0a39dd723")]
    public class GeneralPropertyPage : SettingsPage, EnvDTE80.IInternalExtenderProvider
    {
        #region fields
        private string assemblyName;
        private OutputType outputType;
        private string startupObject;
        private MyPlatformType targetPlatform = MyPlatformType.v2;
        #endregion

        /// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage.GeneralPropertyPage"]/*' />
        public GeneralPropertyPage()
        {
            this.Name = SR.GetString(SR.GeneralCaption);
        }

        #region overriden methods
        /// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage.GetClassName"]/*' />
        public override string GetClassName()
        {
            return this.GetType().FullName;
        }

        /// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage.BindProperties"]/*' />
        protected override void BindProperties()
        {
            if (this.ProjectMgr == null)
            {
                Debug.Assert(false);
                return;
            }

            this.assemblyName = this.ProjectMgr.GetProjectProperty(GeneralPropertyPageTag.AssemblyName.ToString(), true);

            string outputType = this.ProjectMgr.GetProjectProperty(GeneralPropertyPageTag.OutputType.ToString(), false);

            if (outputType != null && outputType.Length > 0)
            {
                try
                {
                    this.outputType = (OutputType)Enum.Parse(typeof(OutputType), outputType);
                }
                catch
                { } //Should only fail if project file is corrupt
            }

            this.startupObject = this.ProjectMgr.GetProjectProperty(GeneralPropertyPageTag.StartupObject.ToString(), false);

            string targetPlatform = this.ProjectMgr.GetProjectProperty(GeneralPropertyPageTag.TargetPlatform.ToString(), false);

            if (targetPlatform != null && targetPlatform.Length > 0)
            {
                try
                {
                    this.targetPlatform = (MyPlatformType)Enum.Parse(typeof(MyPlatformType), targetPlatform);
                }
                catch
                { }
            }
        }

        /// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage.ApplyChanges"]/*' />
        protected override int ApplyChanges()
        {
            if (this.ProjectMgr == null)
            {
                Debug.Assert(false);
                return VSConstants.E_INVALIDARG;
            }

            this.ProjectMgr.SetProjectProperty(GeneralPropertyPageTag.AssemblyName.ToString(), this.assemblyName);
            this.ProjectMgr.SetProjectProperty(GeneralPropertyPageTag.OutputType.ToString(), this.outputType.ToString());
            this.ProjectMgr.SetProjectProperty(GeneralPropertyPageTag.StartupObject.ToString(), this.startupObject);
            this.ProjectMgr.SetProjectProperty(GeneralPropertyPageTag.TargetPlatform.ToString(), this.targetPlatform.ToString());
            this.IsDirty = false;

            return VSConstants.S_OK;
        }
        #endregion

        #region exposed properties
        /// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage.AssemblyName"]/*' />
        [SRCategoryAttribute(SR.Application)]
        [LocDisplayName(SR.AssemblyName)]
        [SRDescriptionAttribute(SR.AssemblyNameDescription)]
        public string AssemblyName
        {
            get { return this.assemblyName; }
            set { this.assemblyName = value; this.IsDirty = true; }
        }

        /// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage.OutputType"]/*' />
        [SRCategoryAttribute(SR.Application)]
        [LocDisplayName(SR.OutputType)]
        [SRDescriptionAttribute(SR.OutputTypeDescription)]
        public OutputType OutputType
        {
            get { return this.outputType; }
            set { this.outputType = value; this.IsDirty = true; }
        }

        /// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage.StartupObject"]/*' />
        [SRCategoryAttribute(SR.Application)]
        [LocDisplayName(SR.StartupObject)]
        [SRDescriptionAttribute(SR.StartupObjectDescription)]
        public string StartupObject
        {
            get { return this.startupObject; }
            set { this.startupObject = value; this.IsDirty = true; }
        }

        /// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage.ProjectFile"]/*' />
        [SRCategoryAttribute(SR.Project)]
        [LocDisplayName(SR.ProjectFile)]
        [SRDescriptionAttribute(SR.ProjectFileDescription)]
        [AutomationBrowsable(false)]
        public string ProjectFile
        {
            get { return Path.GetFileName(this.ProjectMgr.ProjectFile); }
        }

        /// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage.ProjectFolder"]/*' />
        [SRCategoryAttribute(SR.Project)]
        [LocDisplayName(SR.ProjectFolder)]
        [SRDescriptionAttribute(SR.ProjectFolderDescription)]
        [AutomationBrowsable(false)]
        public string ProjectFolder
        {
            get { return Path.GetDirectoryName(this.ProjectMgr.ProjectFolder); }
        }

        /// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage.OutputFile"]/*' />
        [SRCategoryAttribute(SR.Project)]
        [LocDisplayName(SR.OutputFile)]
        [SRDescriptionAttribute(SR.OutputFileDescription)]
        [AutomationBrowsable(false)]
        public string OutputFile
        {
            get { return this.assemblyName + VsProj.GetOuputExtension(this.outputType); }
        }

        /// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage.TargetPlatform"]/*' />
        [SRCategoryAttribute(SR.Project)]
        [LocDisplayName(SR.TargetPlatform)]
        [SRDescriptionAttribute(SR.TargetPlatformDescription)]
        [AutomationBrowsable(false)]
        public MyPlatformType TargetPlatform
        {
            get { return this.targetPlatform; }
            set { this.targetPlatform = value; IsDirty = true; }
        }
        #endregion

        #region IInternalExtenderProvider Members

        bool EnvDTE80.IInternalExtenderProvider.CanExtend(string extenderCATID, string extenderName, object extendeeObject)
        {
            IVsHierarchy outerHierarchy = HierarchyNode.GetOuterHierarchy(this.ProjectMgr);
            if (outerHierarchy is EnvDTE80.IInternalExtenderProvider)
                return ((EnvDTE80.IInternalExtenderProvider)outerHierarchy).CanExtend(extenderCATID, extenderName, extendeeObject);
            return false;
        }

        object EnvDTE80.IInternalExtenderProvider.GetExtender(string extenderCATID, string extenderName, object extendeeObject, EnvDTE.IExtenderSite extenderSite, int cookie)
        {
            IVsHierarchy outerHierarchy = HierarchyNode.GetOuterHierarchy(this.ProjectMgr);
            if (outerHierarchy is EnvDTE80.IInternalExtenderProvider)
                return ((EnvDTE80.IInternalExtenderProvider)outerHierarchy).GetExtender(extenderCATID, extenderName, extendeeObject, extenderSite, cookie);
            return null;
        }

        object EnvDTE80.IInternalExtenderProvider.GetExtenderNames(string extenderCATID, object extendeeObject)
        {
            IVsHierarchy outerHierarchy = HierarchyNode.GetOuterHierarchy(this.ProjectMgr);
            if (outerHierarchy is EnvDTE80.IInternalExtenderProvider)
                return ((EnvDTE80.IInternalExtenderProvider)outerHierarchy).GetExtenderNames(extenderCATID, extendeeObject);
            return null;
        }

        #endregion

        #region ExtenderSupport
        [Browsable(false)]
        [AutomationBrowsable(false)]
        public virtual string ExtenderCATID
        {
            get
            {
                Guid catid = this.ProjectMgr.ProjectMgr.GetCATIDForType(this.GetType());
                if (Guid.Empty.CompareTo(catid) == 0)
                    throw new NotImplementedException();
                return catid.ToString("B");
            }
        }
        [Browsable(false)]
        [AutomationBrowsable(false)]
        public object ExtenderNames
        {
            get
            {
                EnvDTE.ObjectExtenders extenderService = (EnvDTE.ObjectExtenders)this.ProjectMgr.GetService(typeof(EnvDTE.ObjectExtenders));
                return extenderService.GetExtenderNames(this.ExtenderCATID, this);
            }
        }
        public object get_Extender(string extenderName)
        {
            EnvDTE.ObjectExtenders extenderService = (EnvDTE.ObjectExtenders)this.ProjectMgr.GetService(typeof(EnvDTE.ObjectExtenders));
            return extenderService.GetExtender(this.ExtenderCATID, extenderName, this);
        }
        #endregion

    }

    internal enum AdaBuildPropertyPageTag
    {
        BuildOptions,
        CompilerOptions,
        BinderOptions,
        LinkerOptions
    }
    
    [ComVisible(true), Guid("3c693da3-5bca-49b3-bd95-ffe0a39dd723")]
    public class AdaBuildPropertyPage : BuildPropertyPage
    {
        private string outputPath;
        private string buildOptions;
        private string compilerOptions;
        private string binderOptions;
        private string linkerOptions;

        #region ctors
        public AdaBuildPropertyPage()
            : base()
        {
        }
        #endregion

        #region properties
        [SRCategoryAttribute(SR.BuildCaption)]
        [LocDisplayName(SR.OutputPath)]
        [SRDescriptionAttribute(SR.OutputPathDescription)]
        [AutomationBrowsable(false)]
        [Browsable(false)]
        public new string OutputPath
        {
            get { return this.outputPath; }
            //set { this.outputPath = value; this.IsDirty = true; }
        }

        [SRCategoryAttribute(SR.BuildCaption)]
        [LocDisplayName(SR.BuildOptions)]
        [SRDescriptionAttribute(SR.BuildOptionsDescription)]
        public string BuildOptions
        {
            get { return this.buildOptions; }
            set { this.buildOptions = value; this.IsDirty = true; }
        }
        [SRCategoryAttribute(SR.BuildCaption)]
        [LocDisplayName(SR.CompilerOptions)]
        [SRDescriptionAttribute(SR.CompilerOptionsDescription)]
        public string CompilerOptions
        {
            get { return this.compilerOptions; }
            set { this.compilerOptions = value; this.IsDirty = true; }
        }
        [SRCategoryAttribute(SR.BuildCaption)]
        [LocDisplayName(SR.BinderOptions)]
        [SRDescriptionAttribute(SR.BinderOptionsDescription)]
        public string BinderOptions
        {
            get { return this.binderOptions; }
            set { this.binderOptions = value; this.IsDirty = true; }
        }
        [SRCategoryAttribute(SR.BuildCaption)]
        [LocDisplayName(SR.LinkerOptions)]
        [SRDescriptionAttribute(SR.LinkerOptionsDescription)]
        public string LinkerOptions
        {
            get { return this.linkerOptions; }
            set { this.linkerOptions = value; this.IsDirty = true; }
        }
        #endregion

        #region Overriden methods
        protected override void BindProperties()
        {
            if (this.ProjectMgr == null)
            {
                Debug.Assert(false);
                return;
            }
            base.BindProperties();
            this.outputPath = base.OutputPath;
            this.buildOptions = this.GetConfigProperty(AdaBuildPropertyPageTag.BuildOptions.ToString());
            this.compilerOptions = this.GetConfigProperty(AdaBuildPropertyPageTag.CompilerOptions.ToString());
            this.binderOptions = this.GetConfigProperty(AdaBuildPropertyPageTag.BinderOptions.ToString());
            this.linkerOptions = this.GetConfigProperty(AdaBuildPropertyPageTag.LinkerOptions.ToString());
        }

        protected override int ApplyChanges()
        {
            if (this.ProjectMgr == null)
            {
                Debug.Assert(false);
                return VSConstants.E_INVALIDARG;
            }
            this.SetConfigProperty(AdaBuildPropertyPageTag.BuildOptions.ToString(), this.buildOptions);
            this.SetConfigProperty(AdaBuildPropertyPageTag.CompilerOptions.ToString(), this.compilerOptions);
            this.SetConfigProperty(AdaBuildPropertyPageTag.BinderOptions.ToString(), this.binderOptions);
            this.SetConfigProperty(AdaBuildPropertyPageTag.LinkerOptions.ToString(), this.linkerOptions);
            this.IsDirty = false;
            return VSConstants.S_OK;
        }
        #endregion
    }

    [ComVisible(true), CLSCompliant(false), System.Runtime.InteropServices.ClassInterface(ClassInterfaceType.AutoDual)]
    [Guid("3c693da4-5bca-49b3-bd95-ffe0a39dd723")]
    public class AdaProjectNodeProperties : ProjectNodeProperties
    {
        #region ctors
        public AdaProjectNodeProperties(ProjectNode node)
            : base(node)
        {
        }
        #endregion

        #region properties
        [Browsable(false)]
        public string OutputFileName
        {
            get
            {
                return ((VsProj)(this.Node.ProjectMgr)).OutputFileName;
            }
        }
        /// <summary>
        /// Returns/Sets the MainFile project property
        /// </summary>
        [Browsable(false)]
        public string MainFile
        {
            get
            {
                return this.Node.ProjectMgr.GetProjectProperty("MainFile", true);
            }
        }

        [Browsable(false)]
        public string AssemblyName
        {
            get
            {
                return this.Node.ProjectMgr.GetProjectProperty(ProjectFileConstants.AssemblyName);
            }
            /*
            set
            {
                this.Node.ProjectMgr.SetProjectProperty(ProjectFileConstants.AssemblyName, value);
            }
             */
        }

        [Browsable(false)]
        public string OutputType
        {
            get
            {
                return this.Node.ProjectMgr.GetProjectProperty(ProjectFileConstants.OutputType);
            }
        }

        #endregion
    }

}
