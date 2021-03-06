/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/
using System;
using System.Globalization;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Package;
using VSLangProj;

namespace Microsoft.VisualStudio.Package.Automation
{
    [ComVisible(true)]
	public class OAComReference : OAReferenceBase<ComReferenceNode>
	{
		public OAComReference(ComReferenceNode comReference) :
			base(comReference)
		{
		}

		#region Reference override
		public override string Culture
		{
			get
			{
				int locale = 0;
				try
				{
					locale = int.Parse(BaseReferenceNode.LCID);
				}
				catch (System.FormatException)
				{
					// Do Nothing
				}
				if (0 == locale)
				{
					return string.Empty;
				}
				CultureInfo culture = new CultureInfo(locale);
				return culture.Name;
			}
		}
		public override string Identity
		{
			get
			{
				return string.Format(CultureInfo.InvariantCulture, "{0}\\{1}", BaseReferenceNode.TypeGuid.ToString("B"), this.Version);
			}
		}
		public override int MajorVersion
		{
			get { return BaseReferenceNode.MajorVersionNumber; }
		}
		public override int MinorVersion
		{
			get { return BaseReferenceNode.MinorVersionNumber; }
		}
		public override string Name
		{
			get { return BaseReferenceNode.Caption; }
		}
		public override string Path
		{
			get { return BaseReferenceNode.InstalledFilePath; }
		}
		public override VSLangProj.prjReferenceType Type
		{
			get
			{
				return VSLangProj.prjReferenceType.prjReferenceTypeActiveX;
			}
		}
		public override string Version
		{
			get
			{
				Version version = new Version(BaseReferenceNode.MajorVersionNumber, BaseReferenceNode.MinorVersionNumber);
				return version.ToString();
			}
		}
		#endregion
	}
}
