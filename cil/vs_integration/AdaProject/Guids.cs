// Guids.cs
// MUST match guids.h
using System;

namespace AdaCore.AdaPackage
{
    static class GuidList
    {
        public const string guidAdaPackagePkgString = "c9ecf690-c138-42ce-a401-47a5e063cd75";
        public const string guidAdaProjectPkgString = "D3DF18F6-57E7-47EE-A15D-EB44867AC769";

        public static readonly Guid guidAdaPackagePkg = new Guid(guidAdaPackagePkgString);
        public static readonly Guid guidAdaProjectPkg = new Guid(guidAdaProjectPkgString);
    };
}
